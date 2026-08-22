% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__discontinuity_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: correct_latin_kernel__discontinuity_reading
 *   human_readable: Correct Latin Kernel — Discontinuity Reading (Classical/Medieval as Distinct Systems)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This story instantiates the discontinuity reading of the
 *   correct_latin_kernel: the claim that Classical Latin and Medieval Latin
 *   are structurally distinct linguistic systems, such that reconstructing
 *   'proper' Latin required symbolic reoccupation of lost classical structure
 *   from surviving texts rather than merely codifying an evolved
 *   continuation. This reading treats Medieval forms as corruptions of a
 *   prior, purer system — a framing with clear origin in Renaissance humanist
 *   polemic against medieval usage, later absorbed into classical philology's
 *   disciplinary self-understanding. The ε value here (0.58) is authored for
 *   THIS reading's own standing arrangement — the discontinuity framework as
 *   it actually organizes prestige, curricula, and canon in classical
 *   philology — not for any rival reading's alternative. Sibling constraints
 *   (continuity_reading, hybrid_reading) author their own, independently
 *   derived ε values for their own standing arrangements; they are not
 *   consulted here and this story does not average against them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, 0.58).
domain_priors:suppression_score(correct_latin_kernel__discontinuity_reading, 0.52).
domain_priors:theater_ratio(correct_latin_kernel__discontinuity_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__discontinuity_reading, "Correct Latin Kernel — Discontinuity Reading (Classical/Medieval as Distinct Systems)").
narrative_ontology:topic_domain(correct_latin_kernel__discontinuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__discontinuity_reading, '2965788b-d6be-471f-850f-c0efe8770402').
narrative_ontology:cs_kernel_codification('2965788b-d6be-471f-850f-c0efe8770402', distributed).
narrative_ontology:cs_authority_grounding('2965788b-d6be-471f-850f-c0efe8770402', lineage).
narrative_ontology:cs_interpretation_layer_present('2965788b-d6be-471f-850f-c0efe8770402').
narrative_ontology:cs_reading_relation('2965788b-d6be-471f-850f-c0efe8770402', correct_latin_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('2965788b-d6be-471f-850f-c0efe8770402', correct_latin_kernel__hybrid_reading, influences).
narrative_ontology:cs_axiom('2965788b-d6be-471f-850f-c0efe8770402', foundational, medieval_latin_constitutes_a_distinct_system).
narrative_ontology:cs_axiom_status(medieval_latin_constitutes_a_distinct_system, holdable).
narrative_ontology:cs_axiom_grounding('2965788b-d6be-471f-850f-c0efe8770402', medieval_latin_constitutes_a_distinct_system, empirically_contingent).
narrative_ontology:cs_axiom('2965788b-d6be-471f-850f-c0efe8770402', foundational, reconstruction_from_texts_is_recovery_not_correction).
narrative_ontology:cs_axiom_status(reconstruction_from_texts_is_recovery_not_correction, holdable).
narrative_ontology:cs_axiom_grounding('2965788b-d6be-471f-850f-c0efe8770402', reconstruction_from_texts_is_recovery_not_correction, conventional).
narrative_ontology:cs_reference_frame('2965788b-d6be-471f-850f-c0efe8770402', ciceronian_classical_norm).
narrative_ontology:cs_drift_state('2965788b-d6be-471f-850f-c0efe8770402', modern_comparative_linguistics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2965788b-d6be-471f-850f-c0efe8770402', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__discontinuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, classical_philology_establishment).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, renaissance_humanist_tradition).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, textual_critics_of_reconstructed_classical_norm).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, medieval_latin_scholars).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, medieval_scribal_tradition_reputation).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, students_of_living_medieval_documents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets curricular and editorial standards for what counts as correct Latin, using reconstructed Classical forms (recovered symbolically from manuscript comparison and inscriptional evidence) as the authoritative baseline. Trains philologists, edits critical editions, and adjudicates which forms are 'genuine' versus 'corrupt.' Its institutional prestige and pedagogical authority depend on the discontinuity frame being accepted as fact rather than interpretive choice.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, classical_philology_establishment, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__discontinuity_reading, classical_philology_establishment, beneficiary).

% Historically originated the discontinuity framing to justify a return to Ciceronian style against medieval usage, treating the intervening centuries as decline. Its legacy is vindicated wholesale if Medieval Latin is a corrupted system requiring symbolic reoccupation to fix, rather than a legitimate evolutionary stage of the same language.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, renaissance_humanist_tradition, beneficiary,
    institutional, civilizational, analytical, continental).

% Study a body of texts routinely described in the discontinuity frame as departures, errors, or degradations relative to a reconstructed classical norm. Their object of study is treated as evidence of loss rather than as a coherent linguistic system on its own terms, which affects funding priority, canon placement, and disciplinary prestige relative to classicists.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, medieval_latin_scholars, payer,
    moderate, biographical, constrained, continental).

% The historical reputation of medieval scribes and writers themselves — not a living actor, but a standing characterization retrospectively imposed. Under the discontinuity reading, centuries of Latin usage are cast as systemic corruption rather than living linguistic practice, a characterization that cannot be contested by the parties it describes.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, medieval_scribal_tradition_reputation, payer,
    powerless, civilizational, trapped, continental).
narrative_ontology:stakeholder_non_agent(correct_latin_kernel__discontinuity_reading, medieval_scribal_tradition_reputation).

% Students and researchers who need Medieval Latin as a working tool to read charters, chronicles, and scholastic texts on their own terms. They inherit a pedagogical apparatus that measures their target language against a reconstructed classical yardstick, making functional medieval usage look like a set of mistakes to unlearn rather than a system to master.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, students_of_living_medieval_documents, payer,
    powerless, biographical, constrained, national).

% Apply general theories of language change to assess whether Classical-to-Medieval Latin more closely resembles ordinary diachronic drift (continuity) or a genuine systemic break requiring external reconstruction (discontinuity). They examine morphological, syntactic, and lexical continuity data independent of either camp's institutional stakes.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, comparative_historical_linguists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__discontinuity_reading, classical_philology_establishment).
narrative_ontology:fixing_cost_class(correct_latin_kernel__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, teachable reference standard for 'correct' Latin by anchoring instruction and textual criticism to a reconstructed Classical norm, which solves the real problem of variable, geographically and temporally dispersed medieval usage lacking a single agreed-upon grammar.
% TRANSFER_FUNCTION: Moves scholarly prestige, curricular authority, and canon-formation power from those who could describe Medieval Latin as a legitimate system in its own right toward those whose expertise consists in policing conformity to the reconstructed Classical standard; symbolic capital flows from medievalists to classicists.
% ABSENT_VOICES: Medieval writers and scribes themselves have no voice in how their own linguistic practice is retrospectively characterized. Sociolinguists studying language change as a value-neutral process are structurally outside the philological tradition that set the discontinuity/continuity terms of debate.
% DISAPPEARANCE_RATIONALE: If the discontinuity framing vanished, critical editions would stop flagging medieval forms as 'corruptions,' curricula would present Medieval Latin as a parallel system rather than a degraded one, and the classical philology establishment's claim to arbitrate correctness across the full historical range of Latin would lose its evidentiary basis — funding and prestige allocations across classics and medieval studies departments would shift.
% FOUNDING_PROBLEM: Renaissance humanists needed a principled basis to reject what they saw as barbarous medieval usage and to recover an idealized Ciceronian eloquence; reconstruction from inscriptions and manuscripts gave that rejection a scholarly, rather than merely aesthetic, foundation.
% FOUNDING_PROBLEM_CORROBORATION: Historical linguists working outside classical philology (e.g., in general diachronic linguistics) attest that the sharp break the discontinuity reading assumes is not well-supported by continuous morphological data, suggesting the founding problem was more ideological (humanist self-legitimation) than descriptive. The classical philology establishment itself continues to attest the problem as live, but this attestation comes from the reading's own beneficiary tradition.
narrative_ontology:disappearance_verdict(correct_latin_kernel__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__discontinuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin_kernel__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__discontinuity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the discontinuity frame does real coordination work (a stable teaching standard) while also transferring prestige and canon-authority asymmetrically toward classicists and away from medievalists — moderate-high but not extreme, since the frame does not suppress access to medieval texts themselves, only their status. Suppression (0.52) captures that alternative framings (continuity, hybrid) are disfavored in traditional curricula but not forcibly excluded from scholarship — dissenting linguists publish freely. Theater ratio (0.44, declining over the interval from 0.55) reflects that early humanist rejection of medieval Latin was heavily performative (rhetorical purism), while modern philology increasingly grounds the discontinuity claim in genuine comparative morphological evidence, reducing (but not eliminating) the performative component over the long interval.
 *
 * PERSPECTIVAL GAP:
 *   From the classical philology establishment's seat, the discontinuity reading is a coordination achievement: a hard-won, evidence-based reconstruction of a lost system from fragmentary textual symbols, restoring order to what would otherwise be undifferentiated variation. From the medieval Latin scholar's seat, the same reconstruction operates as an extractive hierarchy — their functioning linguistic system is perpetually measured against, and found wanting relative to, a standard reconstructed by and for a rival subfield. The engine computes both seats' types from the same structural data; the divergence is expected and is not resolved by this story.
 *
 * DIRECTIONALITY LOGIC:
 *   The classical philology establishment and renaissance humanist tradition are structural beneficiaries: they set the terms of correctness and receive the disciplinary prestige of being custodians of the 'authentic' standard. Medieval Latin scholars and students of medieval documents are targets: their object of study is systematically framed as derivative, which constrains their institutional standing and pedagogical framing even though their textual access is not blocked. The medieval scribal tradition itself (a non-agent entity) bears a reputational cost with no capacity to respond, which is why it is marked agent:false and excluded from directionality math while still being named for narrative completeness.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — establishing a principled, teachable standard of Latin against perceived medieval barbarism — was live for Renaissance humanists working against genuinely divergent scribal Latin norms with no comparative linguistic apparatus. Today the problem is contested: modern historical linguistics has tools to describe Medieval Latin as a coherent system on its own terms, which would resolve the original problem differently (by de-pathologizing medieval usage rather than symbolically reoccupying lost classical structure). Where the discontinuity frame persists as unquestioned institutional common sense despite this available resolution, it risks mandatrophy — a corrective apparatus outliving the interpretive necessity that justified it, now sustained partly by disciplinary prestige structures rather than by unresolved descriptive need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_break_vs_ideological_construction,
    'Is the Classical/Medieval Latin discontinuity a genuine structural break in the linguistic system (comparable to, say, the Latin-to-Romance vernacular split), or is it substantially a retrospective, ideologically motivated construction serving Renaissance humanist and later classicist institutional interests?',
    'Comparative diachronic analysis of morphological, syntactic, and phonological continuity across the Classical-to-Medieval transition, benchmarked against known cases of genuine language-family splits versus known cases of register/style divergence within a continuously spoken and written language; corroboration sought from historical linguists with no institutional stake in either classics or medieval studies departments.',
    'If the break is substantially real, the discontinuity reading''s coordination function (providing a needed reconstructed standard) is better justified and the extraction component is a smaller share of the total. If the break is substantially constructed, more of the measured extraction (0.58) should be attributed to disciplinary rent-seeking rather than genuine descriptive necessity, and the case for reading this as a tangled_rope shifts further toward a snare-like reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_break_vs_ideological_construction, empirical, 'Whether the discontinuity claim tracks a real systemic break or an institutionally motivated reframing of ordinary continuous change.').

omega_variable(
    kernel_framing_choice_committer_axis,
    'The correct_latin_kernel is under-determined between three readings (continuity, discontinuity, hybrid) that are not merely differences of emphasis but produce structurally different classifications of what reconstruction from texts accomplished (internal correction vs. symbolic reoccupation vs. layered recovery). What determines which reading a given scholarly tradition or individual philologist adopts, and is the discontinuity reading favored by anything other than the historical accident of Renaissance humanist institutional dominance over the discipline''s founding terms?',
    'Track the institutional and disciplinary lineage of scholars adopting each reading; assess whether reading choice correlates with disciplinary training (classics vs. medieval studies vs. general historical linguistics) rather than with independent evaluation of the comparative evidence.',
    'If reading choice tracks disciplinary lineage rather than evidence, this supports treating the discontinuity reading''s dominance as partly a function of who inherited institutional gatekeeping power (classical philology) rather than of the reading''s superior descriptive adequacy — reinforcing the tangled_rope classification''s extraction component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice_committer_axis, conceptual, 'Documents that this story is one reading among three committer-axis alternatives, and names what would need to be true for the choice among them to be evidence-driven rather than institutionally path-dependent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__discontinuity_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__discontinuity_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(corr_tr_t100, correct_latin_kernel__discontinuity_reading, theater_ratio, 100, 0.53).
narrative_ontology:measurement(corr_tr_t200, correct_latin_kernel__discontinuity_reading, theater_ratio, 200, 0.5).
narrative_ontology:measurement(corr_tr_t300, correct_latin_kernel__discontinuity_reading, theater_ratio, 300, 0.47).
narrative_ontology:measurement(corr_tr_t400, correct_latin_kernel__discontinuity_reading, theater_ratio, 400, 0.45).
narrative_ontology:measurement(corr_tr_t500, correct_latin_kernel__discontinuity_reading, theater_ratio, 500, 0.445).
narrative_ontology:measurement(corr_tr_t600, correct_latin_kernel__discontinuity_reading, theater_ratio, 600, 0.44).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__discontinuity_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(corr_be_t100, correct_latin_kernel__discontinuity_reading, base_extractiveness, 100, 0.68).
narrative_ontology:measurement(corr_be_t200, correct_latin_kernel__discontinuity_reading, base_extractiveness, 200, 0.63).
narrative_ontology:measurement(corr_be_t300, correct_latin_kernel__discontinuity_reading, base_extractiveness, 300, 0.6).
narrative_ontology:measurement(corr_be_t400, correct_latin_kernel__discontinuity_reading, base_extractiveness, 400, 0.59).
narrative_ontology:measurement(corr_be_t500, correct_latin_kernel__discontinuity_reading, base_extractiveness, 500, 0.585).
narrative_ontology:measurement(corr_be_t600, correct_latin_kernel__discontinuity_reading, base_extractiveness, 600, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(correct_latin_kernel__discontinuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__discontinuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__discontinuity_reading, 0.08).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint families instantiating the correct_latin_kernel (discontinuity, continuity, hybrid readings). Each reading is authored as an independent ε-invariant constraint per the ε-invariance principle: the discontinuity reading's ε (0.58) reflects extraction/coordination within the discontinuity framework's own standing institutional arrangement, and is not averaged against or reconciled with the continuity or hybrid readings' independently authored ε values. All three are linked via affects_constraints because they compete for the same disciplinary and curricular resources (classics vs. medieval studies prestige, textbook adoption, canon formation) — a shift in dominance among the readings structurally affects resource allocation to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
