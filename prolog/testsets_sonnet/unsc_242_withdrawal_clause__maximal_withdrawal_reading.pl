% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__maximal_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__maximal_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__maximal_withdrawal_reading
 *   human_readable: UNSC 242 — Maximal (Comprehensive) Withdrawal Reading
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   This story instantiates the MAXIMAL WITHDRAWAL reading of the contested
 *   kernel formed by Security Council Resolution 242's Article 2(4)
 *   territorial-integrity default. On this reading, the French text's
 *   definite article ('des territoires occupés') controls interpretation,
 *   requiring withdrawal from ALL occupied territories rather than a
 *   discretionary subset, and the resolution functions as a Rope binding the
 *   occupying state to full retrocession in favor of dispossessed claimants
 *   with what this reading treats as an enforceable legal position. This is
 *   one of three linked readings of the same kernel
 *   (unsc_242_withdrawal_clause): the partial_withdrawal_reading treats the
 *   English indefinite article and 'secure boundaries' language as licensing
 *   retention of strategic territory, and interpretive_authority_structure
 *   treats the deeper contest as being about WHO has authority to resolve the
 *   ambiguity at all (ICJ, drafting states, or the occupying state's own
 *   practice). Per the ε-invariance principle, each reading is authored as
 *   its own constraint with its own stable ε — this file does not average or
 *   hedge across readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.71).
domain_priors:suppression_score(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.62).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "UNSC 242 — Maximal (Comprehensive) Withdrawal Reading").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__maximal_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '1d6bed46-b888-4da8-a1cc-8b1bf2432b28').
narrative_ontology:cs_kernel_codification('1d6bed46-b888-4da8-a1cc-8b1bf2432b28', fixed_text).
narrative_ontology:cs_authority_grounding('1d6bed46-b888-4da8-a1cc-8b1bf2432b28', distributed).
narrative_ontology:cs_reading_relation('1d6bed46-b888-4da8-a1cc-8b1bf2432b28', unsc_242_withdrawal_clause__partial_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('1d6bed46-b888-4da8-a1cc-8b1bf2432b28', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('1d6bed46-b888-4da8-a1cc-8b1bf2432b28', foundational, territorial_acquisition_by_force_categorically_void).
narrative_ontology:cs_axiom_status(territorial_acquisition_by_force_categorically_void, holdable).
narrative_ontology:cs_axiom_grounding('1d6bed46-b888-4da8-a1cc-8b1bf2432b28', territorial_acquisition_by_force_categorically_void, conventional).
narrative_ontology:cs_axiom('1d6bed46-b888-4da8-a1cc-8b1bf2432b28', secondary, authentic_multilingual_text_most_restrictive_reading_controls).
narrative_ontology:cs_axiom_status(authentic_multilingual_text_most_restrictive_reading_controls, holdable).
narrative_ontology:cs_axiom_grounding('1d6bed46-b888-4da8-a1cc-8b1bf2432b28', authentic_multilingual_text_most_restrictive_reading_controls, conventional).
narrative_ontology:cs_reference_frame('1d6bed46-b888-4da8-a1cc-8b1bf2432b28', comprehensive_territorial_restitution_baseline).
narrative_ontology:cs_drift_state('1d6bed46-b888-4da8-a1cc-8b1bf2432b28', contemporary, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('1d6bed46-b888-4da8-a1cc-8b1bf2432b28', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_palestinian_claimants).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, neighboring_arab_states_seeking_territorial_restoration).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state_security_establishment).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, settler_populations_in_occupied_territories).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__maximal_withdrawal_reading, territorial_integrity_default_doctrine).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__maximal_withdrawal_reading, prohibition_on_acquisition_of_territory_by_war).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold no seat at the Security Council and no enforcement mechanism of their own, but the maximal reading treats the territorial default as running directly in their favor — the constraint's coordination function (restoring pre-war boundaries) coincides with their substantive claim. They cannot compel withdrawal; their position depends entirely on other actors treating the reading as binding.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_palestinian_claimants, beneficiary,
    powerless, generational, trapped, regional).

% Lost territory in 1967 and press the maximal reading in every diplomatic and legal forum available, treating the French 'des territoires occupés' as controlling over the English 'from territories occupied.' They benefit directly from full retrocession and have pushed the reading into UN General Assembly resolutions, though they lack the coercive power to force compliance unilaterally.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, neighboring_arab_states_seeking_territorial_restoration, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__maximal_withdrawal_reading, neighboring_arab_states_seeking_territorial_restoration, agenda_setter).

% Holds administrative and military control over the disputed territories and would bear the full cost of the maximal reading: comprehensive withdrawal, dismantlement of security infrastructure, and loss of strategic depth. It has functioning alternative legal arguments (the partial reading) and diplomatic leverage, so its exit from the constraint's bite is not foreclosed — but every year of retained territory increases the political and legal cost of eventual compliance.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state_security_establishment, payer,
    institutional, generational, mobile, regional).

% Built homes, communities, and economic lives inside the disputed territories under the occupying state's sponsorship. Under the maximal reading their settlements are the very thing the withdrawal obligation exists to unwind; they have no standing before the Security Council and depend on their own state's continued rejection of the reading for their situation to persist.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, settler_populations_in_occupied_territories, payer,
    moderate, biographical, constrained, local).

% Drafted and adopted Resolution 242 in both English and French as equally authentic texts, then declined to resolve the resulting definite-article ambiguity through any subsequent binding clarification. It administers the resolution's continued invocation in diplomatic practice but has never compelled the reading it might be read to require.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, un_security_council, agenda_setter,
    institutional, civilizational, analytical, global).

% Has addressed the territorial-integrity default and the prohibition on acquisition of territory by force in advisory opinions touching the same territories, generally favoring the comprehensive reading of withdrawal obligations, without possessing enforcement power over Security Council members.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_court_of_justice, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__maximal_withdrawal_reading, diffuse).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__maximal_withdrawal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under the maximal reading, the resolution coordinates a return to the pre-war territorial baseline: it gives every party a single, legible standard (full withdrawal from territories occupied) rather than leaving boundary questions to be resettled by continued military advantage.
% TRANSFER_FUNCTION: Moves territorial control, security infrastructure, and settlement presence from the occupying state and its settler population back to the dispossessed claimants and neighboring states, reversing gains obtained through the 1967 conflict.
% ABSENT_VOICES: The settler populations most directly dispossessed by this reading have no seat at the Security Council and are not parties to the resolution; the Palestinian claimants for whose benefit the reading is chiefly invoked were also not signatories or negotiating parties to Resolution 242 itself.
% DISAPPEARANCE_RATIONALE: If the maximal reading were abandoned entirely, dispossessed claimants and neighboring states would lose their principal textual anchor for demanding comprehensive retrocession, and diplomatic pressure on the occupying state would shift toward negotiated partial settlements; the occupying state disputes that anything would meaningfully change since it already rejects the maximal reading's binding force in practice.
% FOUNDING_PROBLEM: The 1967 war left territories under military occupation with no agreed mechanism for restoring pre-war boundaries; the Security Council sought a formula that would prevent territorial acquisition by force from becoming permanent through simple inaction.
% FOUNDING_PROBLEM_CORROBORATION: The International Court of Justice, in advisory opinions addressing related occupied-territory questions, has repeatedly affirmed that the prohibition on acquisition of territory by force remains a live and unresolved problem in this specific dispute — corroboration from a judicial body outside both the claimant states and the occupying state.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__maximal_withdrawal_reading, contested).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__maximal_withdrawal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71 at present) because, under this reading, the constraint is comprehensive and mandatory rather than discretionary — every year the occupying state does not comply, the political and legal cost of eventual compliance compounds, which is modeled as rising extraction pressure over the interval. Suppression (0.62) reflects the active diplomatic, legal, and military apparatus required to hold the occupying state's territorial position against the maximal reading's claim, not any inherent instability in the reading itself. Theater ratio (0.4) reflects that a substantial share of diplomatic invocation of the resolution has become ritualized (repeated General Assembly restatements) without corresponding enforcement capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   The dispossessed claimants and neighboring states sit at the beneficiary end: the maximal reading's coordination function (restoring the pre-war baseline) directly serves their substantive claim, even though they lack the power to enforce it. The occupying state's security establishment and the settler populations it sponsors sit at the target end: comprehensive withdrawal is precisely what is extracted from them under this reading. The Security Council is agenda-setter by having adopted and continually re-invoked the resolution without resolving its own drafting ambiguity — an ambiguity whose persistence is convenient for continued diplomatic maneuver on all sides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing permanent territorial acquisition through military conquest) remains live by the ICJ's own corroboration, which is why founding_problem_status is 'live' rather than 'dead' — this blocks a mandatrophy finding. What keeps this reading from collapsing into pure extraction rhetoric is that it names a real, freestanding coordination function (a stable, legible restitution baseline) rather than being cover for opportunistic territorial claims invented after the fact; the claimant states' territorial losses in 1967 are independently documented, not manufactured by the reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definite_article_controlling_text,
    'Does the French text''s definite article (''des territoires occupés''), or the English text''s indefinite phrasing (''from territories occupied''), control interpretation when both are equally authentic UN texts?',
    'A binding ICJ ruling squarely on Resolution 242''s textual ambiguity (none currently exists) applying Vienna Convention rules on treaty interpretation to multi-authentic-text instruments, or a Security Council resolution explicitly clarifying scope.',
    'If the French text is held controlling, this maximal reading gains formal legal authority and the partial reading''s textual basis weakens substantially; if the English text or drafters'' intent controls, this reading loses its principal textual anchor.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(definite_article_controlling_text, conceptual, 'Which language text controls the withdrawal scope under Resolution 242.').

omega_variable(
    coordination_or_partisan_instrument,
    'Is the maximal reading a genuine coordination mechanism restoring an agreed pre-war baseline, or is it a partisan legal instrument adopted selectively by the parties who benefit from full retrocession?',
    'Comparative analysis of how consistently the maximal reading''s proponents apply the same comprehensive-restitution principle to territorial disputes elsewhere where they are not the beneficiary.',
    'If applied consistently across unrelated disputes, this supports the reading as principled coordination; if applied only where the claimant benefits, this supports treating the ''coordination function'' as cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_or_partisan_instrument, conceptual, 'Whether the maximal reading is principled or selectively invoked.').

omega_variable(
    enforceability_without_enforcement,
    'Does a Security Council resolution create an enforceable legal position for the beneficiary claimants when no enforcement mechanism has ever been triggered against the occupying state?',
    'Track subsequent Security Council practice for any Chapter VII enforcement action invoking Resolution 242 specifically, versus continued reliance on non-binding restatement.',
    'Absence of any enforcement action after decades would support classifying the beneficiaries'' ''enforceable legal position'' as largely theatrical rather than operative, raising the theater_ratio finding further.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforceability_without_enforcement, empirical, 'Whether the claimed legal enforceability has ever translated into enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(unsc_tr_t1979, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1979, 0.25).
narrative_ontology:measurement(unsc_tr_t1993, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1993, 0.3).
narrative_ontology:measurement(unsc_tr_t2000, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2000, 0.34).
narrative_ontology:measurement(unsc_tr_t2012, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2012, 0.38).
narrative_ontology:measurement(unsc_tr_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1967, 0.45).
narrative_ontology:measurement(unsc_be_t1979, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1979, 0.52).
narrative_ontology:measurement(unsc_be_t1993, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1993, 0.58).
narrative_ontology:measurement(unsc_be_t2000, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(unsc_be_t2012, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2012, 0.68).
narrative_ontology:measurement(unsc_be_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2024, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1967, 0.4).
narrative_ontology:measurement(unsc_su_t1979, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1979, 0.47).
narrative_ontology:measurement(unsc_su_t1993, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1993, 0.52).
narrative_ontology:measurement(unsc_su_t2000, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2000, 0.56).
narrative_ontology:measurement(unsc_su_t2012, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2012, 0.6).
narrative_ontology:measurement(unsc_su_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__maximal_withdrawal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, partial_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the kernel unsc_242_withdrawal_clause. partial_withdrawal_reading treats the same text as licensing discretionary, security-conditioned withdrawal; interpretive_authority_structure treats the deeper dispute as being about which body holds authority to resolve the ambiguity at all. All three share the same underlying resolution text but instantiate structurally distinct constraints with distinct ε, beneficiary/victim sets, and classifications — they are linked here rather than merged, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
