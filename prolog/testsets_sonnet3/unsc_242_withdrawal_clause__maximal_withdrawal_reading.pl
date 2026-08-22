% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__maximal_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: UNSC 242 Withdrawal Clause — Maximal (Full Retrocession) Reading
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   UN Security Council Resolution 242 (1967) calls for 'withdrawal of Israel
 *   armed forces from territories occupied in the recent conflict,' a formula
 *   whose English text lacks a definite article ('territories') while its
 *   French text carries one ('des territoires'), and whose drafting history
 *   is contested by every party. This story instantiates the MAXIMAL reading:
 *   the French definite article controls, and Charter Article 2(4)'s
 *   territorial-integrity default operates as the interpretive backstop,
 *   yielding a mandatory and comprehensive withdrawal obligation. Under this
 *   reading the constraint functions as a Rope binding the occupying state to
 *   full retrocession, with dispossessed claimants as the coordinated
 *   beneficiaries of a stable, textually anchored resolution formula. This is
 *   a deliberately narrow, single-reading story per the ε-invariance
 *   principle: the partial-withdrawal reading (discretionary scope,
 *   secure-boundaries doctrine) and the interpretive-authority-structure
 *   reading (contest over WHO decides) are separate constraints with their
 *   own ε and their own stakeholder sets, linked here only through
 *   network.affects_constraints and the cs_structure.reading_relations block.
 *   The claim (rope) and the metrics (high ε, moderate-high suppression,
 *   rising theater ratio) are authored independently: this reading is claimed
 *   as genuine coordination around a textual standard, but its metrics
 *   describe a constraint that has required increasing enforcement rhetoric
 *   and increasingly performative invocation over nearly six decades without
 *   achieving the retrocession it mandates.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.78).
domain_priors:suppression_score(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.6).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "UNSC 242 Withdrawal Clause — Maximal (Full Retrocession) Reading").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__maximal_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'f70f9780-ec4b-4235-aee6-80ef2d7d946d').
narrative_ontology:cs_kernel_codification('f70f9780-ec4b-4235-aee6-80ef2d7d946d', fixed_text).
narrative_ontology:cs_authority_grounding('f70f9780-ec4b-4235-aee6-80ef2d7d946d', distributed).
narrative_ontology:cs_reading_relation('f70f9780-ec4b-4235-aee6-80ef2d7d946d', unsc_242_withdrawal_clause__partial_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('f70f9780-ec4b-4235-aee6-80ef2d7d946d', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('f70f9780-ec4b-4235-aee6-80ef2d7d946d', foundational, territorial_acquisition_by_force_categorically_inadmissible).
narrative_ontology:cs_axiom_status(territorial_acquisition_by_force_categorically_inadmissible, holdable).
narrative_ontology:cs_axiom_grounding('f70f9780-ec4b-4235-aee6-80ef2d7d946d', territorial_acquisition_by_force_categorically_inadmissible, conventional).
narrative_ontology:cs_axiom('f70f9780-ec4b-4235-aee6-80ef2d7d946d', secondary, authentic_multilingual_text_with_definite_article_controls_scope).
narrative_ontology:cs_axiom_status(authentic_multilingual_text_with_definite_article_controls_scope, holdable).
narrative_ontology:cs_axiom_grounding('f70f9780-ec4b-4235-aee6-80ef2d7d946d', authentic_multilingual_text_with_definite_article_controls_scope, conventional).
narrative_ontology:cs_reference_frame('f70f9780-ec4b-4235-aee6-80ef2d7d946d', post_1967_ceasefire_territorial_status_quo_ante).
narrative_ontology:cs_drift_state('f70f9780-ec4b-4235-aee6-80ef2d7d946d', post_oslo_normalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f70f9780-ec4b-4235-aee6-80ef2d7d946d', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_territorial_claimants).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, arab_states_parties_to_conflict).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state_administration).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, settler_populations_in_occupied_territory).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__maximal_withdrawal_reading, territorial_integrity_default_doctrine).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__maximal_withdrawal_reading, inadmissibility_of_acquisition_of_territory_by_war).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Populations and successor political authorities whose land came under occupation in 1967. Under this reading they hold an enforceable legal entitlement to full territorial retrocession, backed by the Charter's territorial-integrity default and the French-text definite article. They have no independent enforcement capacity of their own and depend entirely on Security Council will, third-state pressure, or ICJ advisory weight to convert the legal entitlement into an actual return of territory.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_territorial_claimants, beneficiary,
    moderate, generational, trapped, regional).

% The drafting and negotiating states that advanced the French-text, comprehensive-withdrawal reading at the UN and in subsequent diplomacy. They set the interpretive agenda by pressing this reading in every negotiation round and multilateral forum, and they benefit from the reading's comprehensiveness, but cannot compel compliance without Security Council enforcement action they do not control.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, arab_states_parties_to_conflict, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__maximal_withdrawal_reading, arab_states_parties_to_conflict, agenda_setter).

% The state administering the occupied territories bears the full cost of this reading: it is read as mandated, comprehensive withdrawal with no discretion for retained buffer zones or negotiated boundary adjustment. It can resist via diplomatic non-compliance, veto-power patronage, and de facto settlement policy, but each of those responses itself accrues as evidence for the constraint's continued relevance rather than as an exit from it.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state_administration, payer,
    powerful, biographical, constrained, regional).

% Civilian populations who settled in occupied territory under the administering state's policy. Under the maximal reading their presence has no valid legal basis and full withdrawal would require their removal or absorption into a successor sovereignty; they have built lives and communities premised on continued occupation and have no say in the interpretive contest that determines their fate.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, settler_populations_in_occupied_territory, payer,
    moderate, biographical, trapped, local).

% The Security Council's permanent members drafted the ambiguous dual-language text and retain sole authority to issue binding follow-on resolutions or enforcement measures. Individual members can selectively invoke or downplay the maximal reading depending on alliance interests, and veto power lets any one of them block enforcement regardless of the reading's textual merit.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Issues advisory opinions and judgments touching on the territorial-integrity default and the inadmissibility of territorial acquisition by force, lending doctrinal weight to the maximal reading without binding enforcement power of its own.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_court_of_justice, observer,
    institutional, civilizational, analytical, global).

% States that broker peace processes premised on some withdrawal formula would prefer the interpretive question resolved rather than left open, since ambiguity prolongs the negotiations they underwrite, but they are not parties to the resolution's text and have no seat in settling which article's grammar controls.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, third_party_mediator_states, excluded,
    powerful, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, textually anchored standard — full withdrawal from occupied territory — around which post-conflict negotiations, peacekeeping mandates, and third-party mediation can converge instead of re-litigating first principles in every negotiation round.
% TRANSFER_FUNCTION: Moves legal and political leverage from the occupying state to the dispossessed claimants and their state sponsors: sovereignty claims, negotiating position at future talks, and the presumption of illegality attaching to continued occupation and settlement.
% ABSENT_VOICES: Settler populations living in the occupied territory have no seat in the interpretive contest despite bearing the direct consequence of a maximal reading; the occupying state's domestic security establishment, which frames retention as existential rather than discretionary, is also outside the room where the textual question is adjudicated.
% DISAPPEARANCE_RATIONALE: If the maximal reading collapsed entirely — if no party could invoke comprehensive withdrawal as the operative interpretation — occupying-state settlement policy would lose its principal legal counterweight, dispossessed claimants would lose their strongest textual basis for full retrocession, and future ceasefire and armistice negotiations would default toward the partial-withdrawal / negotiated-boundary framework instead.
% FOUNDING_PROBLEM: The 1967 war left territories occupied under contested legal status; the Council needed a formula that would let combatants negotiate peace without either side conceding its foundational legal position on the war's outcome.
% FOUNDING_PROBLEM_CORROBORATION: UN legal counsel opinions and successive Secretary-General reports treat the territorial-integrity default as operative doctrine; independent international-law scholarship outside both the claimant states and the occupying state's own legal establishment continues to treat the French-text comprehensive-withdrawal reading as textually well-grounded, corroborating that the founding problem — unresolved territorial status from a war of conquest — remains live rather than resolved by subsequent practice.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__maximal_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__maximal_withdrawal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is high (0.78 at 2024) because, under this reading, the obligation is total and non-negotiable — there is no discretionary carve-out, so every day of continued occupation constitutes ongoing extraction of territorial control from the beneficiary claimants under the reading's own terms. Suppression is moderate-high (0.6) rather than extreme because the constraint has no independent enforcement mechanism; its coercive force is borrowed from diplomatic isolation, UN General Assembly censure, and international-law scholarly consensus rather than a standing enforcement body, so occupying-state non-compliance is costly but not blocked outright. Theater ratio rises across the interval (0.2 to 0.4) as repeated invocation of the maximal reading in diplomatic communiques, UN debates, and legal briefs increasingly substitutes for any operative progress toward the withdrawal it mandates — a textbook proxy-goal substitution pattern.
 *
 * PERSPECTIVAL GAP:
 *   From the claimant beneficiary seat, this reading is simply what the Charter and the authentic French text require — a Rope binding all parties to a shared, legitimate standard. From the occupying administration's seat, the same textual claim operates as an externally imposed, ever-tightening extraction of negotiating leverage that ignores the competing English-text and security-doctrine reading it considers equally authoritative. The engine computes these divergent per-seat classifications from the structural power/exit data; the claimed type (rope) reflects the beneficiary-adjacent framing this reading instantiates, not a resolution of the underlying textual dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   Dispossessed territorial claimants and their state sponsors are the structural beneficiaries: the maximal reading is the strongest available textual basis for their claim, and it costs them nothing to hold — the burden of proof and compliance falls entirely on the occupying administration. The occupying state and the settler populations it has enabled are the targets: under this reading their continued territorial presence has no valid legal basis, and their only available response — asserting the competing partial-withdrawal reading — is itself the subject of the sibling constraint file, not a resolution available within this one. Settler populations sit at trapped exit because their situation was created by a policy choice they did not make and cannot unmake by individual action.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unresolved territorial sovereignty following a war whose outcome neither side would concede in the text itself — remains live by the corroboration of independent international-law scholarship and continuing UN practice, not merely by the say of the claimant beneficiaries. This blocks a mandatrophy verdict: the arrangement has not outlived a solved problem, since the underlying territorial dispute is unresolved as of 2024. What has drifted is the ratio of theatrical invocation to operative effect — the resolution is cited far more than it is implemented — which is captured in the rising theater_ratio series rather than in a founding-problem-status flip to 'dead.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    french_english_text_authenticity,
    'Is the French-text definite article (''des territoires occupés'') genuinely authoritative over the English indefinite formulation, or were both language versions intended as equally authentic with the ambiguity deliberately preserved by the drafters?',
    'Vienna Convention on the Law of Treaties Article 33 analysis of equally authentic multilingual texts, combined with declassified drafting-history records (UK and US delegation cables from 1967) establishing whether the English indefinite article was a deliberate concession to secure-boundaries advocates or an oversight.',
    'If the French text is found genuinely controlling per VCLT practice, this reading''s textual claim strengthens considerably; if the ambiguity was a deliberate bilingual compromise, this reading and the partial-withdrawal reading remain permanently co-authoritative rather than one superseding the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(french_english_text_authenticity, empirical, 'Whether the French definite article is genuinely dispositive under treaty-interpretation rules or the ambiguity was intentional.').

omega_variable(
    kernel_reading_contest_location,
    'Where exactly does the interpretive disagreement between the maximal and partial readings live — in the grammar of the two authentic texts, in the drafters'' subjective intent, or in the subsequent practice of the parties?',
    'This is the subject of the sibling constraint interpretive_authority_structure, which models the second-order contest over WHO has authority to resolve the ambiguity (ICJ judicial interpretation vs. drafting-state intent vs. occupying-state customary practice) as its own constraint with its own ε and stakeholder set.',
    'If judicial interpretation is found to control, this maximal reading gains institutional backing it currently lacks; if customary practice controls, the occupying state''s decades of retention become self-legitimating, which would sharply lower this reading''s practical ε even if its textual ε remains unchanged.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Locates the committer-level disagreement as a distinct, linked constraint rather than folding it into this reading''s classification.').

omega_variable(
    settler_population_remedy_ambiguity,
    'Under a maximal-withdrawal enforcement scenario, what happens to settler populations who have lived in the occupied territory for decades — removal, absorption into successor sovereignty, or a negotiated residency arrangement outside the resolution''s text?',
    'Comparative analysis of prior territorial retrocession settlements (e.g., Sinai withdrawal precedent) for how settler populations were actually treated when a comprehensive-withdrawal reading was implemented in practice.',
    'If precedent shows orderly negotiated resettlement, the victim classification for settler populations moderates; if precedent shows forced removal or prolonged limbo, the victim status and the exit_options=trapped designation are strongly corroborated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(settler_population_remedy_ambiguity, empirical, 'Unresolved question about the concrete remedy for settler populations under this reading''s own logic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(unsc_tr_t1978, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1978, 0.28).
narrative_ontology:measurement(unsc_tr_t1993, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1993, 0.32).
narrative_ontology:measurement(unsc_tr_t2004, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2004, 0.36).
narrative_ontology:measurement(unsc_tr_t2014, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2014, 0.38).
narrative_ontology:measurement(unsc_tr_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1967, 0.55).
narrative_ontology:measurement(unsc_be_t1978, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1978, 0.62).
narrative_ontology:measurement(unsc_be_t1993, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1993, 0.68).
narrative_ontology:measurement(unsc_be_t2004, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2004, 0.73).
narrative_ontology:measurement(unsc_be_t2014, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2014, 0.76).
narrative_ontology:measurement(unsc_be_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1967, 0.4).
narrative_ontology:measurement(unsc_su_t1978, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1978, 0.48).
narrative_ontology:measurement(unsc_su_t1993, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1993, 0.52).
narrative_ontology:measurement(unsc_su_t2004, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2004, 0.56).
narrative_ontology:measurement(unsc_su_t2014, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2014, 0.58).
narrative_ontology:measurement(unsc_su_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__maximal_withdrawal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, partial_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language label 'the UNSC 242 withdrawal clause,' per the ε-invariance principle: maximal_withdrawal_reading (this file, high ε, Rope-claimed, beneficiaries = dispossessed claimants), partial_withdrawal_reading (lower ε, discretionary-scope claim, different beneficiary map favoring the occupying state's security doctrine), and interpretive_authority_structure (the second-order contest over who holds interpretive authority — ICJ, drafters, or the occupying state's customary practice — which structurally influences both first-order readings by determining which one is treated as authoritative in any given forum). The upstream-downstream relationship runs from interpretive_authority_structure (which forum's ruling matters) toward both first-order readings (which reading that forum would endorse).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
