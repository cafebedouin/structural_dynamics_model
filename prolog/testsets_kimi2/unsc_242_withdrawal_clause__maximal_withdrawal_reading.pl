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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: unsc_242_withdrawal_clause__maximal_withdrawal_reading
 *   human_readable: UNSC 242 Maximal Withdrawal Obligation (French Definite Article Reading)
 *   domain: international law / diplomatic history / treaty interpretation
 *
 * SUMMARY:
 *   This constraint story instantiates the maximal-withdrawal reading of the
 *   UNSC 242 withdrawal clause kernel. Under this reading, the French
 *   definite article ('des territoires') in the authenticated French text of
 *   Resolution 242, read against the Charter Article 2(4) territorial
 *   integrity default, imposes a mandatory and comprehensive obligation on an
 *   occupying state to withdraw from all occupied territories. The English
 *   indefinite article ('territories') is subordinated. The beneficiaries are
 *   dispossessed territorial claimants who gain an enforceable legal position
 *   for full retrocession; the occupying state bears the comprehensive
 *   obligation. The constraint is claimed as rope because it coordinates the
 *   international community around a non-discretionary standard prohibiting
 *   territorial acquisition by force, even though the occupier experiences it
 *   as coercive. Sibling readings include the partial-withdrawal reading
 *   (indefinite English article, discretionary secure boundaries) and the
 *   interpretive-authority-structure reading (contested locus of interpretive
 *   power among ICJ, drafters, and occupying state).
 *
 * KEY AGENTS:
 *   - dispossessed_territorial_claimants: Primary beneficiary (moderate/constrained) â receive enforceable legal position for full territorial retrocession under the maximal reading
 *   - occupying_state: Primary target/payer (institutional/constrained) â bears the mandatory comprehensive withdrawal obligation and associated territorial loss
 *   - un_security_council: Agenda setter (institutional/constrained) â adopted Resolution 242 and maintains the Charter framework within which the maximal reading operates
 *   - international_court_of_justice: Analytical observer (institutional/analytical) â potential adjudicator of the textual dispute without direct territorial stake
 *   - drafting_states: Excluded voice (institutional/analytical) â intended a flexible boundary-security formula; their intent is overridden by the textual logic of the maximal reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.85).
domain_priors:suppression_score(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.66).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "UNSC 242 Maximal Withdrawal Obligation (French Definite Article Reading)").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "international law / diplomatic history / treaty interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__maximal_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'a2da5dd8-807c-40a7-9182-36ee398eca43').
narrative_ontology:cs_kernel_codification('a2da5dd8-807c-40a7-9182-36ee398eca43', fixed_text).
narrative_ontology:cs_authority_grounding('a2da5dd8-807c-40a7-9182-36ee398eca43', lineage).
narrative_ontology:cs_interpretation_layer_present('a2da5dd8-807c-40a7-9182-36ee398eca43').
narrative_ontology:cs_reading_relation('a2da5dd8-807c-40a7-9182-36ee398eca43', unsc_242_withdrawal_clause__partial_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('a2da5dd8-807c-40a7-9182-36ee398eca43', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('a2da5dd8-807c-40a7-9182-36ee398eca43', foundational, french_text_controls_withdrawal_scope).
narrative_ontology:cs_axiom_status(french_text_controls_withdrawal_scope, holdable).
narrative_ontology:cs_axiom_grounding('a2da5dd8-807c-40a7-9182-36ee398eca43', french_text_controls_withdrawal_scope, conventional).
narrative_ontology:cs_axiom('a2da5dd8-807c-40a7-9182-36ee398eca43', foundational, territorial_integrity_non_discretionary).
narrative_ontology:cs_axiom_status(territorial_integrity_non_discretionary, holdable).
narrative_ontology:cs_axiom_grounding('a2da5dd8-807c-40a7-9182-36ee398eca43', territorial_integrity_non_discretionary, deontological).
narrative_ontology:cs_reference_frame('a2da5dd8-807c-40a7-9182-36ee398eca43', un_charter_territorial_integrity_default).
narrative_ontology:cs_drift_state('a2da5dd8-807c-40a7-9182-36ee398eca43', contemporary_post_1967_diplomacy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a2da5dd8-807c-40a7-9182-36ee398eca43', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_territorial_claimants).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold an enforceable legal claim under the maximal reading of UNSC 242 and Charter Article 2(4) to full retrocession of all occupied territories; the French definite article reading secures their position against partial-settlement bargaining.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_territorial_claimants, beneficiary,
    moderate, generational, constrained, national).

% Bears the comprehensive obligation to withdraw from all occupied territories under the maximal reading; the French definite article eliminates the discretionary space that the indefinite English article might have offered.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state, payer,
    institutional, generational, constrained, national).

% Issued Resolution 242; under this reading, the Council's text (especially the authenticated French version with definite article) imposes a mandatory, non-discretionary withdrawal obligation tied to Charter Article 2(4).
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% Judicial arbiter that may be called to interpret the resolution; under this reading, the Court would apply the French definite article and Charter territorial integrity default to reject partial withdrawal arguments.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_court_of_justice, observer,
    institutional, generational, analytical, global).

% The original drafters intended a flexible formula allowing secure boundaries and partial retention; the maximal reading overrides their intent in favor of the textual logic of the definite article, effectively excluding their interpretive authority from the operative meaning.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, drafting_states, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_territorial_claimants).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform, non-discretionary standard for territorial restoration when occupation occurs, eliminating ad hoc bargaining over which territories must be returned and preventing collective-action failure among states seeking to uphold the Charter prohibition on territorial acquisition by force.
% TRANSFER_FUNCTION: Moves territorial control comprehensively from occupying state to dispossessed claimant; moves interpretive authority away from drafting states' intent toward the textual default of Charter Article 2(4) and the French definite article.
% ABSENT_VOICES: Drafting states who intended a flexible, security-oriented boundary settlement; occupying states advocating for strategic territorial retention; partial-withdrawal jurists who read the indefinite English article as controlling.
% DISAPPEARANCE_RATIONALE: If the maximal withdrawal obligation vanished, the legal architecture of post-occupation settlement would shift to discretionary, security-based boundary negotiations; dispossessed claimants would lose their enforceable position for full retrocession, and the Charter territorial integrity default would be significantly weakened.
% FOUNDING_PROBLEM: The 1967 Middle East war created a crisis over occupied territories where ad hoc political bargaining risked legitimizing territorial acquisition by force; the Council needed a formula that would tie territorial restoration to the Charter's Article 2(4) prohibition without reopening the text for amendment.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars outside the beneficiary community (e.g., independent publicists and ICJ jurists) attest that the resolution was drafted to balance multiple aims; occupying states and some drafting-state archives contest that the problem was ever meant to mandate total withdrawal.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__maximal_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__maximal_withdrawal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.85, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.85) is authored high because the maximal reading mandates comprehensive territorial retrocession, which constitutes a massive transfer of control from occupier to claimant. Suppression (0.66) reflects the active textual and jurisprudential suppression of the partial-withdrawal alternative; the definite article reading must continuously displace the indefinite-article reading in diplomatic and legal discourse. Theater ratio (0.28) captures the moderate performative dimension of legal argumentation around treaty authentication, though the coordination function (territorial integrity) is substantively real. Accessibility collapse (0.78) is high because once the Charter default and French textual logic are accepted, partial-withdrawal arguments lose structural legal footing. Resistance (0.82) is high because occupying states have consistently resisted full withdrawal in practice. Metrics and claim are independently authored: the claimed rope status reflects the constraint's coordination function for the international community, while the metrics describe the heavy structural load the constraint places on the occupying party.
 *
 * PERSPECTIVAL GAP:
 *   The dispossessed claimant seat experiences this constraint as coordination â a clear, enforceable legal standard that solves the collective-action problem of resisting territorial acquisition by force. The occupying state seat experiences the identical constraint as high-extraction coercion that strips territorial control without negotiation. The drafting-state seat experiences it as a misappropriation of their flexible formula. The engine computes these divergences from the beneficiary/victim declarations and exit options: claimants are beneficiaries with constrained exit but legal empowerment; the occupier is a payer with institutional power yet diplomatically constrained exit; drafters are excluded from the current operative text.
 *
 * DIRECTIONALITY LOGIC:
 *   Dispossessed territorial claimants are declared beneficiaries (low d, subsidy/empowerment direction). The occupying state is declared victim/payer (high d, target direction). The UN Security Council and ICJ are neither beneficiaries nor victims; their directionality reverts to the institutional power-atom fallback near symmetric (0.5), reflecting their administrative/adjudicative role without direct territorial stake. Drafting states are excluded and do not feed directionality computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing territorial acquisition by force and providing a stable post-war settlement rule â remains contested in status. The constraint's mandate has not clearly outlived its function: territorial integrity is still a live coordination problem. The classification as rope (rather than snare) is protected by the genuine coordination function: the constraint solves a real collective-action problem for the community of states by standardizing the post-occupation baseline. The high extraction from the occupier does not by itself convert the constraint to a snare because the extraction is incident to the coordination (restoring the territorial status quo), not a separate rent-seeking mechanism. Were the coordination function shown to be cover for targeted dispossession of one particular occupier, the classification would shift toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    french_definite_article_travaux_prep,
    'Do the travaux prÃ©paratoires and simultaneous interpretation records confirm the French definite article was deliberately chosen to mandate comprehensive withdrawal, or was it a by-product of bilingual drafting?',
    'Archival review of the 1967 UN Security Council negotiating records and comparison with the English text''s indefinite article.',
    'If the drafters did not intend comprehensiveness, the maximal reading''s natural-law textual logic weakens and the constraint may reclassify as tangled rope (coordination function real but later interpreters layered extraction onto it).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(french_definite_article_travaux_prep, empirical, 'Empirical uncertainty about drafters'' intent behind the French definite article').

omega_variable(
    interpretive_authority_locus,
    'Does the authority to fix the resolution''s meaning reside in the authenticated text, the original drafters, the ICJ, or the Security Council itself?',
    'Comparative analysis of VCLT Articles 31-33 application to Security Council resolutions and the UN''s bilingual authentication practice.',
    'If the text alone controls, the constraint remains rope; if an interpreter must actively suppress drafters'' intent to sustain the reading, the constraint shifts toward tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_locus, conceptual, 'Conceptual ambiguity about where interpretive authority lies').

omega_variable(
    occupier_reciprocity_benefit,
    'Can the occupying state ever be a net long-term beneficiary of a strict territorial-integrity rule through reciprocity, reputation, or institutional stability?',
    'Game-theoretic and historical analysis of state compliance with territorial-integrity norms over repeated interactions.',
    'If the occupier is also a beneficiary in the long run, directionality for the occupier shifts downward toward symmetric; if never, the occupier remains a pure target.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(occupier_reciprocity_benefit, empirical, 'Empirical uncertainty whether the occupier benefits from the norm long-term').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_242_max_tr_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(unsc_242_max_tr_t10, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(unsc_242_max_tr_t20, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(unsc_242_max_tr_t30, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(unsc_242_max_tr_t40, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(unsc_242_max_tr_t50, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(unsc_242_max_tr_t60, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(unsc_242_max_be_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(unsc_242_max_be_t10, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(unsc_242_max_be_t20, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(unsc_242_max_be_t30, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 30, 0.75).
narrative_ontology:measurement(unsc_242_max_be_t40, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 40, 0.79).
narrative_ontology:measurement(unsc_242_max_be_t50, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 50, 0.82).
narrative_ontology:measurement(unsc_242_max_be_t60, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 60, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(unsc_242_max_su_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(unsc_242_max_su_t10, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(unsc_242_max_su_t20, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(unsc_242_max_su_t30, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(unsc_242_max_su_t40, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(unsc_242_max_su_t50, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 50, 0.64).
narrative_ontology:measurement(unsc_242_max_su_t60, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 60, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, partial_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% This constraint is the maximal-withdrawal reading of the UNSC 242 withdrawal clause kernel. It decomposes from the natural-language concept 'UNSC 242 withdrawal obligation' by isolating the specific claim that the French definite article mandates comprehensive territorial retrocession, distinct from the partial-withdrawal reading (indefinite English article) and the interpretive-authority reading (contested locus of meaning). Each reading carries a distinct epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
