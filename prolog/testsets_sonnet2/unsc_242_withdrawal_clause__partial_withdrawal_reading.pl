% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__partial_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__partial_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__partial_withdrawal_reading
 *   human_readable: UNSC Resolution 242 Withdrawal Clause — Partial/Discretionary Withdrawal Reading
 *   domain: international_law/diplomatic_history
 *
 * SUMMARY:
 *   UNSC Resolution 242 (1967) calls for 'withdrawal of Israeli armed forces
 *   from territories occupied in the recent conflict' alongside 'the right of
 *   every State in the area to live in peace within secure and recognized
 *   boundaries.' The English text's omission of a definite article before
 *   'territories' has been read, principally by the occupying power and the
 *   drafting Western powers, as licensing partial and negotiated withdrawal
 *   keyed to security arrangements rather than a comprehensive return to
 *   pre-1967 lines. This story instantiates that reading only. The
 *   maximal-withdrawal reading (French definite-article text, Article 2(4)
 *   territorial-integrity default) and the interpretive-authority-structure
 *   reading (who gets to adjudicate the ambiguity at all — ICJ, drafting
 *   states, or occupying state via practice) are separate constraints with
 *   their own ε values and stakeholder structures, linked here via
 *   network.affects_constraints. Conflating them would violate ε-invariance:
 *   the maximal reading measures a near-mountain textual obligation with low
 *   authored ambiguity, while this reading measures a moderate,
 *   actively-negotiated extraction mechanism riding on that same textual gap.
 *
 * KEY AGENTS:
 *   - occupying_power: Primary beneficiary (institutional/arbitrage) — retains strategic territory and controls withdrawal pace
 *   - great_power_mediators: Secondary beneficiary (institutional/arbitrage) — retains diplomatic centrality by keeping the question open
 *   - displaced_claimant_population: Primary target (powerless/trapped) — bears cost of indefinite non-resolution
 *   - neighboring_frontline_states: Secondary target (moderate/constrained) — bears ongoing security and diplomatic costs
 *   - international_court_of_justice: Analytical observer — has not delivered a controlling ruling
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.52).
domain_priors:suppression_score(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.58).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__partial_withdrawal_reading, "UNSC Resolution 242 Withdrawal Clause — Partial/Discretionary Withdrawal Reading").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__partial_withdrawal_reading, "international_law/diplomatic_history").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__partial_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__partial_withdrawal_reading, '0354e250-d346-4888-b32d-e49e331a6439').
narrative_ontology:cs_kernel_codification('0354e250-d346-4888-b32d-e49e331a6439', fixed_text).
narrative_ontology:cs_authority_grounding('0354e250-d346-4888-b32d-e49e331a6439', distributed).
narrative_ontology:cs_reading_relation('0354e250-d346-4888-b32d-e49e331a6439', unsc_242_withdrawal_clause__maximal_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('0354e250-d346-4888-b32d-e49e331a6439', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('0354e250-d346-4888-b32d-e49e331a6439', foundational, textual_indefiniteness_encodes_negotiated_discretion).
narrative_ontology:cs_axiom_status(textual_indefiniteness_encodes_negotiated_discretion, holdable).
narrative_ontology:cs_axiom_grounding('0354e250-d346-4888-b32d-e49e331a6439', textual_indefiniteness_encodes_negotiated_discretion, conventional).
narrative_ontology:cs_axiom('0354e250-d346-4888-b32d-e49e331a6439', foundational, secure_boundaries_clause_permits_strategic_retention).
narrative_ontology:cs_axiom_status(secure_boundaries_clause_permits_strategic_retention, holdable).
narrative_ontology:cs_axiom_grounding('0354e250-d346-4888-b32d-e49e331a6439', secure_boundaries_clause_permits_strategic_retention, instrumental).
narrative_ontology:cs_reference_frame('0354e250-d346-4888-b32d-e49e331a6439', id_1967_ceasefire_compromise_text).
narrative_ontology:cs_drift_state('0354e250-d346-4888-b32d-e49e331a6439', post_oslo_negotiation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0354e250-d346-4888-b32d-e49e331a6439', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, great_power_mediators).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, displaced_claimant_population).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, neighboring_frontline_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls territories taken in the 1967 war and cites the indefinite English article ('withdrawal from territories', not 'the territories') plus the resolution's 'secure and recognized boundaries' language as textual license to withdraw only partially, on its own security terms and its own schedule. Retains strategic depth (border highlands, river lines, buffer zones) while negotiating incremental, conditioned withdrawals. Can stall indefinitely because no fixed line or timetable is imposed on it by the text itself.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, beneficiary,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, agenda_setter).

% The drafting powers (principally the US and UK, whose English-language draft prevailed procedurally at the Security Council vote) benefit from the ambiguity they authored: it keeps them as indispensable brokers of phased, negotiated withdrawal rather than executors of a fixed legal mandate. Their diplomatic leverage, aid relationships, and mediation role all depend on withdrawal scope remaining a matter of ongoing negotiation rather than settled law.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, great_power_mediators, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, great_power_mediators, agenda_setter).

% Refugees and residents of the occupied territories whose claim to return, sovereignty, or compensation depends on a fixed enforcement line the indefinite-article reading refuses to supply. Each round of 'phased withdrawal' negotiation re-opens the scope question, meaning no negotiated outcome is ever final against them; they have no seat at the negotiating table and no independent enforcement mechanism to invoke.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, displaced_claimant_population, payer,
    powerless, civilizational, trapped, regional).

% States bordering the retained strategic territories bear ongoing security costs, periodic military confrontation, and diplomatic pressure to accept interim arrangements as if they were final settlements. Their exit options are constrained by asymmetric military power and dependence on the same great-power mediators who benefit from the ambiguity.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, neighboring_frontline_states, payer,
    moderate, generational, constrained, regional).

% The bilingual drafting staff who flagged at the time that the English and French texts diverged were overruled by the political decision to adopt the English text as the working version while leaving French, Spanish, Russian, and Arabic texts (all using definite articles) as equally authentic under UN rules. Their technical objection was not resolved; it was procedurally set aside.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, drafting_delegation_translators, excluded,
    powerless, immediate, trapped, regional).

% Has been asked in advisory contexts to interpret the resolution's binding force and scope but has not delivered a definitive ruling that resolves the article-indefiniteness question against this reading; its advisory opinions are cited by both sides without settling the enforcement gap.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, international_court_of_justice, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__partial_withdrawal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a formula both the occupying power and the great-power mediators can accept at the moment of drafting — ending active war footing and establishing a negotiating framework — without either side having to accept the other's preferred final map at the point of adoption.
% TRANSFER_FUNCTION: Moves negotiating leverage and de facto territorial control toward the occupying power and diplomatic centrality toward the mediating powers, while moving the cost of indefinite non-resolution — displacement, statelessness, recurring conflict — onto the claimant population and neighboring states who have no vote on the interpretation.
% ABSENT_VOICES: The displaced claimant population was not a party to the Security Council vote and has never held a seat at the table where the withdrawal-scope question is adjudicated; the original bilingual drafting staff who flagged the English/French divergence were overruled procedurally and their technical dissent was never incorporated into the resolution's operative text.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned in favor of a fixed enforcement line, the occupying power would lose its principal legal cover for phased/conditional withdrawal, mediating powers would lose a central lever of ongoing diplomatic relevance, and the claimant population would gain a concrete, litigable benchmark against which non-compliance could be measured — the negotiating architecture of the entire conflict would have to reorganize around a different question.
% FOUNDING_PROBLEM: In November 1967, the Security Council needed language that could pass unanimously among members who disagreed on whether full withdrawal to pre-1967 lines was required; the indefinite article was, per multiple drafters' later accounts, a deliberate compromise to secure passage rather than a settled position on scope.
% FOUNDING_PROBLEM_CORROBORATION: Some original drafters (British diplomat Lord Caradon, who authored the operative English text) later stated publicly that the omission of 'the' before 'territories' was intentional and meant to leave withdrawal scope open to negotiation over secure boundaries — an account corroborating this reading from inside the drafting process itself, though other participants in the same negotiations and independent legal scholars dispute that intent controls over the equally authentic French text, and no neutral third party outside the drafting states and the occupying power has affirmed that the ambiguity was meant to be permanent rather than a temporary drafting compromise.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__partial_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__partial_withdrawal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored as moderate (0.52) rather than high because the constraint is genuinely conditional and phased — some withdrawal and disengagement has actually occurred over the decades (Sinai, partial West Bank redeployments), which is real coordination activity, not pure theater. But the trajectory rises steadily from 1967 to 2024 because each cycle of negotiation re-opens rather than closes the scope question, layering additional rent-extraction (continued settlement activity, prolonged negotiating leverage) onto what began as a genuine diplomatic compromise. Suppression is authored higher (0.58) and also rising, reflecting the active diplomatic, legal, and military apparatus required to keep the indefinite reading operative against a competing textually-authentic reading that would otherwise control. Theater ratio rises to 0.42 as repeated 'peace process' negotiating rounds increasingly perform engagement with withdrawal scope without a fixed enforcement mechanism ever emerging.
 *
 * PERSPECTIVAL GAP:
 *   From the occupying power's seat this reads as prudent, security-driven diplomacy consistent with the resolution's own 'secure and recognized boundaries' language — a rope. From the claimant population's seat the same structure operates as an open-ended extraction mechanism disguised as an ongoing peace process — approaching a snare. The engine computes both from the same structural data; the divergence is the substantive finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying power and great-power mediators are structural beneficiaries: the indefinite reading converts textual ambiguity directly into negotiating leverage they control, and their exit options are arbitrage-grade (they can walk away from any specific negotiating round without losing their underlying position). The displaced claimant population is the structural target — trapped, powerless, with no independent legal mechanism to force resolution — and directionality is derived at the high end. Neighboring frontline states sit closer to target than beneficiary: they have some diplomatic leverage (moderate power) but constrained exit given asymmetric military capacity and dependence on the same mediators who benefit from the ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — ending active 1967 war-footing and establishing SOME negotiating framework — was arguably solved by the early 1970s when active hostilities on that front subsided. The withdrawal-scope ambiguity has persisted for over five decades since, well past the point where its original coordination function (securing a ceasefire framework acceptable to all Security Council members) was live. This is a classic mandatrophy signature: a mandate (open-ended negotiated withdrawal) outliving the founding problem it was built to solve (securing passage of an emergency ceasefire resolution), while the constraint's operative beneficiaries treat the mandate as permanently justified by an ongoing security rationale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drafters_intent_vs_textual_ambiguity,
    'Was the indefinite English article a deliberate substantive choice by the drafters to permit partial withdrawal, or an artifact of translation/drafting compromise never intended to control interpretation permanently?',
    'Declassified negotiating records and diplomatic cables from the November 1967 drafting sessions; comparison against contemporaneous statements by all voting Council members, not only the English-drafting powers.',
    'If deliberate and substantive, this reading has a stronger textualist claim to be the controlling interpretation. If an artifact of compromise never meant to permanently override the other four equally authentic language texts, this reading is a constructed extraction mechanism riding on a translation accident rather than a genuine coordination outcome — which would push its classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drafters_intent_vs_textual_ambiguity, empirical, 'Whether the English indefinite article encodes deliberate drafters'' intent or an unresolved translation artifact.').

omega_variable(
    secure_boundaries_scope_limit,
    'Does the ''secure and recognized boundaries'' clause function as a narrow security-arrangement provision (buffer zones, demilitarization) or as an open-ended license for permanent territorial retention?',
    'Comparative analysis of how ''secure boundaries'' language has been applied in other UN-mediated territorial settlements where a comparable clause existed alongside a definite withdrawal obligation.',
    'A narrow reading would sharply reduce the legitimate scope of this constraint''s discretionary claim, pushing its classification toward tangled_rope-with-declining-legitimacy or snare; a broad reading sustains the current moderate-extraction tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secure_boundaries_scope_limit, conceptual, 'Whether the secure-boundaries clause is a narrow security provision or an open-ended retention license.').

omega_variable(
    committer_reading_disagreement_locus,
    'This constraint is one reading (partial_withdrawal_reading) of the unsc_242_withdrawal_clause kernel. The sibling readings — maximal_withdrawal_reading and interpretive_authority_structure — differ from this one at the point of which language text and which interpretive authority controls. Where exactly does the disagreement live: in the text itself (English vs. French), in the authority to adjudicate (ICJ vs. drafting-state intent vs. state practice), or in both simultaneously?',
    'A definitive ICJ advisory opinion squarely addressing both the textual-authenticity question (which language version controls, or whether all five must be read consistently) and the authority question (who may authoritatively resolve textual conflict in Security Council resolutions) would collapse this ambiguity; absent that, the disagreement persists as a live multi-locus dispute.',
    'If the disagreement is purely textual, resolving the language-authenticity question would settle scope directly. If it is purely about interpretive authority, even a clear textual answer would not settle the dispute because parties would contest who gets to declare it clear. Current evidence suggests both loci are simultaneously contested, which is why three separate constraints (rather than one) are required under the ε-invariance principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_reading_disagreement_locus, conceptual, 'Whether the kernel disagreement is located in textual authenticity, interpretive authority, or both.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__partial_withdrawal_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(unsc_tr_t1978, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1978, 0.28).
narrative_ontology:measurement(unsc_tr_t1993, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1993, 0.35).
narrative_ontology:measurement(unsc_tr_t2005, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(unsc_tr_t2015, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(unsc_tr_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1967, 0.3).
narrative_ontology:measurement(unsc_be_t1978, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1978, 0.38).
narrative_ontology:measurement(unsc_be_t1993, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1993, 0.44).
narrative_ontology:measurement(unsc_be_t2005, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(unsc_be_t2015, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(unsc_be_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1967, 0.35).
narrative_ontology:measurement(unsc_su_t1978, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1978, 0.42).
narrative_ontology:measurement(unsc_su_t1993, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1993, 0.5).
narrative_ontology:measurement(unsc_su_t2005, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2005, 0.54).
narrative_ontology:measurement(unsc_su_t2015, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2015, 0.56).
narrative_ontology:measurement(unsc_su_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__partial_withdrawal_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.12).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% This constraint is the partial_withdrawal_reading member of the unsc_242_withdrawal_clause kernel family (3 stories). unsc_242_maximal_withdrawal_reading instantiates the competing claim that withdrawal is mandatory from all occupied territories per the French definite-article text and Charter Article 2(4) territorial-integrity default — a claim with lower authored ambiguity and a different beneficiary/victim structure (claimant population as beneficiary of a fixed line, occupying power as the constrained party). unsc_242_interpretive_authority_structure addresses the prior, structurally distinct question of which body (ICJ, drafting states, or occupying state via customary practice) has authority to resolve the textual conflict at all — its ε and stakeholder set concern institutional authority contest, not territorial scope. Each story authors its own ε and classification per the ε-invariance principle; do not average or merge their metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
