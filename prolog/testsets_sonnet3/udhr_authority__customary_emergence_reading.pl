% ============================================================================
% CONSTRAINT STORY: udhr_authority__customary_emergence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_authority__customary_emergence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: udhr_authority__customary_emergence_reading
 *   human_readable: UDHR as Emergent Customary International Law (State Practice + Opinio Juris)
 *   domain: international_law/political_philosophy/human_rights
 *
 * SUMMARY:
 *   In 1948 the UDHR was adopted as a declaration explicitly outside the
 *   treaty system, expected to carry moral rather than legal force. Over the
 *   following decades, a doctrinal argument developed and hardened: certain
 *   UDHR provisions had, through consistent invocation by states,
 *   international bodies, and tribunals, become binding customary
 *   international law independent of any state's individual consent. This
 *   customary-emergence account is now itself invoked strategically — by
 *   advocacy networks to pressure non-complying states, by powerful states
 *   selectively (asserting bindingness against rivals while resisting it
 *   domestically), and by scholars and tribunals whose institutional role is
 *   to adjudicate exactly when crystallization has occurred. The claim/metric
 *   divergence here is structural to the reading itself: it is claimed as
 *   tangled_rope (a genuine doctrinal solution to a genuine gap — how does
 *   moral consensus become law without formal ratification — that
 *   simultaneously enables asymmetric strategic use) and the metrics track
 *   its documented drift toward higher extraction and higher theater as the
 *   doctrine matured and was put to increasingly instrumental use.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_authority__customary_emergence_reading, 0.48).
domain_priors:suppression_score(udhr_authority__customary_emergence_reading, 0.42).
domain_priors:theater_ratio(udhr_authority__customary_emergence_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_authority__customary_emergence_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(udhr_authority__customary_emergence_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(udhr_authority__customary_emergence_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_authority__customary_emergence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(udhr_authority__customary_emergence_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_authority__customary_emergence_reading, tangled_rope).
narrative_ontology:human_readable(udhr_authority__customary_emergence_reading, "UDHR as Emergent Customary International Law (State Practice + Opinio Juris)").
narrative_ontology:topic_domain(udhr_authority__customary_emergence_reading, "international_law/political_philosophy/human_rights").

domain_priors:requires_active_enforcement(udhr_authority__customary_emergence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_authority__customary_emergence_reading, '2ca677a3-7da3-47d3-b413-bbace46d217d').
narrative_ontology:cs_kernel_codification('2ca677a3-7da3-47d3-b413-bbace46d217d', distributed).
narrative_ontology:cs_authority_grounding('2ca677a3-7da3-47d3-b413-bbace46d217d', practice).
narrative_ontology:cs_interpretation_layer_present('2ca677a3-7da3-47d3-b413-bbace46d217d').
narrative_ontology:cs_reading_relation('2ca677a3-7da3-47d3-b413-bbace46d217d', udhr_authority__binding_universalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('2ca677a3-7da3-47d3-b413-bbace46d217d', udhr_authority__aspirational_sovereignty_reading, influences).
narrative_ontology:cs_axiom('2ca677a3-7da3-47d3-b413-bbace46d217d', foundational, bindingness_accrues_through_accumulated_practice_and_opinio_juris).
narrative_ontology:cs_axiom_status(bindingness_accrues_through_accumulated_practice_and_opinio_juris, holdable).
narrative_ontology:cs_axiom_grounding('2ca677a3-7da3-47d3-b413-bbace46d217d', bindingness_accrues_through_accumulated_practice_and_opinio_juris, conventional).
narrative_ontology:cs_axiom('2ca677a3-7da3-47d3-b413-bbace46d217d', secondary, consent_at_adoption_is_not_required_once_crystallization_occurs).
narrative_ontology:cs_axiom_status(consent_at_adoption_is_not_required_once_crystallization_occurs, holdable).
narrative_ontology:cs_axiom_grounding('2ca677a3-7da3-47d3-b413-bbace46d217d', consent_at_adoption_is_not_required_once_crystallization_occurs, empirically_contingent).
narrative_ontology:cs_reference_frame('2ca677a3-7da3-47d3-b413-bbace46d217d', customary_international_law_two_element_test).
narrative_ontology:cs_drift_state('2ca677a3-7da3-47d3-b413-bbace46d217d', post_cold_war_human_rights_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2ca677a3-7da3-47d3-b413-bbace46d217d', '').
narrative_ontology:cs_kernel_id(udhr_authority__customary_emergence_reading, udhr_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_authority__customary_emergence_reading, powerful_states_with_favorable_practice_records).
narrative_ontology:constraint_beneficiary(udhr_authority__customary_emergence_reading, international_law_scholars_and_tribunals).
narrative_ontology:constraint_beneficiary(udhr_authority__customary_emergence_reading, human_rights_advocacy_networks).
narrative_ontology:constraint_victim(udhr_authority__customary_emergence_reading, states_with_contested_practice_records).
narrative_ontology:constraint_victim(udhr_authority__customary_emergence_reading, individuals_in_non_complying_states).
narrative_ontology:constraint_victim(udhr_authority__customary_emergence_reading, smaller_states_lacking_practice_documentation_capacity).
narrative_ontology:constraint_vindicates(udhr_authority__customary_emergence_reading, customary_international_law_doctrine).
narrative_ontology:constraint_vindicates(udhr_authority__customary_emergence_reading, opinio_juris_as_binding_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have long, well-documented diplomatic and judicial practice they can cite as evidence of opinio juris, and control much of the scholarly apparatus (foreign ministries, national courts, funded research institutes) that produces the record. Can selectively invoke customary status when it constrains rivals and disclaim it when it would constrain themselves, because the transition point from aspiration to binding custom is itself undetermined.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, powerful_states_with_favorable_practice_records, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(udhr_authority__customary_emergence_reading, powerful_states_with_favorable_practice_records, agenda_setter).

% Adjudicate and write the treatises that determine which provisions have 'crystallized' into custom. Their determinations carry no independent enforcement power but shape which claims other actors can credibly make. Career and institutional prestige are built on being the ones who declare crystallization, giving them a stake in the doctrine's continued indeterminacy as much as its eventual resolution.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, international_law_scholars_and_tribunals, agenda_setter,
    institutional, civilizational, analytical, global).

% Use the customary-law claim strategically in litigation, UN proceedings, and public campaigns to pressure non-complying states, gaining leverage they would not have under a pure treaty-consent framework. Depend on the ambiguity persisting long enough to be useful in each new case rather than being definitively settled.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, human_rights_advocacy_networks, beneficiary,
    organized, generational, constrained, global).

% Face accusations of violating 'binding custom' derived from a document they may have voted for as aspiration only, decades before any claim of customary status arose. Cannot cleanly exit the obligation because withdrawal from a non-treaty customary norm is not a recognized legal act; their consent was never sought for the transformation itself.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, states_with_contested_practice_records, payer,
    moderate, biographical, constrained, national).

% Are the intended beneficiaries of the underlying rights but have no standing to invoke the customary-law claim directly against their own state in most domestic systems; the doctrine's practical enforcement runs through state-to-state diplomacy and international bodies that may or may not act, leaving the individual's actual protection contingent on politics far outside their control.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, individuals_in_non_complying_states, payer,
    powerless, biographical, trapped, national).

% Lack the diplomatic archives, legal staff, and scholarly attention that make a state's practice legible as evidence of opinio juris. Their compliance or non-compliance is measured unevenly against a record that powerful states control the production of, so the same customary-law claim falls on them with less capacity to contest or reinterpret it.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, smaller_states_lacking_practice_documentation_capacity, payer,
    powerless, generational, trapped, national).

% Monitor state compliance and issue findings referencing customary status alongside treaty obligations, without power to compel compliance. Sit between the doctrinal claim and its practical effect, translating the emergence narrative into soft findings that states can accept or contest.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, un_human_rights_treaty_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism by which a widely-endorsed but originally non-binding declaration can acquire binding legal force through the accumulation of consistent state behavior and expressed legal conviction, allowing the international system to recognize normative consensus without requiring universal treaty ratification.
% TRANSFER_FUNCTION: Moves interpretive and coercive leverage from states that resist being bound by evolving custom to states, tribunals, and advocacy networks positioned to declare what the custom now requires — and moves practical protection (or its absence) onto individuals whose treatment is the subject of the claim but who hold no direct standing to invoke it.
% ABSENT_VOICES: Individuals in non-complying states are the ultimate rights-holders under any reading but have no seat in the doctrinal debate about when and whether the transformation to binding custom has occurred; smaller states whose practice is undocumented or under-studied are effectively unable to contest characterizations of 'general practice' made on their behalf or against them.
% DISAPPEARANCE_RATIONALE: Advocacy networks and tribunals would say the world rearranges sharply — a major lever for pressuring non-complying states through customary-law claims disappears, and enforcement collapses back to treaty consent alone. States with contested practice records would say little changes in their actual treatment of persons, since the customary claim was already unenforceable against them in practice; the dispute over which account is correct is itself constitutive of this reading.
% FOUNDING_PROBLEM: The UDHR was adopted in 1948 as a non-binding declaration precisely because states would not accept binding human rights obligations by treaty at that time; the customary-emergence account exists to solve the follow-on problem of how a document explicitly designed to avoid binding force could nonetheless become binding decades later without a formal treaty-ratification event.
% FOUNDING_PROBLEM_CORROBORATION: The International Court of Justice and academic customary-international-law scholarship (outside advocacy networks) have at various points recognized specific UDHR provisions (e.g., prohibition of genocide, slavery, racial discrimination) as customary, providing corroboration from a source with institutional distance from the beneficiary advocacy networks; however, no comparably independent source corroborates that the UDHR AS A WHOLE has crystallized into custom, and several states' foreign ministries dispute the general claim outright — so corroboration exists for a narrow subset of the claim but not for the doctrine's broad strategic use.
narrative_ontology:disappearance_verdict(udhr_authority__customary_emergence_reading, contested).
narrative_ontology:founding_problem_status(udhr_authority__customary_emergence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_authority__customary_emergence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_authority__customary_emergence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_authority__customary_emergence_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_authority__customary_emergence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_authority__customary_emergence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_authority__customary_emergence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction starts low (0.10 in 1948, when the customary-emergence claim barely existed as doctrine) and rises steadily to 0.48 by 2024 as the claim became a routine tool in international litigation and diplomatic pressure, used unevenly against states with weaker capacity to contest the practice record. Theater ratio also climbs (0.20 to 0.44) because an increasing share of invocations of 'customary status' function as rhetorical leverage in specific disputes rather than as settled findings applied consistently — the same provision is asserted as crystallized against one state and left uninvoked against another with comparable practice. Suppression rises moderately (0.15 to 0.42) reflecting the growing but still incomplete machinery (treaty body findings, ICJ dicta, scholarly consensus-building) that makes resisting a customary-law claim costlier over time without providing any clean, symmetric mechanism for a state to contest or exit the classification. All three series share the 1948-2024 grid.
 *
 * DIRECTIONALITY LOGIC:
 *   Powerful states with long, well-curated practice records are the structural beneficiaries: they can invoke or disclaim customary status opportunistically because they control much of the evidentiary record and have the diplomatic and legal capacity to shape which claims are made against them. International law scholars and tribunals set the interpretive agenda without directly extracting resources, so they are agenda_setters whose institutional interest lies partly in the doctrine's productive ambiguity. Advocacy networks are genuine beneficiaries of the leverage the doctrine provides even though they do not control it. States with contested or thin practice records, and especially individuals in non-complying states, sit at the target end: they bear reputational, diplomatic, or in the individual case, unremedied-harm costs from a legal transformation to which their own consent was structurally irrelevant.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to give legal teeth to a document deliberately designed without them — was genuinely live in the mid-20th century and arguably remains live for a narrow, ICJ-corroborated set of provisions (genocide, slavery, racial discrimination prohibitions). Classifying this as tangled_rope rather than snare prevents mislabeling a doctrine with real coordination value (it lets the international system recognize genuine normative consensus without waiting for universal treaty ratification) as pure extraction; classifying it as tangled_rope rather than rope prevents ignoring the asymmetric way the doctrine's indeterminacy is exploited by parties with unequal capacity to shape the practice record. The founding_problem_status is authored contested precisely because the doctrine's narrow, corroborated core (a small set of provisions) looks live and settled, while its broad strategic use (treating the UDHR as a whole as crystallized custom) looks more like an extractive gloss on a narrower genuine achievement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    crystallization_threshold_indeterminacy,
    'At what point, precisely, does accumulating state practice plus opinio juris cross from evidence of a norm''s existence into a fully binding customary rule — and who has the authority to declare that the threshold has been crossed?',
    'A body of consistent ICJ and arbitral tribunal rulings applying an explicit, reproducible test for crystallization across many UDHR provisions, rather than case-by-case dicta; alternatively, a codified restatement (e.g., by the International Law Commission) that fixes criteria ex ante rather than ex post.',
    'If the threshold is genuinely indeterminate and irreducibly contestable, the strategic-use extraction identified in this story is a permanent structural feature of the doctrine, not a transitional defect — supporting a durable tangled_rope classification. If a reproducible test emerges and is applied evenly, the doctrine could stabilize into either a rope (a real, evenly-applied coordination solution) or collapse toward the binding_universalism_reading for the settled provisions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crystallization_threshold_indeterminacy, conceptual, 'Whether the aspiration-to-custom transition has (or could have) a determinate, non-strategic crossing point.').

omega_variable(
    even_versus_selective_practice_evidencing,
    'Is the unevenness in which states'' practice gets counted as evidence of opinio juris a contingent, fixable data problem, or a structural feature of who controls the production of the international-law record?',
    'Comparative empirical study of how frequently tribunals and scholars cite practice from powerful versus smaller states when assessing crystallization of the same provisions, controlling for actual compliance behavior.',
    'If the unevenness is a fixable data problem (better archives, more scholarly attention to under-documented states), the extraction identified here is partly an artifact of resource asymmetry, correctable without abandoning the doctrine. If it is structural (powerful states will always better control their own evidentiary record), the asymmetric extraction is intrinsic to the customary-emergence mechanism itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(even_versus_selective_practice_evidencing, empirical, 'Whether unequal capacity to document practice is a correctable artifact or intrinsic to the doctrine.').

omega_variable(
    kernel_committer_reading_boundary,
    'Where exactly does the customary_emergence_reading''s authority claim diverge from the aspirational_sovereignty_reading''s claim, given that both agree the 1948 UDHR was non-binding at adoption?',
    'Compare the two readings'' treatment of a specific test case: a state that voted for the UDHR in 1948, never ratified any subsequent binding human-rights treaty covering the same provisions, but is now accused of violating ''customary'' obligations derived from the UDHR. The aspirational_sovereignty_reading would require ongoing state consent (e.g., via treaty or persistent-objector status) for bindingness; this reading holds the state bound regardless, once crystallization is found, unless it can show persistent objection.',
    'This is the committer-structure content required by Rule 2: it does not change this story''s own ε, but documents that the sibling readings diverge specifically on whether a state''s post-1948 silence counts as acquiescence (this reading) or as continued non-consent (aspirational_sovereignty_reading) — the disagreement is located in the treatment of state silence over time, not in the founding facts, which both readings share.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_reading_boundary, conceptual, 'Locates the specific structural disagreement between this reading and the aspirational_sovereignty_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_authority__customary_emergence_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_authority__customary_emergence_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(udhr_tr_t1966, udhr_authority__customary_emergence_reading, theater_ratio, 1966, 0.28).
narrative_ontology:measurement(udhr_tr_t1984, udhr_authority__customary_emergence_reading, theater_ratio, 1984, 0.34).
narrative_ontology:measurement(udhr_tr_t2000, udhr_authority__customary_emergence_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(udhr_tr_t2012, udhr_authority__customary_emergence_reading, theater_ratio, 2012, 0.41).
narrative_ontology:measurement(udhr_tr_t2024, udhr_authority__customary_emergence_reading, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_authority__customary_emergence_reading, base_extractiveness, 1948, 0.1).
narrative_ontology:measurement(udhr_be_t1966, udhr_authority__customary_emergence_reading, base_extractiveness, 1966, 0.18).
narrative_ontology:measurement(udhr_be_t1984, udhr_authority__customary_emergence_reading, base_extractiveness, 1984, 0.28).
narrative_ontology:measurement(udhr_be_t2000, udhr_authority__customary_emergence_reading, base_extractiveness, 2000, 0.37).
narrative_ontology:measurement(udhr_be_t2012, udhr_authority__customary_emergence_reading, base_extractiveness, 2012, 0.43).
narrative_ontology:measurement(udhr_be_t2024, udhr_authority__customary_emergence_reading, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_authority__customary_emergence_reading, suppression_requirement, 1948, 0.15).
narrative_ontology:measurement(udhr_su_t1966, udhr_authority__customary_emergence_reading, suppression_requirement, 1966, 0.22).
narrative_ontology:measurement(udhr_su_t1984, udhr_authority__customary_emergence_reading, suppression_requirement, 1984, 0.29).
narrative_ontology:measurement(udhr_su_t2000, udhr_authority__customary_emergence_reading, suppression_requirement, 2000, 0.34).
narrative_ontology:measurement(udhr_su_t2012, udhr_authority__customary_emergence_reading, suppression_requirement, 2012, 0.38).
narrative_ontology:measurement(udhr_su_t2024, udhr_authority__customary_emergence_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_authority__customary_emergence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_authority__customary_emergence_reading, 0.12).
narrative_ontology:affects_constraint(udhr_authority__customary_emergence_reading, binding_universalism_reading).
narrative_ontology:affects_constraint(udhr_authority__customary_emergence_reading, aspirational_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the udhr_authority kernel. binding_universalism_reading treats UDHR obligations as justiciable and consent-independent from the outset (or from a fixed early date), producing a high, relatively flat extraction profile with a hard-edged victim set (non-complying states, full stop). aspirational_sovereignty_reading treats the UDHR as perpetually non-binding absent explicit state consent (treaty ratification), producing a low, flat extraction profile with essentially no coercive victims. This reading (customary_emergence_reading) is distinguished by its TEMPORAL structure: extraction is authored as rising from near-zero in 1948 to moderate by 2024, because the reading's own premise is that bindingness is achieved gradually and is most exploitable precisely during the ambiguous transition window, which by this reading's own account has not fully closed even now. All three stories share the same underlying text (the UDHR) but diverge on the mechanism and timing of its authority, producing three different ε trajectories rather than three different measurements of one ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
