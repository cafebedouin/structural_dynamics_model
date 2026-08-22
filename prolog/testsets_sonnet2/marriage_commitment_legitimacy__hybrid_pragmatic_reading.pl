% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__hybrid_pragmatic_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__hybrid_pragmatic_reading
 *   human_readable: 1890 Manifesto as Strategic Ambiguity: Prophetic Authority Managing Federal Crisis While Preserving Doctrinal Core
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   This story instantiates the hybrid_pragmatic_reading of the
 *   marriage_commitment_legitimacy kernel: the 1890 Manifesto is read neither
 *   as pure revelation (endogenous_reinterpretation_reading) nor as pure
 *   coerced capitulation with unchanged doctrine
 *   (exogenous_override_reading), but as a deliberate institutional strategy
 *   that used prophetic authority AS THE INSTRUMENT for managing an
 *   existential external crisis while preserving maximal doctrinal
 *   flexibility through calculated scope ambiguity — the declaration
 *   addresses future practice without definitively resolving the eternal
 *   status of existing plural marriages or the truth-value of the original
 *   revelation. This reading treats the ambiguity itself as the load-bearing
 *   structural feature: it is what let leadership satisfy federal courts,
 *   preserve institutional assets, retain the claim to continuous revelation,
 *   and defer (rather than answer) the doctrinal question — at the direct
 *   cost of the individuals left to live inside the unresolved space.
 *   Extractiveness (0.52) sits at a moderate level reflecting this reading's
 *   assessment: institutional leadership captures the coordination benefit
 *   (survival, legitimacy, asset retention) while rank-and-file plural
 *   families and later fundamentalist dissenters absorb the interpretive and
 *   social costs the ambiguity was engineered to externalize.
 *
 * KEY AGENTS:
 *   - church_hierarchical_leadership: institutional beneficiary and agenda-setter, deploys ambiguity strategically
 *   - rank_and_file_plural_families: bear interpretive uncertainty with no clear resolution
 *   - excommunicated_fundamentalist_dissenters: punished for holding the pre-1890 position never explicitly repudiated as false
 *   - federal_government: satisfied by practical compliance, indifferent to doctrinal resolution
 *   - local_ecclesiastical_officials: administer the ambiguity leadership declined to resolve
 *   - church_historians_and_scholars: analytical seat assessing sincerity vs. strategy over time
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.52).
domain_priors:suppression_score(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.58).
domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "1890 Manifesto as Strategic Ambiguity: Prophetic Authority Managing Federal Crisis While Preserving Doctrinal Core").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '1911ff51-f27c-433b-968d-2115ca2b99c0').
narrative_ontology:cs_kernel_codification('1911ff51-f27c-433b-968d-2115ca2b99c0', formalized).
narrative_ontology:cs_authority_grounding('1911ff51-f27c-433b-968d-2115ca2b99c0', lineage).
narrative_ontology:cs_interpretation_layer_present('1911ff51-f27c-433b-968d-2115ca2b99c0').
narrative_ontology:cs_reading_relation('1911ff51-f27c-433b-968d-2115ca2b99c0', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('1911ff51-f27c-433b-968d-2115ca2b99c0', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('1911ff51-f27c-433b-968d-2115ca2b99c0', foundational, ambiguity_as_deliberate_institutional_instrument).
narrative_ontology:cs_axiom_status(ambiguity_as_deliberate_institutional_instrument, holdable).
narrative_ontology:cs_axiom_grounding('1911ff51-f27c-433b-968d-2115ca2b99c0', ambiguity_as_deliberate_institutional_instrument, instrumental).
narrative_ontology:cs_axiom('1911ff51-f27c-433b-968d-2115ca2b99c0', foundational, prophetic_authority_compatible_with_strategic_scope_management).
narrative_ontology:cs_axiom_status(prophetic_authority_compatible_with_strategic_scope_management, holdable).
narrative_ontology:cs_axiom_grounding('1911ff51-f27c-433b-968d-2115ca2b99c0', prophetic_authority_compatible_with_strategic_scope_management, conventional).
narrative_ontology:cs_reference_frame('1911ff51-f27c-433b-968d-2115ca2b99c0', continuous_revelation_through_living_prophet).
narrative_ontology:cs_drift_state('1911ff51-f27c-433b-968d-2115ca2b99c0', post_second_manifesto_enforcement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1911ff51-f27c-433b-968d-2115ca2b99c0', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, church_hierarchical_leadership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_plural_families).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, excommunicated_fundamentalist_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_government).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, local_ecclesiastical_officials).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the 1890 declaration and subsequently controls its interpretation, deploying prophetic authority to satisfy federal prosecutors and secure statehood while never formally repudiating the underlying doctrine of eternal marriage's plural form. Retains discretion to enforce, relax, or reinterpret compliance depending on institutional need — the ambiguity itself is the asset, letting leadership claim continuity to internal audiences and rupture to external ones simultaneously. Preserves temple assets, political legitimacy, and hierarchical control by managing rather than resolving the doctrinal tension.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, church_hierarchical_leadership, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__hybrid_pragmatic_reading, church_hierarchical_leadership, beneficiary).

% Existing plural families are told to comply with the law going forward while leadership offers no clear guidance on whether their marriages remain valid, whether they must separate, or whether continued cohabitation constitutes ongoing sin or ongoing obedience. Many are left to interpret ambiguous signals for themselves, facing social shame, prosecution risk, or spiritual anxiety depending on local leaders' inconsistent enforcement. Exit from the marriages is costly (family, economic, spiritual); exit from the Church is nearly unthinkable given identity investment.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_plural_families, payer,
    powerless, biographical, trapped, regional).

% Members who take the pre-1890 doctrine at face value and continue or initiate plural marriage are excommunicated as the institution enforces the new practice boundary while insisting doctrine has not changed. They bear the full cost of the ambiguity leadership created — punished for holding the position the institution itself once required and never explicitly renounced as false, only as impractical.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, excommunicated_fundamentalist_dissenters, payer,
    powerless, generational, trapped, regional).

% Achieves its policy objective — cessation of plural marriage as an organized practice — without needing to fully verify the sincerity or completeness of the doctrinal reversal. Accepts the Manifesto's ambiguity as sufficient compliance because the practical outcome (institutional withdrawal from the practice) satisfies the enforcement goal, even though the underlying theological claim about eternal marriage's true form is left unresolved.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_government, beneficiary,
    institutional, generational, analytical, national).

% Bishops and stake presidents must apply the ambiguous directive to actual families in their communities, deciding case by case whether to discipline, tolerate, or quietly ignore continuing plural households. They absorb the interpretive burden leadership declined to resolve centrally, and inconsistent local enforcement becomes a further source of instability and resentment they did not create but must administer.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, local_ecclesiastical_officials, agenda_setter,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__hybrid_pragmatic_reading, local_ecclesiastical_officials, payer).

% Study the documentary record, private correspondence, and subsequent second Manifesto (1904) to assess whether the 1890 declaration reflects genuine revelation, coerced capitulation, or calculated ambiguity. Their analysis can shift the institution's own official narrative over time, but they do not control practice or doctrine themselves.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, church_historians_and_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__hybrid_pragmatic_reading, church_hierarchical_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the institution a single, publicly citable act that simultaneously satisfies federal legal pressure (ending organized plural marriage) and preserves internal continuity of prophetic authority and core eternal-marriage theology, avoiding a costly, explicit doctrinal reversal that might fracture belief in continuous revelation.
% TRANSFER_FUNCTION: Moves institutional survival and property preservation (temple assets, statehood prospects, hierarchical legitimacy) to Church leadership and the collective membership as an ongoing entity, at the cost of clarity, consistency, and closure borne by individual plural families and dissenters who are left to navigate — and in some cases be punished under — an unresolved doctrinal boundary.
% ABSENT_VOICES: Plural wives and children whose family and inheritance status became legally and socially ambiguous are rarely quoted in the official record; fundamentalist dissenters who took the earlier doctrine literally are excommunicated and their theological argument (that the practice was never actually repudiated as false) is institutionally unrepresented in mainstream Church history.
% DISAPPEARANCE_RATIONALE: Had this ambiguous framing not been adopted — had leadership instead issued either an unambiguous doctrinal repudiation or an unambiguous defiance of federal law — the institution's subsequent history (statehood, temple retention, membership continuity, the eventual splintering of fundamentalist sects) would have diverged sharply. The scope ambiguity is load-bearing for the institution's continued existence in its current form.
% FOUNDING_PROBLEM: The Church faced simultaneous federal prosecution, disincorporation, and seizure of temple property under the Edmunds-Tucker Act, threatening institutional survival, while core leadership retained genuine belief in the eternal validity of plural marriage as revealed doctrine.
% FOUNDING_PROBLEM_CORROBORATION: Federal government records and contemporary press accounts from outside the Church corroborate that the crisis (disincorporation, property seizure, mass imprisonment of Church leaders) was genuinely severe and externally imposed. However, whether the response constituted sincere revelation, coerced capitulation, or calculated hybrid strategy is attested differently by faithful Church historians (revelation), critical historians and fundamentalist splinter groups (coercion or strategy), with no fully independent adjudicator; the ambiguity itself is part of what is contested.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__hybrid_pragmatic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.52 (moderate, not high) because this reading holds that genuine coordination value existed — institutional survival was a real collective good for the membership, not pure rent extraction — but leadership's choice to preserve ambiguity rather than resolve it converted what could have been a clean transition into an ongoing extraction of certainty from the people most affected. Theater ratio rises through 1904 (peaking near the Second Manifesto, when renewed federal scrutiny forced another round of performative clarification without full resolution) then settles as enforcement stabilizes. Suppression requirement rises correspondingly as the Church had to actively discipline dissenters who read the original doctrine literally, which is exactly the enforcement cost this reading predicts: an ambiguous settlement requires ongoing coercive maintenance because it never closes the interpretive question it opened.
 *
 * DIRECTIONALITY LOGIC:
 *   Church hierarchical leadership sits near the beneficiary end: institutional continuity, asset retention, and preserved claim to prophetic authority all accrue there, with maximal exit options (they set the terms). Rank-and-file plural families and fundamentalist dissenters sit near the target end: trapped by identity and community investment, they bear the cost of living inside a doctrinal space leadership deliberately left open. The federal government is a secondary beneficiary — it gets its policy outcome without needing the deeper theological question settled, which is precisely why it accepted an ambiguous rather than an explicit resolution.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (existential federal threat to institutional survival) was genuinely live in 1890 and objectively resolved by the 1900s (statehood achieved, prosecutions ended, property returned). Under a naive reading this should trigger full mandatrophy — the crisis passed, so continued ambiguity should have been retired. But this reading holds that the ambiguity was retained well past the crisis precisely because it had already proven useful for a second function (doctrinal flexibility management) unrelated to the original federal threat — which is why founding_problem_status is authored as contested rather than cleanly dead: one function (crisis management) ended, but a second function (managing internal theological continuity) persisted and is still cited by the institution today.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sincerity_vs_strategy_indeterminacy,
    'Was the scope ambiguity in the 1890 Manifesto a deliberate strategic choice by leadership, or an emergent property of genuine theological uncertainty under crisis conditions that was only later recognized as institutionally useful?',
    'Close documentary analysis of private leadership correspondence and journal entries from 1888-1891 versus public statements; comparison with the drafting process and internal debate record, to the extent it survives or has been disclosed.',
    'If deliberate strategy, this reading''s extraction attribution to leadership intent is strongly supported. If emergent/unintentional, the same structural outcome (members bearing interpretive cost) persists but the moral attribution to leadership as a knowing beneficiary weakens, which would push the reading toward the exogenous_override framing for the founding moment even while retaining hybrid dynamics in the following decades.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sincerity_vs_strategy_indeterminacy, conceptual, 'Whether the ambiguity was authored deliberately or emerged unintentionally from crisis conditions.').

omega_variable(
    second_manifesto_as_ratchet_or_correction,
    'Does the 1904 Second Manifesto represent a tightening of the same strategic ambiguity (a ratchet extending leadership''s flexible authority) or a genuine correction closing a loophole leadership had not intended to leave open?',
    'Compare enforcement records and excommunication rates before and after 1904 against the stated rationale in Church council minutes, where available, and against contemporaneous external reporting.',
    'A ratchet reading strengthens the tangled_rope classification (deepening extraction under continued ambiguity); a correction reading suggests convergence toward a cleaner rope or scaffold once the loophole closed, which would argue for periodizing this constraint into two intervals rather than one continuous story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(second_manifesto_as_ratchet_or_correction, empirical, 'Whether the 1904 Second Manifesto intensified or resolved the original ambiguity.').

omega_variable(
    kernel_framing_choice,
    'Is the ''hybrid pragmatic'' framing itself a defensible middle reading, or does treating ambiguity-as-mechanism smuggle in an assumption of leadership omniscience/control that neither pure-revelation nor pure-coercion readings require?',
    'Cross-reading comparison: hold this story''s stakeholder and metric structure against the exogenous_override_reading and endogenous_reinterpretation_reading files and check whether the beneficiary/victim sets and ε values are genuinely distinguishable or converge under scrutiny.',
    'If the hybrid reading collapses into one of its siblings under closer analysis, this constraint should be retired or merged rather than maintained as a third distinct kernel reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the hybrid-pragmatic reading is structurally distinct from its two sibling readings or a blend that dissolves under analysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 1890, 1935).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1890, 0.35).
narrative_ontology:measurement(marr_tr_t1898, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1898, 0.42).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1904, 0.5).
narrative_ontology:measurement(marr_tr_t1912, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1912, 0.48).
narrative_ontology:measurement(marr_tr_t1920, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1920, 0.45).
narrative_ontology:measurement(marr_tr_t1935, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1935, 0.47).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1890, 0.4).
narrative_ontology:measurement(marr_be_t1898, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1898, 0.46).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1904, 0.55).
narrative_ontology:measurement(marr_be_t1912, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1912, 0.52).
narrative_ontology:measurement(marr_be_t1920, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1920, 0.5).
narrative_ontology:measurement(marr_be_t1935, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1935, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1890, 0.45).
narrative_ontology:measurement(marr_su_t1898, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1898, 0.55).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1904, 0.65).
narrative_ontology:measurement(marr_su_t1912, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1912, 0.6).
narrative_ontology:measurement(marr_su_t1920, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1920, 0.58).
narrative_ontology:measurement(marr_su_t1935, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1935, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__hybrid_pragmatic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.1).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the marriage_commitment_legitimacy kernel (the 1890 Manifesto). endogenous_reinterpretation_reading authors low extraction (genuine revelation, doctrine fully and sincerely changed, no structural victims beyond ordinary religious dissent costs). exogenous_override_reading authors high extraction concentrated on the institution itself as the target of federal coercion, with doctrine unchanged beneath a suspended practice. This hybrid_pragmatic_reading authors moderate extraction located specifically in the space between those poles — the ambiguity as mechanism — with leadership as beneficiary and rank-and-file members/dissenters as payers of interpretive uncertainty. All three share the same underlying historical kernel but diverge on ε, beneficiary/victim structure, and claimed_type because they instantiate structurally different claims about what the Manifesto IS, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
