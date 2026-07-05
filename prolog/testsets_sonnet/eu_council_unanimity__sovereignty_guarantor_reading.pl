% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__sovereignty_guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__sovereignty_guarantor_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: eu_council_unanimity__sovereignty_guarantor_reading
 *   human_readable: Council Unanimity as Sovereignty Guarantee (Sovereignty Guarantor Reading)
 *   domain: institutional_design/international_relations
 *
 * SUMMARY:
 *   This story instantiates the sovereignty_guarantor_reading of the
 *   eu_council_unanimity kernel: unanimity in the Council of the EU on
 *   sovereignty-implicating matters (taxation, foreign and defense policy,
 *   treaty amendment, enlargement, own-resources) is read here as a
 *   rights-preserving structural guarantee rather than a vulnerability or a
 *   bargaining chip. Under this reading, the veto is a legitimate exercise of
 *   retained sovereignty, not an extraction mechanism, and the beneficiary
 *   set is the entire membership (particularly small and constitutionally
 *   cautious states) rather than any minoritarian actor capturing rents. This
 *   is a distinct constraint from the veto_trap_reading (which locates
 *   extraction in strategic blocking for side-payments) and the
 *   diplomatic_capital_reading (which locates the function in
 *   consensus-building legitimacy); each reading is authored as its own file
 *   with its own ε, per the ε-invariance principle, and linked via
 *   network.affects_constraints and cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - small_member_states: Primary beneficiary (moderate/constrained) — holds veto as sovereignty guarantee
 *   - constitutionally_sensitive_member_states: Primary beneficiary (moderate/constrained) — veto required by domestic constitutional order
 *   - large_member_states: Symmetric payer/beneficiary (powerful/constrained) — bears coordination cost, retains same guarantee
 *   - european_commission: Analytical observer (institutional/analytical) — designs proposals within the constraint
 *   - eu_citizens_of_would_be_majority_coalitions: Excluded (powerless/trapped) — no direct standing in the mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__sovereignty_guarantor_reading, 0.32).
domain_priors:suppression_score(eu_council_unanimity__sovereignty_guarantor_reading, 0.28).
domain_priors:theater_ratio(eu_council_unanimity__sovereignty_guarantor_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__sovereignty_guarantor_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__sovereignty_guarantor_reading, "Council Unanimity as Sovereignty Guarantee (Sovereignty Guarantor Reading)").
narrative_ontology:topic_domain(eu_council_unanimity__sovereignty_guarantor_reading, "institutional_design/international_relations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__sovereignty_guarantor_reading, '6fdb9eac-a745-4a44-a73f-3b1c8219251b').
narrative_ontology:cs_kernel_codification('6fdb9eac-a745-4a44-a73f-3b1c8219251b', formalized).
narrative_ontology:cs_authority_grounding('6fdb9eac-a745-4a44-a73f-3b1c8219251b', lineage).
narrative_ontology:cs_interpretation_layer_present('6fdb9eac-a745-4a44-a73f-3b1c8219251b').
narrative_ontology:cs_reading_relation('6fdb9eac-a745-4a44-a73f-3b1c8219251b', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('6fdb9eac-a745-4a44-a73f-3b1c8219251b', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('6fdb9eac-a745-4a44-a73f-3b1c8219251b', foundational, state_consent_is_precondition_for_sovereignty_pooling).
narrative_ontology:cs_axiom_status(state_consent_is_precondition_for_sovereignty_pooling, holdable).
narrative_ontology:cs_axiom_grounding('6fdb9eac-a745-4a44-a73f-3b1c8219251b', state_consent_is_precondition_for_sovereignty_pooling, deontological).
narrative_ontology:cs_axiom('6fdb9eac-a745-4a44-a73f-3b1c8219251b', foundational, veto_exercise_is_rights_exercise_not_extraction).
narrative_ontology:cs_axiom_status(veto_exercise_is_rights_exercise_not_extraction, holdable).
narrative_ontology:cs_axiom_grounding('6fdb9eac-a745-4a44-a73f-3b1c8219251b', veto_exercise_is_rights_exercise_not_extraction, conventional).
narrative_ontology:cs_reference_frame('6fdb9eac-a745-4a44-a73f-3b1c8219251b', treaty_founding_sovereignty_pooling_bargain).
narrative_ontology:cs_drift_state('6fdb9eac-a745-4a44-a73f-3b1c8219251b', post_lisbon_enlarged_union, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('6fdb9eac-a745-4a44-a73f-3b1c8219251b', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, constitutionally_sensitive_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states).
narrative_ontology:constraint_vindicates(eu_council_unanimity__sovereignty_guarantor_reading, state_consent_doctrine).
narrative_ontology:constraint_vindicates(eu_council_unanimity__sovereignty_guarantor_reading, sovereign_equality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a veto on unanimity-gated decisions (taxation, foreign policy, treaty change, own-resources) that lets them block outcomes a qualified-majority process could impose over their objection. This is their structural counterweight to population- and GDP-weighted voting in other Council procedures. Their exit from the arrangement would mean accepting majoritarian override on matters they consider core to sovereignty.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states, beneficiary,
    moderate, generational, constrained, continental).

% States whose domestic constitutional courts (e.g., on fiscal transfers, defense commitments, or treaty competence) require that any binding EU action affecting sovereignty receive explicit national consent. Unanimity gives these states a formal mechanism to ensure their constitutional order is not overridden by aggregate European preference.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, constitutionally_sensitive_member_states, beneficiary,
    moderate, generational, constrained, continental).

% Bear the coordination cost of needing to negotiate every unanimity-gated file to a form all 27 states can accept, which slows action and dilutes ambitious proposals. They also benefit from the same guarantee when their own sovereignty-sensitive interests are at stake, so the cost is symmetric across the membership rather than extracted by any single actor.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, payer,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, beneficiary).

% Proposes legislation and must design unanimity-gated initiatives (fiscal, foreign policy, treaty change) knowing any single state can block. Experiences the constraint as a design parameter shaping what is proposable, not as extraction directed at it specifically.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, european_commission, observer,
    institutional, generational, analytical, continental).

% Citizens of states forming a hypothetical qualified-majority coalition on a given file have no direct voice in the unanimity mechanism itself; their preference, even if shared by a large majority of the Union's population, cannot become binding EU law over a single dissenting state's objection on gated matters. They are not parties to Council votes at all — their interests are mediated entirely through their own government's veto or acquiescence.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens_of_would_be_majority_coalitions, excluded,
    powerless, biographical, trapped, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that collective EU action on matters implicating core state sovereignty (taxation, treaty change, foreign and defense policy, enlargement, own resources) proceeds only with the explicit consent of every member state, preventing a majority coalition from binding a dissenting sovereign state on matters it has not agreed to.
% TRANSFER_FUNCTION: No systematic transfer occurs under this reading: unanimity does not move resources or authority from one party to another. It withholds collective action absent universal consent, which is a rights-exercise (the power to decline), not an extraction mechanism. Coordination costs (delay, negotiation effort) are borne symmetrically by all states seeking action, not extracted asymmetrically by any veto-holder.
% ABSENT_VOICES: Citizens of would-be majority coalitions have no direct standing in the unanimity mechanism; their aggregate preference is not counted unless every government agrees. Under this reading, their absence is a feature, not a defect: the mechanism exists precisely to prevent aggregate preference from overriding individual state consent on sovereignty-implicating matters.
% DISAPPEARANCE_RATIONALE: If unanimity requirements were replaced overnight by qualified-majority voting across all currently-gated domains, smaller and constitutionally cautious states would lose their formal capacity to block collective action they consider incompatible with their sovereign interests; some would likely reduce engagement with or seek opt-outs from EU structures rather than accept binding majoritarian rule on core sovereignty questions. The current equilibrium of deep integration alongside preserved formal sovereignty depends on the guarantee remaining in place.
% FOUNDING_PROBLEM: The founding member states and successive accession states needed assurance that pooling sovereignty in some domains (market integration, agriculture, trade) would not become a slippery slope toward binding majoritarian control over domains they had not agreed to pool (taxation, foreign policy, treaty amendment, defense). Unanimity on these residual domains was the structural guarantee that made deeper integration in other domains politically acceptable.
% FOUNDING_PROBLEM_CORROBORATION: National constitutional courts, most prominently the German Federal Constitutional Court's Lisbon and OMT jurisprudence, have repeatedly held that certain sovereignty-sensitive competences (fiscal transfers, defense, treaty-amending power) require unanimous state consent as a condition of constitutional compatibility with EU membership — this is an attestation from outside the Council or Commission, from domestic judicial bodies whose institutional interest is constitutional fidelity, not EU institutional convenience.
narrative_ontology:disappearance_verdict(eu_council_unanimity__sovereignty_guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__sovereignty_guarantor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__sovereignty_guarantor_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eu_council_unanimity__sovereignty_guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).
:- end_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.32) reflecting genuine coordination cost (negotiation delay, diluted ambition) without systematic asymmetric extraction — no party captures rents from another party's veto under this reading. Suppression is low-moderate (0.28): the mechanism does not suppress alternatives so much as require universal buy-in, and states retain the option to negotiate, side-package, or eventually treaty-amend the scope of unanimity itself. Theater ratio is low (0.15) and only mildly rising, since the sovereignty-guarantee function has remained substantively operative (constitutional courts continue to invoke it) rather than becoming a performative residue. Accessibility collapse is moderate (0.45): once a state understands the mechanism, alternatives (majority voting) remain conceptually available and are actively debated in treaty reform discussions, so collapse is far from mountain-like completeness. Resistance is moderate (0.35): larger states and the Commission periodically campaign to narrow unanimity's scope, but this reading treats that campaigning as ordinary institutional contestation, not evidence of illegitimate extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, the derivation should place small and constitutionally sensitive states close to the beneficiary end of directionality — the constraint subsidizes their capacity to protect sovereignty-sensitive interests they could not otherwise defend given their limited size or bargaining power. Large states sit closer to symmetric: they pay the coordination cost of unanimity when it blocks their preferred fast action, but they retain and periodically exercise the exact same veto right when their own core interests are at stake, so no directional asymmetry of extraction runs against them specifically. The excluded citizen bloc is not assigned a victim declaration under this reading because the reading's core claim is that non-consent is a right, not a harm — their non-participation is definitional to the guarantee, not an extraction from them.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereignty_guarantor_reading resists mandatrophy mislabeling in both directions: it does not let genuine sovereignty-protection be recast as pure obstruction (the veto_trap_reading's frame), nor does it inflate ordinary coordination friction into performative theater requiring reform (theater_ratio is authored low and stable). The founding_problem is authored as status='live' precisely because domestic constitutional courts continue, in the present day, to treat unanimity as constitutionally required for certain competences — this is corroboration from outside the Council's own institutional interest, which distinguishes a genuinely persisting function from a legacy justification kept alive only by those who benefit from it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_use_legitimate_or_extractive,
    'Is a given exercise of the unanimity veto a legitimate defense of core sovereignty interests, or a strategic blocking threat deployed to extract side-payments or concessions unrelated to the substantive sovereignty concern?',
    'Case-by-case examination of whether the vetoing state''s stated sovereignty concern is substantively connected to the blocked measure, versus historical instances (e.g., linkage politics in enlargement or budget negotiations) where the veto was withdrawn upon receipt of an unrelated concession — the latter pattern would support the veto_trap_reading over this one for those instances.',
    'If most historical veto exercises show the linkage-and-concession pattern rather than substantive sovereignty defense, this reading''s claim that no systematic extraction occurs would be undermined for those cases, though the reading would remain valid for cases of genuine constitutional or sovereignty objection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_use_legitimate_or_extractive, empirical, 'Whether actual veto exercises match the sovereignty-defense frame or the extraction frame.').

omega_variable(
    beneficiary_set_completeness,
    'Does the unanimity guarantee actually benefit all small and constitutionally-sensitive states equally, or does it structurally favor states with more to lose from majoritarian override (larger economies with more fiscal exposure) over genuinely small states whose interests are rarely the ones triggering unanimity gates?',
    'Comparative analysis of which states have historically invoked or benefited from unanimity-gated protections versus which states'' sovereignty-sensitive interests have never been tested by a majoritarian threat.',
    'If the benefit is concentrated among a subset of states rather than genuinely universal, the ''all small states'' beneficiary framing in this reading would need narrowing, moving the story closer to a tangled_rope with an uneven beneficiary distribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_set_completeness, conceptual, 'Whether the declared universal beneficiary set is accurate or an idealization.').

omega_variable(
    reading_selection_dependence,
    'The choice to author this constraint under the sovereignty_guarantor_reading rather than the veto_trap_reading or diplomatic_capital_reading was guided by treating constitutional-court corroboration (Lisbon, OMT jurisprudence) as evidence of live, non-extractive function. Is this the correct interpretive anchor, or does it privilege one class of evidence (formal constitutional doctrine) over another (revealed strategic behavior in negotiations) that would support a different reading?',
    'Would require adjudicating whether constitutional-court doctrine or revealed negotiating behavior is the more authoritative signal of the mechanism''s actual function — this is itself a framing choice without a clean empirical resolution.',
    'If revealed negotiating behavior were treated as primary instead, the same underlying institutional fact (unanimity requirement) might be more defensibly classified under the veto_trap_reading for a substantial subset of historical cases, even though this story''s ε and beneficiary structure would remain internally coherent for the sovereignty-defense subset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_dependence, conceptual, 'Alternative framing (doctrinal vs. behavioral evidence) that could shift which reading best fits observed practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__sovereignty_guarantor_reading, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t1993, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(eu_c_tr_t1999, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 1999, 0.11).
narrative_ontology:measurement(eu_c_tr_t2005, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(eu_c_tr_t2012, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2012, 0.13).
narrative_ontology:measurement(eu_c_tr_t2018, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2018, 0.14).
narrative_ontology:measurement(eu_c_tr_t2024, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t1993, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 1993, 0.28).
narrative_ontology:measurement(eu_c_be_t1999, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 1999, 0.29).
narrative_ontology:measurement(eu_c_be_t2005, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2005, 0.3).
narrative_ontology:measurement(eu_c_be_t2012, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2012, 0.31).
narrative_ontology:measurement(eu_c_be_t2018, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2018, 0.31).
narrative_ontology:measurement(eu_c_be_t2024, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2024, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(eu_council_unanimity__sovereignty_guarantor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__sovereignty_guarantor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__sovereignty_guarantor_reading, 0.12).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity_veto_trap_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity_diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the eu_council_unanimity kernel, each authored as a separate constraint file with its own ε, beneficiary/victim structure, and claimed_type per the ε-invariance principle. sovereignty_guarantor_reading (this file) treats unanimity as rights-preserving non-extraction (moderate ε, rope). veto_trap_reading treats it as minoritarian extraction via credible blocking threats (high ε, snare or tangled_rope). diplomatic_capital_reading treats it as a consensus-forcing legitimacy mechanism (moderate ε, rope with different beneficiary emphasis). All three are linked bidirectionally via affects_constraints since they describe the same institutional kernel under different structural claims about what the veto actually does.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
