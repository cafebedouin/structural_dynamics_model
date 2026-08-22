% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__sovereignty_maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__sovereignty_maximalist_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__sovereignty_maximalist_reading
 *   human_readable: Sovereignty-Maximalist Reading of the RBIO Norm Complex
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates the sovereignty-maximalist reading of the RBIO
 *   norm complex kernel: state sovereignty is treated as near-absolute, RBIO
 *   norms are legitimate only insofar as they shield states from external
 *   interference, and humanitarian justifications for intervention are read
 *   as presumptively pretextual cover for regime change. This is not a claim
 *   about the RBIO complex in general — the liberal-institutional reading
 *   (consent-based, revisable multilateralism) and the hegemonic-extraction
 *   reading (frozen hegemonic project maintained by P5 veto) are separate
 *   constraints with separate ε values, linked here only by network edges.
 *   Under THIS reading's own lights, the standing arrangement (the
 *   non-intervention norm as currently invoked and enforced) is substantially
 *   extractive: it functions as a shield that incumbent regimes deploy
 *   against external accountability, at the direct expense of the populations
 *   trapped inside those regimes. The referent for ε is this standing
 *   arrangement as the reading itself evaluates it, not the reading's own
 *   preferred equilibrium (a world with zero pretextual intervention and full
 *   non-interference, which the reading would score as ε≈0).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.71).
domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.68).
domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__sovereignty_maximalist_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__sovereignty_maximalist_reading, "Sovereignty-Maximalist Reading of the RBIO Norm Complex").
narrative_ontology:topic_domain(rbio_practice_norm_complex__sovereignty_maximalist_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__sovereignty_maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__sovereignty_maximalist_reading, '183ac9ad-3d1c-464b-9a26-d8debb4b31ad').
narrative_ontology:cs_kernel_codification('183ac9ad-3d1c-464b-9a26-d8debb4b31ad', distributed).
narrative_ontology:cs_authority_grounding('183ac9ad-3d1c-464b-9a26-d8debb4b31ad', distributed).
narrative_ontology:cs_reading_relation('183ac9ad-3d1c-464b-9a26-d8debb4b31ad', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('183ac9ad-3d1c-464b-9a26-d8debb4b31ad', rbio_practice_norm_complex__hegemonic_extraction_reading, influences).
narrative_ontology:cs_axiom('183ac9ad-3d1c-464b-9a26-d8debb4b31ad', foundational, sovereignty_is_absolute_baseline).
narrative_ontology:cs_axiom_status(sovereignty_is_absolute_baseline, holdable).
narrative_ontology:cs_axiom_grounding('183ac9ad-3d1c-464b-9a26-d8debb4b31ad', sovereignty_is_absolute_baseline, conventional).
narrative_ontology:cs_axiom('183ac9ad-3d1c-464b-9a26-d8debb4b31ad', foundational, humanitarian_claims_presumptively_pretextual).
narrative_ontology:cs_axiom_status(humanitarian_claims_presumptively_pretextual, holdable).
narrative_ontology:cs_axiom_grounding('183ac9ad-3d1c-464b-9a26-d8debb4b31ad', humanitarian_claims_presumptively_pretextual, empirically_contingent).
narrative_ontology:cs_reference_frame('183ac9ad-3d1c-464b-9a26-d8debb4b31ad', westphalian_non_intervention_baseline).
narrative_ontology:cs_drift_state('183ac9ad-3d1c-464b-9a26-d8debb4b31ad', post_r2p_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('183ac9ad-3d1c-464b-9a26-d8debb4b31ad', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, incumbent_authoritarian_regimes).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, sovereignty_bloc_diplomats).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_governments).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, atrocity_survivors_seeking_intervention).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, besieged_civilian_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke absolute sovereignty and the non-intervention norm to block external scrutiny of internal repression, deploying the maximalist reading in UN forums to veto or dilute resolutions naming human rights abuses inside their borders. They set the diplomatic agenda within sovereignty-bloc coalitions (e.g. non-aligned voting blocs) and face no meaningful cost for invoking the norm since it is the reading their survival depends on.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, incumbent_authoritarian_regimes, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__sovereignty_maximalist_reading, incumbent_authoritarian_regimes, agenda_setter).

% Diplomats and legal advisors from states that benefit from a strict non-intervention norm coordinate voting blocs, draft resolution language emphasizing 'sovereign equality,' and litigate the boundaries of Article 2(4) in international forums. They can shift positions issue-by-issue depending on whether sovereignty protection or another interest is at stake.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, sovereignty_bloc_diplomats, agenda_setter,
    institutional, generational, mobile, global).

% Live under governments that cite sovereignty to block external human rights monitoring, refugee corridors, or protective intervention. They have no internal recourse (courts and elections captured by the same regime) and the maximalist reading forecloses the one external channel — international pressure or intervention — that might otherwise constrain their government. Exit is either impossible or requires becoming a refugee, which the same governments often restrict.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, populations_under_repressive_governments, payer,
    powerless, biographical, trapped, national).

% Communities experiencing mass atrocity (ethnic cleansing, genocide, systematic starvation as a weapon) whose only route to protection is external forces crossing the sovereignty line. Under this reading, any external actor invoking a 'responsibility to protect' claim is presumptively read as an agent of hidden regime-change intent, which delays or blocks the intervention regardless of the facts on the ground.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, atrocity_survivors_seeking_intervention, payer,
    powerless, immediate, trapped, regional).

% Civilians in active conflict zones where the incumbent government blocks humanitarian corridors or aid delivery, citing sovereign control over its own territory as grounds to reject externally administered relief. Their survival depends on aid the sovereignty norm, as read here, gives their own government veto power over.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, besieged_civilian_populations, payer,
    powerless, immediate, trapped, local).

% States and coalitions that argue humanitarian catastrophe should trigger external action are treated, within this reading, as presumptively acting in bad faith — their arguments are heard only to be categorically discounted as regime-change pretexts, regardless of case-specific merit. Their voice is structurally present in the debate but their claims are pre-foreclosed by the reading's core axiom.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, interventionist_powers, excluded,
    powerful, biographical, constrained, global).

% Adjudicates competing invocations of sovereignty and humanitarian exception in specific crises; its veto structure means the sovereignty-maximalist reading can be enforced unilaterally by any P5 member sympathetic to it, regardless of the Council's collective judgment on the merits of a given case.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, un_security_council, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__sovereignty_maximalist_reading, incumbent_authoritarian_regimes).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__sovereignty_maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable baseline rule — non-interference in internal affairs — that lets weaker and stronger states alike avoid a Hobbesian free-for-all in which any state could invoke humanitarian language to justify invasion of any other; this genuinely solves a coordination problem around the abuse of intervention doctrine.
% TRANSFER_FUNCTION: Moves protective leverage away from populations inside repressive states and toward the incumbent governments that rule them: the norm transfers the power to admit or refuse external scrutiny, aid, and protection entirely to the state apparatus being accused, regardless of that apparatus's own culpability.
% ABSENT_VOICES: The populations the norm is meant to shield from opportunistic intervention are the same populations who, under this reading, have no seat at the UN table and no channel to request protection that isn't itself discounted as evidence of foreign manipulation; interventionist powers are present but pre-delegitimized rather than genuinely excluded from the room.
% DISAPPEARANCE_RATIONALE: If the maximalist reading disappeared and a lower bar for humanitarian exception took its place, the calculus for both authoritarian regimes and would-be interveners would shift immediately: regimes would face real exposure to coordinated external pressure or action during mass-atrocity events, and interveners would face a correspondingly lower legal and rhetorical bar, changing crisis dynamics in places currently shielded by sovereignty invocation.
% FOUNDING_PROBLEM: The post-1945 order needed a rule against powerful states using pretextual justifications (moral, religious, civilizational) to invade weaker ones, a pattern that had underwritten centuries of colonial conquest and great-power war.
% FOUNDING_PROBLEM_CORROBORATION: Non-aligned movement historians and postcolonial legal scholars — a source base largely outside the incumbent regimes that most benefit from the reading today — corroborate that the anti-pretext concern was genuine and historically grounded; however, human rights monitors and atrocity-prevention researchers, also outside the beneficiary set, attest that the same rule is now invoked reflexively to block interventions with strong independent evidentiary bases, suggesting the founding problem's protective function has been substantially captured by incumbents it was never designed to shield.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__sovereignty_maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__sovereignty_maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.35 to 0.71) as the norm's core enforcement move — 'discount all humanitarian claims as regime-change pretexts' — is invoked with increasing frequency and increasing rhetorical sophistication by sovereignty-bloc coalitions, even as the number of documented cases of clearly bad-faith intervention pretexts (which motivated the norm originally) has not grown proportionally. Suppression is high (0.68) because the reading's enforcement mechanism is a diplomatic veto structure (UNSC, voting blocs) with real coercive teeth — it can and does block Council action outright. Theater ratio is moderate-rising (0.42) reflecting the growing gap between the norm's stated coordination function (preventing pretextual invasion) and its practiced function (blocking scrutiny of internal atrocities with strong independent evidence).
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent-regime seat, the reading is coherent and self-evidently protective: sovereignty is what stands between them and externally imposed regime change, full stop. From the payer seats — populations with no internal recourse — the same structure is the mechanism that guarantees no external recourse either. The engine should compute these as structurally different experiences of the same arrangement, not as a disagreement to be averaged.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent authoritarian regimes and the diplomats representing sovereignty-bloc coalitions are the structural beneficiaries: they invoke the reading, administer its application case-by-case, and bear essentially no cost from doing so. Populations under repressive governments, atrocity survivors, and besieged civilians are structural targets: the reading's core move — discounting external protective claims as pretextual — directly forecloses the one channel (external pressure or action) that could otherwise constrain the government harming them. Their exit options are trapped by construction: fleeing is itself often blocked by the same government the norm shields.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing great-power pretextual invasion under moral cover) was real and remains partially live — pretextual interventions still occur. But the reading's mechanism for solving that problem (blanket discounting of humanitarian claims) has outrun the problem's actual incidence: it now also blocks interventions with strong independent evidentiary support, which the founding problem never intended to protect against. This is exactly the mandatrophy pattern — a genuine coordination rule whose enforcement machinery has widened past its original justification, now serving incumbent power more than the anti-pretext principle it was built on.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_maximalist_kernel_reading_identity,
    'Is the sovereignty-maximalist reading a defensible independent interpretation of the RBIO norm complex''s founding text and practice, or is it itself a strategic reading adopted selectively by regimes for whom non-intervention is instrumentally convenient?',
    'Track whether states asserting the maximalist reading apply it consistently across cases where their own strategic interests point the opposite direction (e.g. do they invoke intervention rhetoric themselves when a rival state is the target). Consistency would support the reading as principled; inconsistency would support the instrumental-adoption hypothesis.',
    'If the reading is applied consistently regardless of the invoking state''s own interests, it strengthens the case for treating it as a genuine normative position with its own coherent axioms rather than a cover story; if applied only when convenient, it strengthens the hegemonic-extraction sibling''s account of selective enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_maximalist_kernel_reading_identity, empirical, 'Whether the maximalist reading is a principled position or an instrumentally deployed cover for regime self-protection.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does the sovereignty-maximalist reading''s core axiom (no legitimate intervention authority beyond self-defense) logically foreclose the liberal-institutional reading''s consent-based intervention authority, or can both be held by different parties simultaneously within the broader RBIO practice?',
    'Examine whether any single institutional framework (e.g. a specific UN resolution regime) has ever simultaneously accommodated both a consent-based intervention track and an absolute non-intervention floor without internal contradiction.',
    'If no single framework can hold both, the relationship is properly forecloses rather than coexists_with, which would change how the engine should treat cross-reading contamination in the network; the current authored relation (coexists_with) reflects that different state coalitions hold each position as live without either being logically eliminated globally.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether sovereignty-maximalist and liberal-institutional readings are logically incompatible within one framework or merely competing across different actors'' frameworks.').

omega_variable(
    pretext_base_rate_ambiguity,
    'What is the true base rate of humanitarian intervention claims that are, in fact, pretextual cover for regime change, versus claims with genuine humanitarian grounding that this reading nonetheless discounts?',
    'Historical case-coding of post-1945 interventions justified on humanitarian grounds, cross-referenced against post-hoc assessments of whether regime change was the primary strategic objective versus a secondary or absent one.',
    'A high pretext base rate would substantially strengthen this reading''s structural justification; a low one would suggest the reading''s blanket discounting mechanism is disproportionate to the problem it claims to solve, sharpening the mandatrophy reading of its current operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pretext_base_rate_ambiguity, empirical, 'The actual historical frequency of pretextual versus genuine humanitarian intervention claims, which this reading''s core mechanism assumes is high.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__sovereignty_maximalist_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(rbio_tr_t1960, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1960, 0.24).
narrative_ontology:measurement(rbio_tr_t1975, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(rbio_tr_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1990, 0.31).
narrative_ontology:measurement(rbio_tr_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(rbio_tr_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2015, 0.39).
narrative_ontology:measurement(rbio_tr_t2025, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(rbio_be_t1960, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1960, 0.42).
narrative_ontology:measurement(rbio_be_t1975, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement(rbio_be_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(rbio_be_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(rbio_be_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(rbio_be_t2025, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2025, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1945, 0.4).
narrative_ontology:measurement(rbio_su_t1960, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1960, 0.46).
narrative_ontology:measurement(rbio_su_t1975, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(rbio_su_t1990, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1990, 0.54).
narrative_ontology:measurement(rbio_su_t2005, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(rbio_su_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2015, 0.63).
narrative_ontology:measurement(rbio_su_t2025, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the rbio_practice_norm_complex kernel. The liberal_institutional_reading treats the same kernel text/practice as universal and consent-based with enforcement gaps as a capacity problem; the hegemonic_extraction_reading treats it as a frozen hegemonic project maintained by P5 veto with selective enforcement as evidence of extraction. This sovereignty_maximalist_reading treats it as legitimate only when protecting sovereignty against interference, with humanitarian exceptions read as regime-change pretexts. Each reading authors its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; none averages or hedges across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
