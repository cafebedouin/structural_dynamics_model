% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Constitutional Authority
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the judicial supremacy reading of a contested
 *   constitutional authority kernel: courts are the final, unchallengeable
 *   arbiters of constitutional meaning, with power to invalidate legislative
 *   and executive acts and no textual override mechanism available to the
 *   political branches. This is one of three structurally distinct readings
 *   of the same kernel text — coordinate construction (distributed
 *   interpretive authority across branches) and parliamentary primacy
 *   (legislative supremacy over constitutional meaning) are separate
 *   constraints with their own ε, beneficiaries, and stakeholders, linked
 *   here only through network.affects_constraints. This story's ε is authored
 *   solely for the judicial-supremacy arrangement as this reading's own
 *   lights see it: high extraction because the arrangement concentrates
 *   counter-majoritarian veto power in an unelected, effectively unreviewable
 *   body, with legislative and executive policy space treated as the paid
 *   cost.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, 0.66).
domain_priors:suppression_score(constitutional_authority_boundary__judicial_supremacy_reading, 0.71).
domain_priors:theater_ratio(constitutional_authority_boundary__judicial_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__judicial_supremacy_reading, "Judicial Supremacy Reading of Constitutional Authority").
narrative_ontology:topic_domain(constitutional_authority_boundary__judicial_supremacy_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__judicial_supremacy_reading, '3cf32aec-7754-4d7c-a75e-f437f95dc5b9').
narrative_ontology:cs_kernel_codification('3cf32aec-7754-4d7c-a75e-f437f95dc5b9', fixed_text).
narrative_ontology:cs_authority_grounding('3cf32aec-7754-4d7c-a75e-f437f95dc5b9', lineage).
narrative_ontology:cs_interpretation_layer_present('3cf32aec-7754-4d7c-a75e-f437f95dc5b9').
narrative_ontology:cs_reading_relation('3cf32aec-7754-4d7c-a75e-f437f95dc5b9', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('3cf32aec-7754-4d7c-a75e-f437f95dc5b9', constitutional_authority_boundary__parliamentary_primacy_reading, forecloses).
narrative_ontology:cs_axiom('3cf32aec-7754-4d7c-a75e-f437f95dc5b9', foundational, judicial_interpretation_is_final_and_unreviewable).
narrative_ontology:cs_axiom_status(judicial_interpretation_is_final_and_unreviewable, holdable).
narrative_ontology:cs_axiom_grounding('3cf32aec-7754-4d7c-a75e-f437f95dc5b9', judicial_interpretation_is_final_and_unreviewable, conventional).
narrative_ontology:cs_axiom('3cf32aec-7754-4d7c-a75e-f437f95dc5b9', secondary, countermajoritarian_review_is_necessary_check_on_transient_majorities).
narrative_ontology:cs_axiom_status(countermajoritarian_review_is_necessary_check_on_transient_majorities, holdable).
narrative_ontology:cs_axiom_grounding('3cf32aec-7754-4d7c-a75e-f437f95dc5b9', countermajoritarian_review_is_necessary_check_on_transient_majorities, instrumental).
narrative_ontology:cs_reference_frame('3cf32aec-7754-4d7c-a75e-f437f95dc5b9', founding_era_judicial_review_establishment).
narrative_ontology:cs_drift_state('3cf32aec-7754-4d7c-a75e-f437f95dc5b9', contemporary_polarized_appointments_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3cf32aec-7754-4d7c-a75e-f437f95dc5b9', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, apex_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_bar_specialists).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, elected_legislature).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, executive_policy_agenda).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, judicial_finality_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, countermajoritarian_check_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final say on what the constitutional text means, including the power to invalidate statutes and executive acts with no textual mechanism for override. Sets its own interpretive doctrine, controls its own docket and standing rules, and answers to no electoral cycle. Collects durable interpretive authority that compounds over time as precedent accumulates in its own favor.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, apex_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__judicial_supremacy_reading, apex_judiciary, beneficiary).

% A specialized profession of litigators, clerks, and academics whose expertise and livelihood depend on the court's role as final arbiter. Benefits from the complexity and durability of judicial doctrine; has no incentive to support alternative arrangements that would distribute interpretive authority elsewhere.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_bar_specialists, beneficiary,
    organized, generational, mobile, national).

% Passes legislation reflecting current majority will, but any statute may be invalidated by judicial constitutional interpretation with no ordinary-legislative override available. Can attempt constitutional amendment (a supermajority process, slow and often practically foreclosed) or court-packing/jurisdiction-stripping (norm-breaking, high political cost). Effectively operates within a policy space perimeter it does not control and cannot redraw through normal politics.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, elected_legislature, payer,
    powerful, biographical, constrained, national).

% Implements policy subject to judicial review of both legislative authorization and executive action itself. Cannot compel judicial deference and has no remedy against an adverse constitutional ruling beyond appointment influence over future vacancies, which operates on a multi-year lag and offers no recourse for the present dispute.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, executive_policy_agenda, payer,
    powerful, biographical, constrained, national).

% The voting public whose preferences are expressed through elected branches. Has no direct channel to contest a constitutional ruling; can only act indirectly through future elections that shape future appointments, a mechanism disconnected in time from any particular decision. Their considered policy preference can be permanently foreclosed by a single ruling with no legislative override.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, electoral_majorities, excluded,
    powerless, biographical, trapped, national).

% Study and debate the comparative merits of judicial supremacy against coordinate construction and parliamentary primacy. Do not hold formal power but shape the intellectual legitimacy of each reading through scholarship, judicial citation, and comparative constitutional analysis.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__judicial_supremacy_reading, apex_judiciary).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, and (in principle) politically insulated forum for resolving disputes about the meaning of foundational law, preventing each branch from simply asserting its own preferred interpretation and creating gridlock or open constitutional conflict.
% TRANSFER_FUNCTION: Moves final interpretive authority over the constitutional text — and thus practical control over the outer boundary of permissible legislative and executive action — from the elected branches to the judiciary, with no textual mechanism for the elected branches to reverse an adverse ruling.
% ABSENT_VOICES: Electoral majorities whose enacted preferences are invalidated have no direct forum to contest the ruling itself; their only lever is prospective and indirect (future appointments), which cannot undo or contest the specific decision. Legislatures acting collectively across a polity also lack a formal override channel once the text is read this way.
% DISAPPEARANCE_RATIONALE: If judicial finality vanished overnight, legislatures and executives would immediately face no textual barrier to re-enacting invalidated policy, the constitutional bar's premium expertise in litigating against an unappealable body would lose much of its value, and political conflicts currently resolved (or frozen) by court ruling would return to open political contestation — the entire practical boundary of majoritarian policymaking would shift.
% FOUNDING_PROBLEM: Early framers sought a mechanism to prevent transient legislative or executive majorities from permanently entrenching self-serving arrangements or violating minority and structural constitutional guarantees, and to provide a stable, apolitical-seeming forum for resolving disputes about the meaning of foundational law.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and the constitutional bar attest the problem remains live — durable protection against majoritarian overreach still requires an unchallengeable interpretive body. Legislative scholars, comparative constitutionalists studying jurisdictions with override mechanisms or coordinate construction, and political scientists studying counter-majoritarian drift attest from outside the judiciary that unreviewable finality has substantially outrun the founding problem, now functioning as durable policy-veto power exercised by an unelected body with no external corroboration required for legitimacy.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_authority_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 0.66, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.66) is high because the reading vests final interpretive authority in a body insulated from electoral correction, with no legislative override — the classic marker of a counter-majoritarian veto. Suppression (0.71) reflects that the arrangement's persistence depends on active enforcement of judicial finality against political-branch resistance (jurisdiction-stripping proposals, court-packing threats, non-compliance episodes) — this is not passive coordination but an actively defended boundary. Theater ratio is modest (0.28) and rising: the arrangement performs a genuine coordination function (resolving disputes about foundational law) but an increasing share of judicial activity manages its own legitimacy narrative rather than adjudicating disputes neutrally. Accessibility collapse (0.62) reflects that once judicial finality is established as the operative reading, alternative correction channels (constitutional amendment, legislative override) become practically foreclosed even though they remain nominally available. Resistance (0.58) captures recurring political-branch pushback (court-packing threats, jurisdiction-stripping bills, non-compliance episodes) that never fully displaces the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's own seat, judicial supremacy is simply the correct constitutional design — the text says what it says, and the court's role is definitional rather than extractive. From the legislature's and executive's seats, the identical structure operates as an unaccountable veto over democratically expressed preferences. The engine computes these divergent seat-level classifications from the declared power/exit/scope data; this story does not adjudicate between them — it authors the structural facts (who sets the agenda, who bears the cost, who is excluded) and lets per-seat computation surface the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The apex judiciary is the clear structural beneficiary: it sets its own interpretive doctrine, controls its own jurisdiction and standing rules, and accumulates durable authority immune to electoral correction — d sits near the full-beneficiary end. The constitutional bar benefits secondarily through the premium value of specialized expertise litigating before an unappealable body. The elected legislature and executive are targets: their policy space is bounded by rulings they cannot appeal, override, or timely correct — d sits near the full-target end, amplified by their formally powerful-but-constrained exit options (constitutional amendment is nominally available but practically near-foreclosed by supermajority thresholds). Electoral majorities are the most trapped party: their considered preferences, expressed through elected representatives, can be permanently vetoed with only an indirect, multi-year corrective channel (future appointments) available.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing transient majorities from entrenching self-serving arrangements or trampling structural guarantees — has not disappeared, which is why this reading is authored as tangled_rope rather than snare: there is a genuine, still-live coordination function (a stable forum for constitutional dispute resolution) bundled with the extraction. But the founding_problem_status is authored as contested, not dead, because reasonable outside observers dispute whether unreviewable finality (as opposed to some weaker form of judicial review with legislative response) is still proportionate to that founding problem, or has calcified into durable policy-veto power exercised without external correction. The tangled_rope classification, rather than mountain or rope, is the structural claim that this reading's coordination function is real but is bundled with asymmetric extraction that requires active enforcement to sustain against political-branch resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the constitutional text itself compel the judicial supremacy reading, or is this reading a contingent institutional development (e.g., a judiciary asserting final-arbiter status through its own precedent) that the text''s plain language does not require?',
    'Comparative constitutional analysis of jurisdictions with textually similar provisions that developed coordinate-construction or parliamentary-primacy practice instead; originalist and structural analysis of the founding-era debates over judicial review''s scope.',
    'If the text does not compel this reading, the judicial supremacy arrangement is better understood as an institutional accretion of power rather than a textually mandated structure, which would strengthen the case that its persistence depends on active defense (suppression) rather than textual necessity. If the text does compel it, some of the measured extraction would need to be re-attributed to the founding text itself rather than to institutional practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether judicial supremacy is textually compelled or institutionally constructed.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Within a single constitutional order, can the judicial supremacy reading and the parliamentary primacy reading both operate as live doctrine simultaneously, or does adopting one foreclose the other as a matter of institutional logic?',
    'Track whether any jurisdiction has operated with genuine ambiguity between the two readings for an extended period without a decisive institutional resolution (e.g., through a landmark case or a written override provision) — persistent ambiguity would suggest coexistence is structurally possible even if uncomfortable.',
    'If foreclosure is correct, judicial supremacy''s establishment in a given order structurally eliminates parliamentary primacy as a live option there, which is why this story marks that relation as forecloses rather than coexists_with in cs_structure. If coexistence is possible, the relation should be revised to influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether judicial supremacy and parliamentary primacy can coexist within one order or are mutually exclusive.').

omega_variable(
    counter_majoritarian_legitimacy_tradeoff,
    'Is the countermajoritarian check this reading provides (protection of minority rights and structural guarantees against transient majorities) worth its measured extraction cost (permanent policy foreclosure with no override), or does the specific value of that tradeoff depend on empirically contingent facts about how often courts protect minorities versus how often they block majoritarian reform?',
    'Longitudinal empirical study of a judicial-supremacy jurisdiction''s constitutional docket, coding rulings by whether they protected a minority/structural interest against majoritarian overreach or blocked majoritarian policy with no minority-protective rationale.',
    'A docket dominated by genuine minority-protective rulings would support treating a larger share of the arrangement as legitimate coordination (lower net ε); a docket dominated by policy-blocking rulings unrelated to minority protection would support the higher ε authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counter_majoritarian_legitimacy_tradeoff, empirical, 'Whether judicial supremacy''s countermajoritarian function is empirically vindicated or is cover for policy-blocking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__judicial_supremacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(cons_tr_t8, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(cons_tr_t16, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(cons_tr_t24, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(cons_tr_t32, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cons_be_t8, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(cons_be_t16, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(cons_be_t24, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 24, 0.59).
narrative_ontology:measurement(cons_be_t32, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 32, 0.63).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 40, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cons_su_t8, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(cons_su_t16, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(cons_su_t24, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(cons_su_t32, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'which branch has final constitutional authority' per the ε-invariance principle. judicial_supremacy_reading (this story, ε=0.66, tangled_rope) authors courts as beneficiary and elected branches as victim. coordinate_construction_reading authors distributed interpretive authority with no single final arbiter (expected lower ε, closer to rope). parliamentary_primacy_reading authors the legislature as final authority (expected different beneficiary/victim structure, likely rope or tangled_rope from the opposite direction). Each sibling has its own ε, its own stakeholders, and its own classification; they are linked here rather than merged into one measurement-dependent story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
