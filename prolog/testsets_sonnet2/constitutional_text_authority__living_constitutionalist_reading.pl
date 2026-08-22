% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__living_constitutionalist_reading, []).

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
 *   constraint_id: constitutional_text_authority__living_constitutionalist_reading
 *   human_readable: Living Constitutionalist Reading of Constitutional Text Authority
 *   domain: constitutional_law/legal_theory
 *
 * SUMMARY:
 *   This story authors the living constitutionalist reading of the
 *   constitutional_text_authority kernel: the claim that constitutional
 *   meaning tracks evolving social attitudes and moral principles, so that
 *   authority for constitutional change flows not exclusively through Article
 *   V amendment but through judicial recognition that contemporary
 *   understanding has shifted. Brown v. Board is the canonical instance — a
 *   reinterpretation of the Fourteenth Amendment's equal protection guarantee
 *   that reversed Plessy without any formal amendment, on the theory that
 *   'separate but equal' could no longer be squared with contemporary
 *   understanding of equality even though the constitutional text had not
 *   changed. This reading is generated as its own ε-invariant constraint per
 *   Rule 1: it does not describe the originalist or positivist readings, does
 *   not average across them, and does not hedge its own extraction value
 *   against theirs. The sibling readings (originalist_reading,
 *   positivist_reading) are separate constraint files linked via
 *   network.affects_constraints, per the ε-invariance decomposition
 *   principle.
 *
 * KEY AGENTS:
 *   - judiciary_interpretive_authority: institutional agenda-setter and primary beneficiary of the interpretive discretion this reading grants
 *   - civil_rights_litigants: beneficiaries who depend on this reading as their only practical avenue for recognition absent formal amendment
 *   - legislative_branch_policy_primacy: payer whose enacted policy choices are displaced by judicial reinterpretation
 *   - losing_parties_in_contested_social_disputes: payers whose settled expectations under prior constitutional understanding are overturned without their participation
 *   - state_governments_preempted_by_evolving_doctrine: payers whose valid state law is preempted by a shift in federal constitutional doctrine
 *   - originalist_jurists: excluded voice within the same institution, unable to exit the practice, only to out-vote it across appointment cycles
 *   - constitutional_theorists: analytical observers of the legitimacy question
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__living_constitutionalist_reading, 0.42).
domain_priors:suppression_score(constitutional_text_authority__living_constitutionalist_reading, 0.38).
domain_priors:theater_ratio(constitutional_text_authority__living_constitutionalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__living_constitutionalist_reading, "Living Constitutionalist Reading of Constitutional Text Authority").
narrative_ontology:topic_domain(constitutional_text_authority__living_constitutionalist_reading, "constitutional_law/legal_theory").

domain_priors:requires_active_enforcement(constitutional_text_authority__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__living_constitutionalist_reading, 'aeee811b-5fd4-4294-b95b-cdcda8412985').
narrative_ontology:cs_kernel_codification('aeee811b-5fd4-4294-b95b-cdcda8412985', fixed_text).
narrative_ontology:cs_authority_grounding('aeee811b-5fd4-4294-b95b-cdcda8412985', practice).
narrative_ontology:cs_interpretation_layer_present('aeee811b-5fd4-4294-b95b-cdcda8412985').
narrative_ontology:cs_reading_relation('aeee811b-5fd4-4294-b95b-cdcda8412985', constitutional_text_authority__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('aeee811b-5fd4-4294-b95b-cdcda8412985', constitutional_text_authority__positivist_reading, influences).
narrative_ontology:cs_axiom('aeee811b-5fd4-4294-b95b-cdcda8412985', foundational, contemporary_moral_understanding_constitutes_constitutional_meaning).
narrative_ontology:cs_axiom_status(contemporary_moral_understanding_constitutes_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('aeee811b-5fd4-4294-b95b-cdcda8412985', contemporary_moral_understanding_constitutes_constitutional_meaning, deontological).
narrative_ontology:cs_axiom('aeee811b-5fd4-4294-b95b-cdcda8412985', secondary, judicial_recognition_of_evolved_meaning_is_legitimate_constitutional_change).
narrative_ontology:cs_axiom_status(judicial_recognition_of_evolved_meaning_is_legitimate_constitutional_change, holdable).
narrative_ontology:cs_axiom_grounding('aeee811b-5fd4-4294-b95b-cdcda8412985', judicial_recognition_of_evolved_meaning_is_legitimate_constitutional_change, instrumental).
narrative_ontology:cs_reference_frame('aeee811b-5fd4-4294-b95b-cdcda8412985', post_brown_evolving_standards_framework).
narrative_ontology:cs_drift_state('aeee811b-5fd4-4294-b95b-cdcda8412985', contemporary_originalist_resurgence_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('aeee811b-5fd4-4294-b95b-cdcda8412985', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, judiciary_interpretive_authority).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, marginalized_groups_seeking_recognition).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, civil_rights_litigants).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, legislative_branch_policy_primacy).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, losing_parties_in_contested_social_disputes).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, state_governments_preempted_by_evolving_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__living_constitutionalist_reading, brown_v_board_legitimacy).
narrative_ontology:constraint_vindicates(constitutional_text_authority__living_constitutionalist_reading, evolving_standards_of_decency_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__living_constitutionalist_reading, unenumerated_rights_recognition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal courts, especially the Supreme Court, hold the power to declare that constitutional meaning has shifted with contemporary moral understanding, and to strike down or reinterpret statutes and precedents on that basis. This authority is largely self-granted and self-policed; no external body can override a constitutional ruling except through amendment or the Court's own future reversal. The judiciary both administers this interpretive mode and is the primary institutional beneficiary of the discretion it grants.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, judiciary_interpretive_authority, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__living_constitutionalist_reading, judiciary_interpretive_authority, beneficiary).

% Groups seeking recognition of rights not enumerated in the original text (racial equality claimants in Brown, later movements invoking evolving-standards doctrine) depend entirely on courts reading contemporary values into old text, since the amendment process is practically foreclosed to them. They have no alternative path to vindication if living constitutionalism is abandoned in favor of fixed original meaning.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, civil_rights_litigants, beneficiary,
    moderate, generational, constrained, national).

% Congress and state legislatures find their enacted policy choices displaced when courts determine that evolving constitutional meaning forecloses a legislative option that was not foreclosed at ratification. They can respond by passing conforming legislation or pursuing amendment, but amendment is practically unavailable and conforming legislation concedes the judiciary's authority to have moved the boundary in the first place.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, legislative_branch_policy_primacy, payer,
    organized, generational, constrained, national).

% Parties on the losing side of a values-driven reinterpretation (traditionalist objectors to changes in family law, religious-liberty claimants whose prior settled expectations are displaced) experience the same constitutional text suddenly meaning something different than it did within their lifetime, with no vote and no notice beyond litigation outcomes they did not choose to enter.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, losing_parties_in_contested_social_disputes, payer,
    moderate, biographical, trapped, national).

% States that enacted policy consistent with prior constitutional understanding find that policy invalidated when the federal judiciary updates the governing interpretation, without any change to the state's own law or any national plebiscite on the underlying moral question.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, state_governments_preempted_by_evolving_doctrine, payer,
    organized, generational, constrained, regional).

% Jurists committed to fixed original public meaning argue that living constitutionalism substitutes judicial preference for legitimate constitutional change, but their objection operates within the same institution and cannot exit the practice — they can only outvote or outlast the interpretive mode when appointments shift, which is itself a demonstration of the mode's contingency rather than a genuine alternative channel.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, originalist_jurists, excluded,
    institutional, civilizational, constrained, national).

% Legal scholars analyze whether living constitutionalism constitutes legitimate constitutional development or judicial usurpation of the amendment power, without themselves being bound by or benefiting materially from either reading.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows a constitutional text drafted for one era to remain workable as social and moral understanding changes, without requiring the practically near-impossible supermajority consensus of Article V amendment for every needed adjustment.
% TRANSFER_FUNCTION: Moves the authority to determine the content of fundamental law from the amendment process (requiring broad political consensus) to the judiciary (requiring only five votes on the Supreme Court), and moves policy outcomes from legislatures' enacted choices to courts' contemporary-values determinations.
% ABSENT_VOICES: The public that would have voted on a formal amendment is never consulted on the specific substantive change; state legislatures whose laws are preempted have no vote on the reinterpretation; losing litigants in values-driven rulings had no notice that settled text could shift within their lifetime under a process they cannot appeal beyond the judiciary itself.
% DISAPPEARANCE_RATIONALE: If living constitutionalism were abandoned overnight in favor of strict fixed-meaning reading, dozens of doctrines recognizing unenumerated rights, evolving-standards protections, and post-ratification equality expansions would lose their interpretive foundation; legislatures and states would regain policy latitude currently foreclosed by evolving doctrine, and civil rights litigants would lose their primary avenue for judicial recognition absent formal amendment.
% FOUNDING_PROBLEM: The formal amendment process (Article V) is so difficult to invoke that a constitution reчитаемый only by its original meaning would ossify against centuries of moral, technological, and social change, leaving manifest injustices (e.g., segregation) without a constitutional remedy absent near-impossible supermajority consensus.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and comparative constitutionalists outside the sitting judiciary attest that formal amendment has in fact become vanishingly rare since the mid-20th century, corroborating the founding problem's continued relevance; originalist scholars and several state attorneys general attest from outside the beneficiary set that the problem is being used to justify ongoing judicial policymaking well past what textual staleness would require, arguing the founding problem has become a pretext rather than a live constraint.
narrative_ontology:disappearance_verdict(constitutional_text_authority__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__living_constitutionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text_authority__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__living_constitutionalist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__living_constitutionalist_reading_tests).
:- end_tests(constitutional_text_authority__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects that this reading transfers real policy-making authority from the amendment process and legislatures to the judiciary, but the transfer coexists with a genuine coordination function — constitutional text drafted in 1787-1868 remaining workable for governance two centuries later without requiring supermajority amendment for every adaptation. Suppression (0.38) is moderate: the reading does not physically coerce compliance, but its rulings bind nationally and preempt inconsistent state and federal law once announced, with no ordinary political remedy available to the losing side short of future judicial reversal or the practically unavailable amendment route. Theater ratio (0.28) is modest — the reinterpretation function is substantively real (rights are genuinely recognized or denied through this mechanism) rather than performative, though some 'evolving standards' invocations function partly as rhetorical cover for outcomes reachable on narrower grounds. Accessibility collapse (0.35) and resistance (0.62) reflect that this is a contested doctrine, not settled law: originalist and positivist alternatives remain fully live and are actively argued in every generation of appointments and major rulings, so alternatives have not collapsed the way a natural-law constraint's would.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is the structural agenda-setter and beneficiary: it both wields and is unconstrained in the exercise of the interpretive discretion this reading grants (analytical exit — it faces no external check besides amendment or self-reversal). Civil rights litigants and marginalized groups seeking recognition are beneficiaries with constrained exit — they have no alternative avenue to vindication if this reading is abandoned. Legislatures, preempted states, and losing parties in contested disputes are targets: their enacted or settled expectations are displaced by a reinterpretation they did not vote on and cannot appeal beyond the judiciary that produced it. Originalist jurists are excluded in a structurally unusual way — they hold institutional power but cannot exit the practice; they can only contest it from within, which is why they are marked excluded rather than payer despite holding real power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Article V's near-total unavailability leaving a fixed-meaning constitution unable to remedy manifest injustice — remains genuinely live for the beneficiary account (civil rights litigants still lack a practical amendment path), which is why founding_problem_status is authored as contested rather than dead: unlike a pure mandatrophy case, this reading's coordination function has not clearly outlived its necessity, but the payer seats (legislatures, states, originalist jurists) plausibly argue the doctrine is now invoked well beyond what textual staleness requires, extending into ordinary policy disputes that could be resolved through legislation. Classifying this as tangled_rope rather than snare or rope reflects that both the coordination story (constitutions must remain workable across centuries) and the extraction story (judicial policy-making displacing legislative and state authority without electoral accountability) are simultaneously true and load-bearing — this is precisely the profile a tangled rope, not a pure extraction mechanism, is meant to capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    living_constitutionalism_legitimate_development_or_usurpation,
    'Is judicial recognition of evolving constitutional meaning a legitimate mode of constitutional change coextensive with popular sovereignty, or is it an unaccountable transfer of the amendment power to an unelected judiciary?',
    'No empirical resolution exists; this is fundamentally a conceptual/normative dispute about the theory of constitutional legitimacy itself, resolvable only by which theory of democratic authorization one accepts — whether courts articulating contemporary consensus count as channeling popular will or displacing it.',
    'If living constitutionalism is legitimate constitutional development, this reading functions closer to genuine coordination (rope-leaning) with the extraction component understood as the ordinary cost of any interpretive authority. If it is usurpation, the extraction component dominates and the reading is better understood as a tangled_rope trending toward snare, with the coordination story functioning mainly as legitimating cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(living_constitutionalism_legitimate_development_or_usurpation, conceptual, 'Whether living constitutionalism is legitimate constitutional development or judicial usurpation of Article V authority — the central normative fault line of the kernel.').

omega_variable(
    kernel_reading_disagreement_location,
    'This constraint is one reading of the constitutional_text_authority kernel (sibling readings: originalist_reading, positivist_reading). Where exactly does the disagreement between readings sit — is it about what the text meant historically, about what makes constitutional meaning valid regardless of content, or about whether moral evolution can itself generate constitutional authority?',
    'The disagreement is located precisely at the source-of-authority question: this reading locates authority in the evolving application of enduring principles to changed circumstances; the originalist_reading locates it in fixed historical public meaning; the positivist_reading locates it in formal pedigree of enactment independent of moral content. A sibling reading would not merely re-decide the same cases differently — it would deny that contemporary moral consensus can itself be a source of constitutional authority (originalist) or would deny that moral content bears on validity at all (positivist).',
    'Under the originalist_reading, Brown v. Board would need to be justified on originalist grounds (e.g., that segregation was never consistent with the original public meaning of equal protection) rather than on evolving-standards grounds — a structurally different justification producing the same holding through a different beneficiary/victim architecture. Under the positivist_reading, the legitimacy question is bracketed entirely in favor of pedigree, changing which agents count as the relevant ''agenda_setter'' for validity purposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locating the precise structural disagreement among the three sibling readings of the constitutional_text_authority kernel.').

omega_variable(
    coordination_extraction_ratio_over_time,
    'Has the ratio of genuine coordination function (adapting genuinely stale text) to extraction function (judicial policy substitution) shifted over the 1954-2024 interval, and in which direction?',
    'Comparative analysis of the doctrinal basis invoked in major living-constitutionalist rulings across decades: rulings addressing textually silent, genuinely unanticipated circumstances (coordination-heavy) versus rulings overriding textually plausible legislative or state determinations on contested contemporary policy questions (extraction-heavy).',
    'A rising extraction share over time would support the T17 mountain-extraction-accumulation-style concern applied here to a tangled_rope: institutional entrenchment of interpretive discretion beyond what the founding problem requires. A stable or falling share would support the reading''s continued coordination legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_ratio_over_time, empirical, 'Whether the coordination-to-extraction ratio within living constitutionalist practice has shifted since 1954.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__living_constitutionalist_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1954, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 1954, 0.12).
narrative_ontology:measurement(cons_tr_t1968, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 1968, 0.15).
narrative_ontology:measurement(cons_tr_t1982, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 1982, 0.19).
narrative_ontology:measurement(cons_tr_t1996, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 1996, 0.22).
narrative_ontology:measurement(cons_tr_t2010, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(cons_tr_t2024, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t1954, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 1954, 0.22).
narrative_ontology:measurement(cons_be_t1968, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 1968, 0.28).
narrative_ontology:measurement(cons_be_t1982, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 1982, 0.33).
narrative_ontology:measurement(cons_be_t1996, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 1996, 0.36).
narrative_ontology:measurement(cons_be_t2010, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 2010, 0.39).
narrative_ontology:measurement(cons_be_t2024, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1954, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 1954, 0.25).
narrative_ontology:measurement(cons_su_t1968, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 1968, 0.29).
narrative_ontology:measurement(cons_su_t1982, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 1982, 0.32).
narrative_ontology:measurement(cons_su_t1996, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 1996, 0.34).
narrative_ontology:measurement(cons_su_t2010, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 2010, 0.36).
narrative_ontology:measurement(cons_su_t2024, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority__originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority__positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposed from the single natural-language label 'constitutional text authority' per the epsilon-invariance principle: this reading (living_constitutionalist, epsilon=0.42, tangled_rope), the originalist_reading (fixed historical meaning, expected lower epsilon, contested claimed type), and the positivist_reading (formal pedigree, morality-independent validity, expected distinct beneficiary structure). All three describe the same kernel — what grounds constitutional authority — but each instantiates a structurally distinct constraint with its own beneficiary/victim architecture and its own epsilon. They are linked here via affects_constraints rather than merged, because each reading's classification and metrics must remain independently authored and independently falsifiable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
