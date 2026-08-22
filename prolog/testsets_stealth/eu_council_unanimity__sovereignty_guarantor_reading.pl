% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__sovereignty_guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: eu_council_unanimity__sovereignty_guarantor_reading
 *   human_readable: Council Unanimity as Sovereignty Guarantor (Consent-Baseline Reading)
 *   domain: political/institutional/international-relations
 *
 * SUMMARY:
 *   This story instantiates the sovereignty_guarantor_reading of the EU
 *   Council unanimity rule: the treaty requirement that Council action in
 *   designated sovereignty-sensitive domains carries only with every member
 *   state's assent. On this reading the rule is a consent baseline erected
 *   after the 1965-66 empty-chair crisis — each government's refusal of
 *   assent is the exercise of a reserved right, the rule's beneficiaries are
 *   all member states (most consequentially the small ones whose votes would
 *   vanish under population weighting), and its costs are coordination costs
 *   rather than systematic transfers. The claim/metric gap is deliberate and
 *   structural: the constraint is CLAIMED as rope from this reading's seat
 *   while the authored metrics describe moderate, slowly accumulating
 *   coordination burden — the engine measures the divergence; this file does
 *   not reconcile them. KEY AGENTS (by structural relationship): -
 *   small_member_states: primary protected party (organized/constrained) —
 *   formal vote equality is their shield against demographic weight -
 *   mid_sized_member_states: protected party with episodic blocking
 *   experience (organized/constrained) - large_member_states: dual-positioned
 *   — most often the proposer waiting on others' assent, occasionally the
 *   isolated state the rule shields (powerful/constrained) -
 *   european_commission: agenda proposer bearing stalled-initiative costs
 *   (institutional/constrained) - eu_accession_candidates: affected
 *   non-members with no Council seat, held at bilateral disputes' mercy
 *   (powerless/trapped) - constitutional_scholars_and_treaty_lawyers:
 *   analytical observers tracking the rule's function across treaty
 *   generations (analytical/analytical)
 *
 * KEY AGENTS:
 *   - small_member_states: primary protected party (organized/constrained) — formal vote equality shields them from demographic weight
 *   - mid_sized_member_states: protected party with episodic blocking experience (organized/constrained)
 *   - large_member_states: dual-positioned proposer-and-shielded party (powerful/constrained)
 *   - european_commission: agenda proposer bearing stalled-initiative costs (institutional/constrained)
 *   - eu_accession_candidates: affected non-members excluded from the Council (powerless/trapped)
 *   - constitutional_scholars_and_treaty_lawyers: analytical observers (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__sovereignty_guarantor_reading, 0.35).
domain_priors:suppression_score(eu_council_unanimity__sovereignty_guarantor_reading, 0.3).
domain_priors:theater_ratio(eu_council_unanimity__sovereignty_guarantor_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__sovereignty_guarantor_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__sovereignty_guarantor_reading, "Council Unanimity as Sovereignty Guarantor (Consent-Baseline Reading)").
narrative_ontology:topic_domain(eu_council_unanimity__sovereignty_guarantor_reading, "political/institutional/international-relations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__sovereignty_guarantor_reading, '8553e423-a878-4504-b53a-4d1717c40a39').
narrative_ontology:cs_kernel_codification('8553e423-a878-4504-b53a-4d1717c40a39', fixed_text).
narrative_ontology:cs_authority_grounding('8553e423-a878-4504-b53a-4d1717c40a39', lineage).
narrative_ontology:cs_interpretation_layer_present('8553e423-a878-4504-b53a-4d1717c40a39').
narrative_ontology:cs_reading_relation('8553e423-a878-4504-b53a-4d1717c40a39', eu_council_unanimity__veto_trap_reading, forecloses).
narrative_ontology:cs_reading_relation('8553e423-a878-4504-b53a-4d1717c40a39', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('8553e423-a878-4504-b53a-4d1717c40a39', foundational, sovereignty_implicating_action_requires_universal_consent).
narrative_ontology:cs_axiom_status(sovereignty_implicating_action_requires_universal_consent, holdable).
narrative_ontology:cs_axiom_grounding('8553e423-a878-4504-b53a-4d1717c40a39', sovereignty_implicating_action_requires_universal_consent, deontological).
narrative_ontology:cs_axiom('8553e423-a878-4504-b53a-4d1717c40a39', secondary, withheld_consent_is_legitimate_rights_defense).
narrative_ontology:cs_axiom_status(withheld_consent_is_legitimate_rights_defense, holdable).
narrative_ontology:cs_axiom_grounding('8553e423-a878-4504-b53a-4d1717c40a39', withheld_consent_is_legitimate_rights_defense, deontological).
narrative_ontology:cs_reference_frame('8553e423-a878-4504-b53a-4d1717c40a39', luxembourg_compromise_consent_baseline).
narrative_ontology:cs_drift_state('8553e423-a878-4504-b53a-4d1717c40a39', contemporary_enlarged_union_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8553e423-a878-4504-b53a-4d1717c40a39', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, mid_sized_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, european_commission).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, eu_accession_candidates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold one vote each in the Council and can refuse assent to any measure in the files where unanimity applies. Their combined populations would rarely form a winning coalition under population-weighted voting, so the consent requirement is the principal instrument that keeps their written positions binding in negotiation. Leaving the Union, or the rule, would mean accepting binding decisions taken without their assent in the covered domains; remaining inside is the default path.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states, beneficiary,
    organized, generational, constrained, continental).

% Cast one vote each and have refused assent episodically — on budget files, sanctions renewals, and enlargement steps — usually to obtain wording changes or bilateral assurances rather than to stop files outright. They alternate between defending the consent requirement as their shield and chafing at it when their own initiatives stall awaiting a partner's assent.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, mid_sized_member_states, beneficiary,
    organized, generational, constrained, continental).

% Can usually assemble winning coalitions under population-weighted voting, so the consent requirement binds them most often as proposers whose initiatives wait on reluctant partners; the same rule shields them whenever they find themselves isolated, as when their positions on sanctions, migration, or rule-of-law files diverge from the rest. They also finance the largest shares of the common budget that many unanimous files allocate.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, payer).

% Proposes legislation and manages the Union's work programme, but in unanimity files its proposal is a starting bid: the Council can amend by common accord and the Parliament is often limited to consultation. Multi-year programmes routinely carry items that stall or lapse in the Council; the institution argues in treaty-revision debates for wider use of majority voting, which would enlarge its agenda control.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, european_commission, payer,
    institutional, generational, constrained, continental).

% Need the assent of every existing member state at each step of accession — opening and closing negotiating clusters, and final ratification. Bilateral disputes between existing members have held candidacies in place for years irrespective of the candidate's own progress. They hold no seat in the Council and no vote on the terms; their recourse is diplomacy aimed at individual capitals.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_accession_candidates, excluded,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__sovereignty_guarantor_reading, eu_accession_candidates, payer).

% Trace the consent requirement's origins in the 1965-66 empty-chair crisis and the Luxembourg Compromise, track its shrinking domain across successive treaty revisions, and publish assessments of whether it still serves its original purpose. They hold no vote and bear no direct cost; their analyses circulate through ministries, courts, and reform conferences.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, constitutional_scholars_and_treaty_lawyers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__sovereignty_guarantor_reading, diffuse).
narrative_ontology:fixing_cost_class(eu_council_unanimity__sovereignty_guarantor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a consent baseline for collective action in sovereignty-sensitive domains: no member state is bound by measures it did not accept, which keeps integration reversible at the margin for each government and sustains capitals' willingness to delegate into the Union at all.
% TRANSFER_FUNCTION: Moves absolute negative decision power to every member state equally — each government gains a veto regardless of size — and, in operation, moves negotiation time and concession value from proposing coalitions to reluctant minorities during the search for common accord.
% ABSENT_VOICES: Accession candidates and other non-members affected by unanimous decisions have no seat and no vote; citizens of member states are represented only indirectly through their governments; the European Parliament is confined to consultation in many covered files. All three would press for weighted voices or safeguards if admitted to the conversation.
% DISAPPEARANCE_RATIONALE: If the consent requirement vanished overnight, every unanimity-domain file would move to qualified majority: small and mid-sized states would lose their absolute negative voice, currently blocked files would pass over sitting objectors, accession steps would no longer wait on bilateral member disputes, and governments facing domestic sovereignty constraints would confront binding obligations they had refused — a substantial rearrangement of bargaining power inside the Union.
% FOUNDING_PROBLEM: The 1965-66 empty-chair crisis: preventing integration from proceeding over the expressed objection of member governments whose publics had not consented — securing national consent as the legitimacy floor of a deepening common enterprise.
% FOUNDING_PROBLEM_CORROBORATION: Academic and archival histories of the empty-chair crisis and the Luxembourg Compromise, produced independently of any member government, attest the founding problem and its context. The Commission's recurring proposals to extend majority voting attest, from an institutional seat outside the beneficiary set, its judgment that the problem no longer justifies the rule's cost in many domains. Accession-candidate diplomacy attests the cost side. No source outside the beneficiary set attests that the problem is fully dead — rule-of-law and foreign-policy divisions keep its status arguable.
narrative_ontology:disappearance_verdict(eu_council_unanimity__sovereignty_guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__sovereignty_guarantor_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__sovereignty_guarantor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eu_council_unanimity__sovereignty_guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 0.35, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored moderate (0.35 at interval end) because this reading identifies no systematic transfer: the rule moves no revenue and imposes no tribute; its costs are delay, holdout leverage, and the hostage effect on accession candidates — real but coordination-class burdens. Suppression is authored 0.30: the rule constrains majority action and narrows the Commission's agenda control in covered files, but it operates by default (inaction absent common accord) rather than by enforcement machinery, hence requires_active_enforcement is false. Theater is low (0.18): refusals of assent are substantively consequential — files die, wording changes, candidacies wait — so the function is not performed emptily; the slow rise across the series reflects pre-negotiated consensus culture reducing open veto moments while the underlying threat remains functional. Accessibility_collapse is 0.50: alternatives (majority voting in adjacent domains, enhanced cooperation, ad hoc intergovernmental coalitions) remain partially available once the rule is understood. Resistance is 0.55: sustained institutional pressure to widen majority voting — treaty revisions, passerelle debates, reform-conference recommendations — meets the rule continuously. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled downstream by directionality and scope. All three tracked metric series run on one shared time grid (1966, 1979, 1992, 2004, 2014, 2026) so every metric is authored at every examined point; endpoint values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   From the sovereignty-guarantor seat, a state refusing assent exercises a right and the waiting majority pays the legitimate price of consent-based legitimacy. From the seat of a blocked majority coalition — the veto_trap reading's vantage — the identical episode is a smaller actor taxing the many through a credible threat. From the diplomatic_capital seat, the same negotiation is legitimacy-producing iteration that improves the output. The engine computes these per-seat divergences from the structural data; this file supplies only the sovereignty-guarantor instantiation's data and does not adjudicate the contest.
 *
 * DIRECTIONALITY LOGIC:
 *   All three member-state seats derive low directionality from the beneficiary declarations — the rule subsidizes each with an absolute negative voice. The large_member_states seat carries an explicit override to d=0.40: derivation from beneficiary status alone would place them near the beneficiary pole, but structurally they sit nearer symmetric because they propose most files in the covered domains and therefore wait on others' assent far more often than they invoke protection, while financing the largest budget shares that unanimous files allocate. The Commission (payer) and the accession candidates (excluded) derive high directionality; their burdens are real but, on this reading's account, coordination-side rather than transfer-side. The moderate aggregate epsilon reflects that mix: a broad low-d beneficiary mass dominating a narrow high-d cost-bearing fringe.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying from this reading guards against two opposite errors. Reading veto episodes as extraction would collapse the protection into an extraction narrative and erase the consent function that keeps delegating governments willing to stay inside the enterprise; reading the rule as costless coordination would erase the real burdens on proposers, the Commission's programme, and non-member candidates. The omega variables hold the questions that would move the classification: if the veto-episode audit shows extraction-dominated use, the computed seat classification should migrate toward hybrid or extraction types notwithstanding this reading's rope claim; if counterfactual analysis shows the protection rarely binds, the coordination justification thins toward the sibling readings' accounts. The receipt surface records the facts this reading establishes: no named seat captures the rule's gains (they accrue to all member states equally — an affirmative checked claim), and fixing is prohibitive because altering the rule requires the unanimous assent it governs. Unlike an inertially maintained vestige, however, the diffuse gains here are actively defended and substantively exercised — the piton-shaped cell combination is a fact pattern the engine weighs, not a conclusion this file draws.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which of the three readings of the eu_council_unanimity kernel characterizes the rule''s dominant function — sovereign-consent guarantee (this file), minoritarian extraction channel (veto_trap_reading), or legitimacy-manufacturing iteration (diplomatic_capital_reading)?',
    'Systematic coding of veto and blocking episodes across the interval (sovereignty-defense versus concession-extraction versus consensus-building) combined with counterfactual comparison of outcomes under qualified-majority weights.',
    'If extraction-coded episodes dominate, the seat classification migrates toward hybrid or extraction types and the victim set expands; if consensus-building dominates, the rope classification survives but the beneficiary emphasis shifts to negotiation-quality gains. Each outcome corresponds to adopting a sibling reading as the operative constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This constraint is one reading of the eu_council_unanimity kernel; the sibling readings instantiate structurally different constraints with different epsilon values.').

omega_variable(
    veto_episode_function_audit,
    'In recent blocking episodes, what share defends a position genuinely linked to state sovereignty versus extracts bilateral concessions unrelated to it?',
    'Case-level audit with coded criteria: sovereignty linkage of the issue domain, the nature of the concession sought, and the duration of the block relative to the underlying file.',
    'A high extraction-coded share would raise epsilon above the moderate band and move the computed seat classification away from this reading''s rope claim, even though the rule''s text is unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_episode_function_audit, empirical, 'Whether observed veto use matches the rights-exercise characterization this reading assigns it.').

omega_variable(
    accession_candidate_cost_status,
    'Do the costs borne by accession candidates held up by member bilateral disputes count as extraction by the rule, or as the legitimate price of member consent over enlargement?',
    'Conceptual resolution: classify enlargement assent as internal sovereignty exercise (costs are coordination-side) or as fiduciary gatekeeping over third parties (costs are extraction-side borne by non-consenting outsiders).',
    'Counting candidate costs as extraction expands the victim set and raises epsilon; excluding them preserves the moderate profile. The classification of this reading''s constraint turns on the answer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accession_candidate_cost_status, conceptual, 'Boundary question: whose costs belong inside the rule''s extraction ledger.').

omega_variable(
    counterfactual_protection_frequency,
    'How often does the consent requirement actually change outcomes for a state that would have lost under qualified-majority voting?',
    'Reconstruct Council positions under QMV weights for unanimity-domain files across the interval and compare adopted outcomes with the counterfactual majority outcome.',
    'Rare protective effect shifts evidentiary weight toward the sibling readings'' accounts and weakens this reading''s coordination justification; frequent protective effect anchors the rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_protection_frequency, empirical, 'Frequency with which the rule''s protection binds in practice rather than in principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__sovereignty_guarantor_reading, 1966, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_unanimity_sg_tr_t1966, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 1966, 0.1).
narrative_ontology:measurement(eu_unanimity_sg_tr_t1979, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 1979, 0.11).
narrative_ontology:measurement(eu_unanimity_sg_tr_t1992, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 1992, 0.12).
narrative_ontology:measurement(eu_unanimity_sg_tr_t2004, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2004, 0.14).
narrative_ontology:measurement(eu_unanimity_sg_tr_t2014, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2014, 0.16).
narrative_ontology:measurement(eu_unanimity_sg_tr_t2026, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2026, 0.18).

% Extraction over time
narrative_ontology:measurement(eu_unanimity_sg_be_t1966, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 1966, 0.2).
narrative_ontology:measurement(eu_unanimity_sg_be_t1979, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 1979, 0.24).
narrative_ontology:measurement(eu_unanimity_sg_be_t1992, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 1992, 0.28).
narrative_ontology:measurement(eu_unanimity_sg_be_t2004, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2004, 0.31).
narrative_ontology:measurement(eu_unanimity_sg_be_t2014, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2014, 0.34).
narrative_ontology:measurement(eu_unanimity_sg_be_t2026, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2026, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(eu_unanimity_sg_su_t1966, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 1966, 0.28).
narrative_ontology:measurement(eu_unanimity_sg_su_t1979, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 1979, 0.28).
narrative_ontology:measurement(eu_unanimity_sg_su_t1992, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 1992, 0.29).
narrative_ontology:measurement(eu_unanimity_sg_su_t2004, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2004, 0.29).
narrative_ontology:measurement(eu_unanimity_sg_su_t2014, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2014, 0.3).
narrative_ontology:measurement(eu_unanimity_sg_su_t2026, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2026, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__sovereignty_guarantor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__veto_trap_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'EU Council unanimity' covers three structurally distinct claims about one treaty rule: that it guarantees sovereign consent (this file), that it enables minoritarian extraction (eu_council_unanimity__veto_trap_reading), and that it manufactures legitimacy through forced iteration (eu_council_unanimity__diplomatic_capital_reading). Per the epsilon-invariance principle each is authored as a separate constraint with its own epsilon, beneficiary/victim structure, and claimed type; the epsilon values differ because each reading assesses the same standing arrangement by different lights. Family links connect all three members.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_council_unanimity__sovereignty_guarantor_reading, powerful, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
