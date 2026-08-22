% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__advisory_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__advisory_coordination_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: wto_dsb_authority__advisory_coordination_reading
 *   human_readable: WTO DSB as Advisory Coordination Mechanism
 *   domain: international_law/trade_governance
 *
 * SUMMARY:
 *   This story authors the advisory-coordination reading of the WTO Dispute
 *   Settlement Body kernel: DSB panels are treated as a mechanism that
 *   produces expert, informationally valuable opinions that facilitate
 *   negotiated settlement between disputing member states, while ultimate
 *   policy discretion remains with the state. This is one of three
 *   structurally distinct readings of the same underlying institution — the
 *   treaty text, the Appellate Body's own historical self-description, and
 *   the compliance mechanics (retaliation authorization, sequencing
 *   procedures) are read very differently by the binding_referee_reading
 *   (which treats the DSU's obligatory language as creating genuine legal
 *   compliance duties) and the judicial_activism_reading (which treats panel
 *   and Appellate Body interpretive practice as exceeding the treaty
 *   mandate). Per the ε-invariance principle, these are authored as three
 *   separate constraint files rather than one story with a measurement
 *   parameter, because the extraction, suppression, and enforcement profiles
 *   genuinely differ across the readings. This file's ε (0.28) reflects a
 *   low-extraction, low-suppression reading: rulings are treated as
 *   diagnostic inputs to bargaining rather than obligations backed by
 *   coercive machinery, so most of what would count as extraction under a
 *   binding-referee reading (asymmetric compliance costs, forced concessions)
 *   simply does not register as extraction here, because the reading denies
 *   the underlying obligation exists in the first place.
 *
 * KEY AGENTS:
 *   - trading_member_states: Primary beneficiary (institutional/constrained) — uses DSB opinions as negotiation leverage
 *   - less_powerful_member_states: Structurally disadvantaged beneficiary/payer (moderate/trapped) — gets the same diagnostic but lacks leverage to convert it into a remedy
 *   - powerful_member_states: Primary beneficiary (powerful/arbitrage) — absorbs adverse rulings costlessly, weaponizes favorable ones
 *   - wto_secretariat: Agenda-setter (institutional/analytical) — administers the process and has an institutional stake in the non-coercive framing
 *   - trade_law_scholars: Analytical observer — documents the gap between treaty text and observed compliance behavior
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__advisory_coordination_reading, 0.28).
domain_priors:suppression_score(wto_dsb_authority__advisory_coordination_reading, 0.15).
domain_priors:theater_ratio(wto_dsb_authority__advisory_coordination_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__advisory_coordination_reading, rope).
narrative_ontology:human_readable(wto_dsb_authority__advisory_coordination_reading, "WTO DSB as Advisory Coordination Mechanism").
narrative_ontology:topic_domain(wto_dsb_authority__advisory_coordination_reading, "international_law/trade_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__advisory_coordination_reading, '7c1e04b1-cd6f-4852-b366-b2bb77d8fe95').
narrative_ontology:cs_kernel_codification('7c1e04b1-cd6f-4852-b366-b2bb77d8fe95', fixed_text).
narrative_ontology:cs_authority_grounding('7c1e04b1-cd6f-4852-b366-b2bb77d8fe95', distributed).
narrative_ontology:cs_reading_relation('7c1e04b1-cd6f-4852-b366-b2bb77d8fe95', wto_dsb_authority__binding_referee_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c1e04b1-cd6f-4852-b366-b2bb77d8fe95', wto_dsb_authority__judicial_activism_reading, influences).
narrative_ontology:cs_axiom('7c1e04b1-cd6f-4852-b366-b2bb77d8fe95', foundational, sovereign_discretion_survives_adjudication).
narrative_ontology:cs_axiom_status(sovereign_discretion_survives_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('7c1e04b1-cd6f-4852-b366-b2bb77d8fe95', sovereign_discretion_survives_adjudication, conventional).
narrative_ontology:cs_axiom('7c1e04b1-cd6f-4852-b366-b2bb77d8fe95', secondary, compliance_is_bargained_not_compelled).
narrative_ontology:cs_axiom_status(compliance_is_bargained_not_compelled, holdable).
narrative_ontology:cs_axiom_grounding('7c1e04b1-cd6f-4852-b366-b2bb77d8fe95', compliance_is_bargained_not_compelled, empirically_contingent).
narrative_ontology:cs_reference_frame('7c1e04b1-cd6f-4852-b366-b2bb77d8fe95', gatt_era_diplomatic_dispute_resolution).
narrative_ontology:cs_drift_state('7c1e04b1-cd6f-4852-b366-b2bb77d8fe95', post_appellate_body_jurisprudence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7c1e04b1-cd6f-4852-b366-b2bb77d8fe95', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, trading_member_states).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, export_dependent_industries).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, wto_secretariat).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, less_powerful_member_states).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, powerful_member_states).
narrative_ontology:constraint_victim(wto_dsb_authority__advisory_coordination_reading, less_powerful_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bring disputes to the DSB to get an expert, neutral read on whether a trading partner's measure violates negotiated commitments. Uses the panel report as leverage and information in a subsequent negotiation, but is never legally compelled by domestic constitutional order to change the underlying measure — retaliation authorization and implementation remain subject to further political bargaining.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, trading_member_states, beneficiary,
    institutional, generational, constrained, global).

% Rely on their government using DSB findings as a credible, technically-grounded bargaining chip to reopen market access. Benefit when a favorable ruling shifts the negotiating equilibrium, but have no independent standing to compel compliance themselves.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, export_dependent_industries, beneficiary,
    organized, biographical, constrained, global).

% Administers the panel process, appoints experts, and publishes reports. Frames its function as facilitating information exchange between disputing parties, not as adjudicating with binding force. Has an institutional interest in this reading because it preserves participation from powerful members who would exit an arrangement they experienced as coercive.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, wto_secretariat, agenda_setter,
    institutional, civilizational, analytical, global).

% Obtain the same advisory opinion as a powerful state but lack the bilateral leverage to convert a favorable ruling into an actual settlement — the coordination benefit (expert diagnosis) is real, but converting it into a remedy depends on power they don't have. Under this reading they bear no formal cost from a ruling against them either, since compliance is discretionary for everyone.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, less_powerful_member_states, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__advisory_coordination_reading, less_powerful_member_states, beneficiary).

% Can absorb an adverse ruling with minimal consequence, since implementation is a matter of choice rather than obligation, while using favorable rulings as diplomatic ammunition against weaker trading partners. This reading is most congenial to their position because it keeps enforcement tethered to the bilateral power balance they already dominate.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, powerful_member_states, beneficiary,
    powerful, generational, arbitrage, global).

% Study the gap between the DSU's treaty text (which uses obligatory language) and the observed pattern of non-implementation, partial implementation, and negotiated settlement in lieu of formal compliance. Some read this pattern as evidence the advisory-coordination reading is descriptively accurate regardless of what the treaty text says.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, trade_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a technically expert, neutral diagnosis of whether a challenged trade measure is consistent with negotiated commitments, giving disputing states a common factual and legal baseline from which to negotiate a settlement rather than escalating unilaterally.
% TRANSFER_FUNCTION: Moves information and diagnostic legitimacy from the panel process to the disputing parties; does not itself move compliance, market access, or policy change — those remain to be negotiated bilaterally using the ruling as an input.
% ABSENT_VOICES: Domestic constituencies harmed by a measure a panel found inconsistent (e.g., consumers facing tariffs found unjustified) have no standing in the dispute and depend entirely on their government electing to use the ruling as negotiating leverage; if the government has no bilateral power to press the point, their interest is never advanced regardless of the ruling's merits.
% DISAPPEARANCE_RATIONALE: Advocates of this reading argue that if the DSB vanished, states would simply negotiate directly using diplomatic channels and existing power asymmetries — little would change because the panel process is not what drives settlement, bilateral leverage is. Critics (holders of the binding_referee_reading) argue the DSB's absence would remove the last common evidentiary baseline and accelerate unilateral retaliation, so the world would rearrange substantially. The story itself, authored from the advisory-coordination seat, treats world_unchanged as the more defensible reading-internal answer, but the field is marked contested to reflect that this is a live dispute between readings rather than a settled empirical fact within this reading alone.
% FOUNDING_PROBLEM: GATT and then WTO members needed a way to resolve trade disagreements without resorting to unilateral tariff retaliation or trade wars, using a shared technical process to establish facts and legal consistency before political escalation.
% FOUNDING_PROBLEM_CORROBORATION: The WTO Secretariat and many smaller member states attest the advisory/facilitation function remains live and valuable. Independent trade law scholars outside the Secretariat and outside the disputing parties note that the DSU's text and the Appellate Body's own jurisprudence historically described the system in obligatory, binding terms — a status this reading contests rather than confirms, so corroboration for the advisory characterization specifically comes primarily from powerful states with an interest in discretionary enforcement, not from a neutral third party.
narrative_ontology:disappearance_verdict(wto_dsb_authority__advisory_coordination_reading, contested).
narrative_ontology:founding_problem_status(wto_dsb_authority__advisory_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__advisory_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(wto_dsb_authority__advisory_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__advisory_coordination_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__advisory_coordination_reading_tests).
:- end_tests(wto_dsb_authority__advisory_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28, rising modestly over the interval) because under this reading no party is formally compelled to transfer anything — the panel's output is informational, and any transfer that eventually occurs is the product of a separate, voluntary negotiation. Suppression is low (0.15) because no coercive enforcement machinery operates within this reading; a state that ignores an adverse finding faces, at most, authorized retaliation it can choose to absorb, not compulsion. Theater ratio starts low but rises across the interval (0.15 to 0.32) reflecting a documented pattern: as more panel reports accumulate without corresponding implementation, the process increasingly resembles a ritual of legitimation (parties go through litigation for diplomatic cover) rather than a mechanism that reliably produces settlement, even on this reading's own terms. Accessibility collapse is low (0.25) and resistance is moderate (0.35): alternative dispute channels (regional trade agreements, bilateral negotiation, WTO waiver mechanisms) remain genuinely available, and states that dislike a panel outcome resist by simply declining to implement, which the reading treats as a normal, available option rather than a violation requiring suppression to prevent.
 *
 * DIRECTIONALITY LOGIC:
 *   Trading member states broadly, and especially powerful member states, sit near the beneficiary end: they receive a low-cost diagnostic service and retain full discretion over what to do with it. Less powerful member states occupy an ambiguous position — they are formally beneficiaries of the same advisory service, but their trapped exit options (limited alternative markets, limited capacity to retaliate credibly) mean the coordination benefit is asymmetric in practice: the diagnosis is available to them but the leverage to act on it is not. This asymmetry is why less_powerful_member_states carries a secondary payer role even though this reading declares no formal victims — the cost they bear is an opportunity cost (a correct diagnosis that goes unenforced), not an extraction the reading's own logic recognizes as a transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this arrangement as pure extraction: under the advisory-coordination reading, there is no coerced transfer to identify a victim of, which is precisely why this file authors zero victims and a rope-adjacent claimed type rather than tangled_rope or snare. The interesting analytical work is elsewhere — in the divergence between this reading's low-ε self-description and the binding_referee_reading's much higher-ε account of the same underlying institutional facts. Neither reading is being 'corrected' toward the other; each is a complete, internally consistent account, and the corpus captures the disagreement structurally via network linkage and cs_structure.reading_relations rather than by forcing one ε onto a contested kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    advisory_vs_binding_kernel_ambiguity,
    'Does the DSU''s treaty language and the accumulated pattern of Appellate Body jurisprudence establish a genuinely binding compliance obligation, or is the observed pattern of negotiated, partial, and non-implementation evidence that the system functions descriptively as advisory coordination regardless of its textual framing?',
    'Systematic empirical study of implementation outcomes across the full history of DSB rulings, cross-referenced against the bilateral power differential between disputing parties, would show whether compliance correlates with the ruling''s legal content (supporting the binding reading) or with relative bargaining power (supporting the advisory-coordination reading).',
    'If compliance correlates strongly with bargaining power rather than ruling content, this reading''s low-ε characterization is descriptively vindicated and the binding_referee_reading''s higher-ε account should be read as normative/aspirational rather than descriptive. If compliance correlates with ruling content independent of power, the advisory-coordination reading understates the system''s actual coercive function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advisory_vs_binding_kernel_ambiguity, empirical, 'Whether the DSB''s authority is empirically advisory (power-driven settlement) or binding (rule-driven compliance).').

omega_variable(
    reading_selection_by_power_position,
    'Is the choice among the three kernel readings itself correlated with a state''s power position — do powerful states systematically prefer the advisory-coordination reading (which minimizes their compliance exposure) while weaker states and legal scholars systematically prefer the binding-referee reading (which would give them a compulsory remedy they otherwise lack)?',
    'Survey of official government legal positions and public statements across member states, cross-tabulated against relative trade power, would reveal whether the reading choice tracks structural interest.',
    'If reading choice tracks power position cleanly, this suggests the kernel''s ambiguity is not an accident of legal drafting but is itself load-bearing — the ambiguity allows powerful states to accept the system''s legitimacy costs while avoiding its compliance costs, which would be a second-order extraction the advisory-coordination reading''s own low ε does not capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_by_power_position, conceptual, 'Whether kernel-reading selection correlates with structural power position, constituting a hidden extraction the low-ε reading does not register.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__advisory_coordination_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t0, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(wto__tr_t6, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 6, 0.19).
narrative_ontology:measurement(wto__tr_t12, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(wto__tr_t18, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 18, 0.26).
narrative_ontology:measurement(wto__tr_t24, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement(wto__tr_t30, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 30, 0.32).

% Extraction over time
narrative_ontology:measurement(wto__be_t0, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(wto__be_t6, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 6, 0.2).
narrative_ontology:measurement(wto__be_t12, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 12, 0.22).
narrative_ontology:measurement(wto__be_t18, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 18, 0.24).
narrative_ontology:measurement(wto__be_t24, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 24, 0.26).
narrative_ontology:measurement(wto__be_t30, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 30, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(wto_dsb_authority__advisory_coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority__judicial_activism_reading).

% DUAL FORMULATION NOTE:
% This file is one of three sibling readings of the wto_dsb_authority kernel, decomposed per the ε-invariance principle because the three readings assign substantially different extraction and suppression profiles to the same institutional facts. advisory_coordination_reading (this file, ε≈0.28, rope-flavored) treats compliance as voluntary and negotiation-mediated. binding_referee_reading (ε expected substantially higher, tangled_rope or snare candidate) treats the DSU's obligatory language as creating genuine compliance duties with asymmetric burden on states lacking retaliation capacity. judicial_activism_reading treats panel and Appellate Body practice as an illegitimate expansion beyond treaty mandate, which would be authored with its own distinct extraction profile centered on the delegitimation cost to the treaty system itself. All three share the same underlying panels, texts, and case record; they diverge only in what kind of authority that record is read as establishing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
