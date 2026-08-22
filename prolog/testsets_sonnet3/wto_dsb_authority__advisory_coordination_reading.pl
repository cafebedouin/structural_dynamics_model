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
 *   human_readable: WTO DSB as Advisory Coordination Mechanism (Sovereignty-Preserving Reading)
 *   domain: international_law/trade_governance
 *
 * SUMMARY:
 *   This story authors the advisory-coordination reading of DSB authority:
 *   panels issue expert technical opinions that member states use as one
 *   input among several in negotiating trade disputes, and the panel process
 *   is valued because it supplies a shared factual and legal baseline, not
 *   because it compels outcomes. Under this reading the DSB is best
 *   understood as a Rope — a coordination mechanism that solves a genuine
 *   information and credibility problem in multilateral bargaining, with
 *   member states remaining net beneficiaries and no coercive machinery
 *   compelling compliance. The theater ratio rises slowly over the interval
 *   as the appellate mechanism became partially non-functional (2019 onward,
 *   following blocked appointments), which under this reading is read as a
 *   coordination tool losing some of its shared-reference value rather than
 *   as an enforcement collapse — since on this account there was no
 *   enforcement function to collapse.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__advisory_coordination_reading, 0.28).
domain_priors:suppression_score(wto_dsb_authority__advisory_coordination_reading, 0.15).
domain_priors:theater_ratio(wto_dsb_authority__advisory_coordination_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__advisory_coordination_reading, rope).
narrative_ontology:human_readable(wto_dsb_authority__advisory_coordination_reading, "WTO DSB as Advisory Coordination Mechanism (Sovereignty-Preserving Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__advisory_coordination_reading, "international_law/trade_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__advisory_coordination_reading, '77808560-a0d6-4e08-95ea-7af891e367cf').
narrative_ontology:cs_kernel_codification('77808560-a0d6-4e08-95ea-7af891e367cf', fixed_text).
narrative_ontology:cs_authority_grounding('77808560-a0d6-4e08-95ea-7af891e367cf', distributed).
narrative_ontology:cs_reading_relation('77808560-a0d6-4e08-95ea-7af891e367cf', wto_dsb_authority__binding_referee_reading, coexists_with).
narrative_ontology:cs_reading_relation('77808560-a0d6-4e08-95ea-7af891e367cf', wto_dsb_authority__judicial_activism_reading, coexists_with).
narrative_ontology:cs_axiom('77808560-a0d6-4e08-95ea-7af891e367cf', foundational, state_consent_as_sole_source_of_obligation).
narrative_ontology:cs_axiom_status(state_consent_as_sole_source_of_obligation, holdable).
narrative_ontology:cs_axiom_grounding('77808560-a0d6-4e08-95ea-7af891e367cf', state_consent_as_sole_source_of_obligation, conventional).
narrative_ontology:cs_axiom('77808560-a0d6-4e08-95ea-7af891e367cf', secondary, panel_opinions_are_persuasive_not_dispositive).
narrative_ontology:cs_axiom_status(panel_opinions_are_persuasive_not_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('77808560-a0d6-4e08-95ea-7af891e367cf', panel_opinions_are_persuasive_not_dispositive, conventional).
narrative_ontology:cs_reference_frame('77808560-a0d6-4e08-95ea-7af891e367cf', gatt_diplomatic_consultation_baseline).
narrative_ontology:cs_drift_state('77808560-a0d6-4e08-95ea-7af891e367cf', post_appellate_body_paralysis_2019, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('77808560-a0d6-4e08-95ea-7af891e367cf', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, member_state_governments).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, export_dependent_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(wto_dsb_authority__advisory_coordination_reading, smaller_developing_economies).
narrative_ontology:constraint_vindicates(wto_dsb_authority__advisory_coordination_reading, westphalian_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(wto_dsb_authority__advisory_coordination_reading, consent_based_international_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bring disputes to the DSB to get an authoritative technical read on whether a trading partner's measure violates agreed rules, then use that read as leverage in bilateral or plurilateral negotiation. They retain the choice to implement, partially implement, negotiate compensation, or accept retaliation — the ruling informs their bargaining position but does not by itself compel a policy change. Powerful states in particular treat adverse rulings as one input among several (domestic political cost, retaliation capacity, relationship value).
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, member_state_governments, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__advisory_coordination_reading, member_state_governments, agenda_setter).

% Benefit when their government successfully uses a DSB panel opinion to persuade a trading partner to loosen a market-access barrier through negotiation. Their benefit is contingent on their government's bargaining leverage and willingness to spend political capital, not on the ruling automatically taking effect.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, export_dependent_industries, beneficiary,
    organized, biographical, constrained, global).

% Produce reasoned technical opinions applying covered agreements to the facts of a dispute. Under this reading, their output is expert counsel that clarifies the legal landscape for negotiators; they have no independent enforcement capacity and depend entirely on the political will of the parties and the DSB membership to give the opinion any operative force.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, dsb_panelists_and_appellate_body, agenda_setter,
    analytical, immediate, analytical, global).

% Win panel opinions against larger trading partners but lack the retaliation capacity or bilateral leverage to convert a favorable ruling into an actual negotiated concession. Under the advisory-coordination reading this is framed as a normal limit of a consent-based system rather than a defect requiring institutional compulsion; in practice it means the coordination benefit is unevenly realized.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, smaller_developing_economies, payer,
    moderate, biographical, constrained, national).

% Administers the dispute settlement process, tracks compliance notifications, and maintains the institutional record, but under this reading does not itself compel outcomes — it facilitates the negotiation process the panel opinions feed into.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, wto_secretariat, observer,
    institutional, generational, analytical, global).

% Debate whether the DSB's actual operation matches this advisory-coordination characterization or one of the sibling readings; contribute empirical compliance-rate studies that bear on which reading better describes the system's actual behavior.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, trade_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, expert, rules-referenced diagnosis of whether a challenged trade measure is consistent with agreed multilateral commitments, so that governments negotiating a resolution start from a common technical understanding rather than dueling unilateral claims.
% TRANSFER_FUNCTION: Moves informational and reputational leverage from the losing party to the prevailing party in a dispute; no binding transfer of policy authority occurs — any resulting change in trade measures is negotiated, not compelled.
% ABSENT_VOICES: Firms and workers directly harmed by a challenged measure have no standing before the panel at all; only member state governments are parties, so private economic interests are represented only to the extent their government chooses to advance them in negotiation.
% DISAPPEARANCE_RATIONALE: Proponents of this reading hold that if the DSB vanished, states would revert to purely bilateral technical consultation and retaliation-threat bargaining — a real but modest loss of a shared diagnostic forum. Proponents of the sibling binding-referee reading would say the world rearranges dramatically, since (on their account) an enforceable compliance mechanism would disappear. The verdict is genuinely contested because it depends on which reading of DSB authority is correct, which is exactly the kernel dispute this story is one reading of.
% FOUNDING_PROBLEM: Pre-WTO GATT dispute settlement relied on diplomatic consultation with no structured technical adjudication, leaving powerful states able to block panel formation or adoption of unfavorable reports by veto; the DSB was built to supply a standing, rules-based, expert process states could use to clarify their obligations without abandoning ultimate control over domestic policy.
% FOUNDING_PROBLEM_CORROBORATION: Governments that favor this reading (frequently large trading powers with retaliation capacity) attest that the system still functions as advisory input to sovereign bargaining. Independent compliance-rate researchers and smaller-economy trade ministries — parties outside the beneficiary group with the most retaliation leverage — report that in practice large-power compliance correlates with retaliation credibility rather than legal reasoning quality, which is evidence for a mixed reading rather than a clean advisory-coordination account; this story reports that corroboration honestly rather than suppressing it.
narrative_ontology:disappearance_verdict(wto_dsb_authority__advisory_coordination_reading, contested).
narrative_ontology:founding_problem_status(wto_dsb_authority__advisory_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__advisory_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.28 at interval end) because under this reading no party is coerced into a transfer it did not consent to accept — implementation remains a sovereign choice, and the losing party can offer compensation or accept retaliation as alternatives to compliance, all negotiated. Suppression is low (0.15) because there is no compulsory mechanism forcing outcomes; the panel's opinion is persuasive, not dispositive. Accessibility collapse is moderate (0.35), reflecting that once a dispute goes to panel, the range of technical/legal characterizations narrows somewhat, but states retain full discretion over their policy response. Resistance is moderate-low (0.3): some friction exists around whether to even convene a panel, but little active resistance to the advisory process itself, since it imposes no binding cost.
 *
 * PERSPECTIVAL GAP:
 *   The three sibling readings of this kernel diverge sharply on what the panel opinion actually IS. Under advisory_coordination_reading (this story), the opinion is an input to negotiation and the system is fundamentally consensual — hence rope, hence low extraction. Under binding_referee_reading the same textual output is treated as a treaty-grounded obligation whose non-implementation is itself a violation, which would authorize far higher suppression and extraction scores for the same underlying institutional facts. Under judicial_activism_reading the panels are seen as exceeding mandate through interpretive drift, which would generate a different beneficiary/victim structure again (interpreting bodies as illegitimate beneficiaries, member states broadly as victims of scope creep). These are not three measurements of one constraint — per the ε-invariance principle they are three different constraints sharing a kernel, and this file authors only the first.
 *
 * DIRECTIONALITY LOGIC:
 *   Member state governments and their export sectors are the structural beneficiaries: they get a low-cost, expert diagnostic that improves the quality of their negotiating position without surrendering any policy authority — d sits near the beneficiary end. Smaller developing economies are coded as payers not because the process extracts from them directly, but because the coordination benefit is asymmetrically realized: they can win the opinion but often lack the leverage to convert it into an actual negotiated result, so the same nominal 'coordination' yields less real benefit the less bargaining power a state holds. This is why victims is left empty at the base_properties level (no group bears a compelled transfer under this reading) while smaller_developing_economies still carries role: payer at the stakeholder level to capture the uneven realization of the coordination good.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — replacing blockable, ad hoc diplomatic consultation with a standing technical process — remains partially live under this reading: states still convene panels and still value the shared diagnostic. Whether the mandate has drifted (a scaffold-like or judicial-overreach dynamic) is precisely the contest between this reading and its siblings; this reading holds mandatrophy has NOT occurred because the system never claimed binding authority in the first place, so there is no mandate to have outlived.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compliance_rate_diagnosticity,
    'Do observed WTO compliance rates (historically high relative to other international tribunals) reflect genuine voluntary coordination consistent with this advisory reading, or do they reflect a binding-obligation structure that this reading mischaracterizes as merely advisory?',
    'Comparative analysis of compliance behavior when retaliation-capacity is held constant: if compliance tracks legal merits independent of retaliation credibility, that favors a binding-authority account over the advisory-coordination account authored here.',
    'If compliance is shown to track legal reasoning rather than power asymmetry, this reading''s core claim (that rulings are merely inputs to power-driven bargaining) weakens substantially and the binding_referee_reading becomes the better-supported account of the same textual kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_rate_diagnosticity, empirical, 'Whether compliance patterns are better explained by voluntary coordination or by binding treaty obligation.').

omega_variable(
    sibling_reading_foreclosure_question,
    'Is the advisory_coordination_reading and the binding_referee_reading a genuine forecloses pair (a treaty cannot simultaneously create binding obligations and mere advisory inputs for the same act), or can both be held coherently by different parties without contradiction because ''binding'' itself is ambiguous in the DSU text?',
    'Close textual analysis of DSU Articles 3.2, 19, 21-22 (retaliation authorization provisions) cross-referenced with actual state practice in non-implementation cases; a finding that non-implementation triggers authorized countermeasures under treaty law (not mere bilateral discretion) would support foreclosure.',
    'If the readings genuinely foreclose one another, this story''s ''coexists_with'' classification in cs_structure.reading_relations should instead be ''forecloses'' for the binding_referee_reading sibling, changing how the two constraints interact structurally.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_question, conceptual, 'Whether the advisory and binding readings can coexist as different parties'' framings or logically exclude one another.').

omega_variable(
    asymmetric_realization_as_extraction,
    'Does the systematically lower ability of smaller developing economies to convert favorable rulings into negotiated outcomes constitute a form of extraction internal to this reading (i.e., the coordination good is unevenly distributed by design because it depends on retaliation capacity), or is it merely an external inequality the coordination mechanism does not itself cause?',
    'Track whether small-economy win rates on the merits track their actual negotiated outcome rates, controlling for dispute subject matter; a persistent large gap between legal-merit success and negotiated-outcome success implicates the coordination mechanism itself, not just background power inequality.',
    'If the mechanism itself channels the coordination benefit toward already-powerful states, base_properties.extractiveness for this reading should be revised upward and a victim group added, pushing the classification toward tangled_rope even under this reading''s own premises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_realization_as_extraction, empirical, 'Whether uneven realization of DSB coordination benefits by power level constitutes extraction internal to this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__advisory_coordination_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(wto__tr_t2001, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2001, 0.13).
narrative_ontology:measurement(wto__tr_t2007, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2007, 0.15).
narrative_ontology:measurement(wto__tr_t2013, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2013, 0.17).
narrative_ontology:measurement(wto__tr_t2019, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2019, 0.2).
narrative_ontology:measurement(wto__tr_t2025, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 1995, 0.18).
narrative_ontology:measurement(wto__be_t2001, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2001, 0.2).
narrative_ontology:measurement(wto__be_t2007, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2007, 0.22).
narrative_ontology:measurement(wto__be_t2013, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2013, 0.24).
narrative_ontology:measurement(wto__be_t2019, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2019, 0.26).
narrative_ontology:measurement(wto__be_t2025, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2025, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(wto_dsb_authority__advisory_coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__advisory_coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_dsb_authority__advisory_coordination_reading, 0.12).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority__judicial_activism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the wto_dsb_authority kernel. advisory_coordination_reading (this file) authors the DSB as a low-extraction Rope grounded in sovereign consent. binding_referee_reading authors the same textual/institutional kernel as a Tangled Rope or Snare grounded in treaty-based compulsion with meaningfully higher extraction and suppression. judicial_activism_reading authors the kernel as illegitimate interpretive overreach with yet another beneficiary/victim structure (the adjudicating bodies as beneficiaries of expanded jurisdiction, member states broadly as victims of scope creep). Per the ε-invariance principle, these are three distinct constraints sharing one kernel, not one constraint measured three ways; they are linked here for contamination-propagation and family-tracking purposes only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
