% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__consensus_safeguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__consensus_safeguard_reading, []).

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
 *   constraint_id: supermajority_threshold__consensus_safeguard_reading
 *   human_readable: Supermajority Amendment Threshold — Consensus Safeguard Reading
 *   domain: constitutional_theory/political_economy/institutional_design
 *
 * SUMMARY:
 *   This story authors the CONSENSUS SAFEGUARD reading of the supermajority
 *   amendment threshold kernel: the claim that a high bar for constitutional
 *   change filters out transient majoritarian passion and preserves only
 *   changes reflecting deep, durable cross-partisan consensus. Under this
 *   reading the threshold is coordination-dominant: it protects a diffuse,
 *   intergenerational beneficiary class (the constitutional polity and future
 *   citizens) from short-term electoral volatility rewriting foundational
 *   rules, and it has no concentrated beneficiary who profits from blockage
 *   as such — reform coalitions pay in delay and coalition-building cost, not
 *   in permanent exclusion, because ordinary political and legal channels
 *   remain open. This is deliberately a low-ε, low-suppression profile: the
 *   reading treats the threshold as functioning largely as advertised. The
 *   sibling readings — minoritarian_veto_reading (same threshold, read as
 *   entrenchment of historical privilege into permanent blocking power) and
 *   adaptive_gradient_reading (same threshold, read as a tunable parameter
 *   whose legitimacy depends on empirical calibration) — are separate
 *   constraint stories with their own ε values and their own stakeholder
 *   structures, per the ε-invariance principle. They are not alternative
 *   measurements of this constraint; they are different constraints
 *   instantiated from the same kernel text.
 *
 * KEY AGENTS:
 *   - constitutional_polity: diffuse civilizational beneficiary of rule stability
 *   - future_generations: powerless, analytical-exit beneficiary of inherited settlement
 *   - reform_coalition_of_the_day: organized payer bearing delay and coalition-building cost
 *   - legislative_supermajority_architects: institutional agenda-setters administering the threshold
 *   - constitutional_courts: institutional observers who gain interpretive latitude as a side effect
 *   - durable_simple_majority_publics: excluded voice whose sustained-but-insufficient preference is not itself a channel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__consensus_safeguard_reading, 0.18).
domain_priors:suppression_score(supermajority_threshold__consensus_safeguard_reading, 0.28).
domain_priors:theater_ratio(supermajority_threshold__consensus_safeguard_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__consensus_safeguard_reading, rope).
narrative_ontology:human_readable(supermajority_threshold__consensus_safeguard_reading, "Supermajority Amendment Threshold — Consensus Safeguard Reading").
narrative_ontology:topic_domain(supermajority_threshold__consensus_safeguard_reading, "constitutional_theory/political_economy/institutional_design").

domain_priors:requires_active_enforcement(supermajority_threshold__consensus_safeguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__consensus_safeguard_reading, '82525e92-8080-43bf-99d1-4df65f62a880').
narrative_ontology:cs_kernel_codification('82525e92-8080-43bf-99d1-4df65f62a880', fixed_text).
narrative_ontology:cs_authority_grounding('82525e92-8080-43bf-99d1-4df65f62a880', lineage).
narrative_ontology:cs_interpretation_layer_present('82525e92-8080-43bf-99d1-4df65f62a880').
narrative_ontology:cs_reading_relation('82525e92-8080-43bf-99d1-4df65f62a880', supermajority_threshold__minoritarian_veto_reading, coexists_with).
narrative_ontology:cs_reading_relation('82525e92-8080-43bf-99d1-4df65f62a880', supermajority_threshold__adaptive_gradient_reading, influences).
narrative_ontology:cs_axiom('82525e92-8080-43bf-99d1-4df65f62a880', foundational, durability_over_time_indicates_genuine_consensus).
narrative_ontology:cs_axiom_status(durability_over_time_indicates_genuine_consensus, holdable).
narrative_ontology:cs_axiom_grounding('82525e92-8080-43bf-99d1-4df65f62a880', durability_over_time_indicates_genuine_consensus, empirically_contingent).
narrative_ontology:cs_axiom('82525e92-8080-43bf-99d1-4df65f62a880', foundational, foundational_rules_require_higher_warrant_than_ordinary_legislation).
narrative_ontology:cs_axiom_status(foundational_rules_require_higher_warrant_than_ordinary_legislation, holdable).
narrative_ontology:cs_axiom_grounding('82525e92-8080-43bf-99d1-4df65f62a880', foundational_rules_require_higher_warrant_than_ordinary_legislation, deontological).
narrative_ontology:cs_reference_frame('82525e92-8080-43bf-99d1-4df65f62a880', founding_ratification_consensus_standard).
narrative_ontology:cs_drift_state('82525e92-8080-43bf-99d1-4df65f62a880', contemporary_polarized_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('82525e92-8080-43bf-99d1-4df65f62a880', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, constitutional_polity).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, future_generations).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, minority_coalitions_at_risk_of_majoritarian_reversal).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(supermajority_threshold__consensus_safeguard_reading, reform_coalition_of_the_day).
narrative_ontology:constraint_vindicates(supermajority_threshold__consensus_safeguard_reading, deep_consensus_requirement_for_fundamental_change).
narrative_ontology:constraint_vindicates(supermajority_threshold__consensus_safeguard_reading, counter_majoritarian_stability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The body politic as a continuing entity across generations receives a stable framework of basic rules that cannot be rewritten by transient electoral majorities. Its members benefit from predictability in the rules governing rights, structure, and process, and from protection against a narrowly-won election being read as a mandate to rewrite foundational commitments.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, constitutional_polity, beneficiary,
    institutional, civilizational, analytical, national).

% Not yet born or politically enfranchised, future citizens inherit whatever constitutional settlement current supermajorities lock in. The threshold protects them from having foundational commitments overturned by a single generation's passing mood, at the cost of also constraining their own future ability to revise those same commitments easily.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, future_generations, beneficiary,
    powerless, civilizational, analytical, national).

% A political coalition holding a durable simple majority but falling short of the supermajority threshold must build broader coalitions, wait out electoral cycles, or abandon a change it believes reflects genuine popular will. Exit is constrained rather than trapped: ordinary legislation, litigation, and slow coalition-building remain open, but the specific constitutional route is barred.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, reform_coalition_of_the_day, payer,
    organized, biographical, constrained, national).

% The founding and amending institutions that set the threshold level (two-thirds, three-fourths, ratification by sub-units, etc.) administer the rule and can in principle alter the threshold itself, though usually only through the very high-threshold mechanism the rule protects. They do not collect rents from the rule; their exposure is to legitimacy costs if the threshold is perceived as an entrenchment device rather than a quality filter.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, legislative_supermajority_architects, agenda_setter,
    institutional, generational, analytical, national).

% Adjudicate disputes over whether the threshold was met, whether procedural workarounds are valid, and interpret ambiguous constitutional text in the gap left by the high amendment barrier. Their interpretive latitude expands precisely because formal amendment is hard, giving them a secondary agenda-setting role over constitutional meaning.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__consensus_safeguard_reading, constitutional_courts, agenda_setter).

% A public that has sustained a clear majority preference for a specific constitutional change across multiple election cycles has no direct voice in whether that sustained preference should itself count as sufficient warrant — the threshold's design assumes such majorities may still reflect transient passion and does not ask them to certify otherwise.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, durable_simple_majority_publics, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of distinguishing durable, cross-partisan constitutional consensus from a narrow or short-lived electoral majority, so that the foundational rules of the polity are not rewritten on the strength of a single election cycle.
% TRANSFER_FUNCTION: Moves decision-making friction from the moment of constitutional change onto whoever proposes it: a reform coalition must spend additional political capital, time, and coalition-building effort to clear the higher bar, in exchange for the resulting change (once made) being harder for a future narrow majority to undo.
% ABSENT_VOICES: Durable simple-majority publics whose sustained preference falls short of supermajority have no formal channel to argue that persistence of preference across cycles should itself satisfy the 'deep consensus' test the threshold claims to measure; the rule's own framing forecloses that argument by definition rather than by debate.
% DISAPPEARANCE_RATIONALE: Proponents of this reading hold that removing the threshold would make constitutional law functionally identical to ordinary legislation, eroding the special stability that distinguishes foundational commitments from policy preferences — a significant rearrangement. Critics (see sibling readings) hold that removal would simply let sustained majoritarian will finally govern, which is not a rearrangement but a correction. The verdict is genuinely contested between the readings, which is why this kernel is decomposed into multiple constraint stories.
% FOUNDING_PROBLEM: Founding generations sought a mechanism to prevent constitutional fundamentals — rights, structure, separation of powers — from being rewritten by the same ordinary majoritarian process used for tax rates or zoning law, on the theory that foundational rules need a higher warrant than everyday politics.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional theorists and framers' own writings (outside any single beneficiary group) attest the original problem — preventing majoritarian instability in foundational law — was genuine and is documented in ratification debates. Political scientists studying amendment rates across democracies corroborate that very high thresholds correlate with near-total amendment freeze in some polities, which is cited by the sibling minoritarian_veto_reading as evidence the 'consensus filter' function has been substantially overtaken by an entrenchment function in practice; this story's own reading holds the filter function remains live, but acknowledges the corroborating evidence is contested rather than one-sided.
narrative_ontology:disappearance_verdict(supermajority_threshold__consensus_safeguard_reading, contested).
narrative_ontology:founding_problem_status(supermajority_threshold__consensus_safeguard_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__consensus_safeguard_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(supermajority_threshold__consensus_safeguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__consensus_safeguard_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__consensus_safeguard_reading_tests).
:- end_tests(supermajority_threshold__consensus_safeguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) and suppression moderate-low (0.28) because, under THIS reading, no party collects rents from the threshold's operation — the beneficiaries are diffuse and intergenerational, not a concentrated capturer, and the barrier is legitimated by a genuine coordination problem (distinguishing durable consensus from transient passion) rather than by protecting an incumbent interest. Theater ratio is low (0.12) because the threshold's formal function (requiring broad coalition-building for foundational change) is substantially the function it performs under this reading — there is little daylight between the stated justification and the observed operation, by construction of the reading. Accessibility collapse is moderate (0.35): ordinary legislative and judicial routes remain open to a blocked reform coalition, so alternatives have not collapsed completely, only the specific constitutional-amendment route has. Resistance is moderate-low (0.3): reform coalitions that fall short do express frustration and mount repeated campaigns, but this reading treats that as evidence the filter is working as intended (requiring persistence) rather than evidence of illegitimate blockage.
 *
 * DIRECTIONALITY LOGIC:
 *   The constitutional polity and future generations sit near the full-beneficiary end of directionality: they receive stability without paying a concentrated cost, and their 'benefit' is diffuse across the whole population rather than captured by an identifiable agent. The reform coalition of the day sits closer to the target end but not at the extreme — its exit options are constrained, not trapped, because ordinary politics and litigation remain available; only the specific high-threshold amendment route is barred. Legislative architects and courts are agenda-setters/observers rather than beneficiaries in the rent-collecting sense — this is precisely what distinguishes this reading from a snare-shaped reading: no seat here is authored as profiting from blockage as such.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing foundational law from being rewritten by ordinary majoritarian churn — remains genuinely contestable as live or dead, which is why founding_problem_status is authored 'contested' rather than 'live': under this reading the filter function persists and is still exercised (coalitions do form, do clear the bar, and changes do happen), so the mandate has not obviously outlived its function. Mandatrophy would be triggered instead under the minoritarian_veto_reading's story, where the same structural fact (amendment freeze) is read as the coordination function having died while the barrier persists as pure entrenchment. Keeping these as separate stories prevents the single natural-language label 'supermajority threshold' from forcing one classification to average across genuinely opposed readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_vs_entrenchment_reading_selection,
    'Is the supermajority threshold, as it actually operates in this polity, functioning primarily as a genuine filter for durable cross-partisan consensus (this reading) or primarily as an entrenchment mechanism protecting a historically advantaged minority''s veto (the sibling minoritarian_veto_reading)?',
    'Empirical comparison of (a) which proposed amendments cleared the threshold and whether they reflected genuinely broad, cross-partisan coalitions versus narrow ideologically-homogeneous supermajorities concentrated by malapportionment; (b) amendment failure patterns — whether blocked amendments consistently favor the same substantive interests across decades, which would suggest entrenchment rather than a neutral consensus filter.',
    'If empirical amendment patterns show durable cross-partisan majorities routinely succeed while narrow but persistent majorities are blocked regardless of their persistence, this reading''s low-extraction, diffuse-beneficiary framing is supported. If the same identifiable minority interest is protected by blockage across multiple unrelated amendment attempts, the minoritarian_veto_reading''s framing better fits the observed data and this story''s ε would be understated for the polity in question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_vs_entrenchment_reading_selection, empirical, 'Whether observed amendment outcomes support the consensus-filter framing or the entrenchment framing for this reading''s polity.').

omega_variable(
    diffuse_beneficiary_naturalness,
    'Is ''the constitutional polity'' and ''future generations'' as beneficiary classes a genuine collective-action beneficiary structure, or is naming a diffuse beneficiary a way of obscuring that specific, identifiable incumbent interests are the actual beneficiaries of any given threshold-protected settlement?',
    'Case-by-case analysis of specific constitutional provisions the threshold has protected from amendment: if the protected provisions disproportionately benefit specific identifiable groups (e.g., specific regional, economic, or demographic interests) rather than being genuinely general rules, the diffuse-beneficiary framing is an artifact of aggregation rather than a structural fact.',
    'If specific protected provisions turn out to systematically benefit identifiable incumbent groups, this reading''s classification (rope, diffuse benefit, no victims) would need reassessment toward tangled_rope for those specific provisions, even while the general threshold mechanism might remain rope-like for other provisions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diffuse_beneficiary_naturalness, conceptual, 'Whether the diffuse beneficiary framing survives disaggregation to specific protected constitutional provisions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__consensus_safeguard_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(supe_tr_t60, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 60, 0.11).
narrative_ontology:measurement(supe_tr_t80, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 80, 0.115).
narrative_ontology:measurement(supe_tr_t100, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 100, 0.12).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 40, 0.16).
narrative_ontology:measurement(supe_be_t60, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 60, 0.16).
narrative_ontology:measurement(supe_be_t80, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 80, 0.17).
narrative_ontology:measurement(supe_be_t100, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 100, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(supermajority_threshold__consensus_safeguard_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__consensus_safeguard_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(supermajority_threshold__consensus_safeguard_reading, 0.1).
narrative_ontology:affects_constraint(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold__minoritarian_veto_reading).
narrative_ontology:affects_constraint(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold__adaptive_gradient_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposed from the single natural-language label 'supermajority threshold' per the ε-invariance principle. consensus_safeguard_reading (this file) authors low ε (0.18), rope classification, diffuse beneficiaries, no victims. minoritarian_veto_reading authors the same threshold with an identifiable victim set (durable sub-threshold majorities) and higher ε, likely tangled_rope or snare. adaptive_gradient_reading treats the threshold's classification as conditional on empirical calibration data rather than fixed, and functions as the analytical bridge between the other two readings — hence 'influences' rather than 'coexists_with' in the reading_relations above. All three share the same underlying kernel text and the same fixed numerical threshold; they differ in what function they attribute to it and therefore in beneficiary/victim structure and ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
