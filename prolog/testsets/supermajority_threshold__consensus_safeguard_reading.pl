% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__consensus_safeguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: supermajority_threshold__consensus_safeguard_reading
 *   human_readable: Supermajority Amendment Threshold as Consensus Safeguard
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   A supermajority threshold for constitutional amendment is presented by
 *   this reading as a structural safeguard ensuring that constitutional
 *   changes reflect deep, persistent democratic consensus rather than
 *   transient majoritarian passion. The threshold requires broad agreement
 *   across diverse constituencies, making the amendment process intentionally
 *   difficult. The reading is one of three competing interpretations of the
 *   same constitutional kernel: the minoritarian_veto_reading frames the same
 *   mechanism as entrenching privilege, while the adaptive_gradient_reading
 *   frames it as a calibration problem requiring evidence-based tuning. This
 *   story instantiates ONLY the consensus-safeguard reading and its
 *   structural claim about constitutional quality. The sibling readings are
 *   separate constraint stories with their own ε values and framings.
 *
 * KEY AGENTS:
 *   - supermajority_coalition_at_amendment_moment: must assemble and hold consensus across diverse constituencies; successfully clearing the threshold demonstrates durable preference
 *   - blocking_minority_coalition: cannot amend when they hold only simple-majority support; experience the threshold as veto but reading frames as necessary friction
 *   - constitutional_text_as_stabilized_kernel: benefits from resistance to casual revision; the threshold is the mechanism preserving its authority
 *   - future_generations: inherit either constitutional stability or instability; reading assumes they benefit from consensus-based amendments over majoritarian churning
 *   - comparative_constitutional_framers: observe the tradeoff and inform what threshold level is optimal; analytical seat, not beneficiary or payer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__consensus_safeguard_reading, 0.28).
domain_priors:suppression_score(supermajority_threshold__consensus_safeguard_reading, 0.12).
domain_priors:theater_ratio(supermajority_threshold__consensus_safeguard_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__consensus_safeguard_reading, rope).
narrative_ontology:human_readable(supermajority_threshold__consensus_safeguard_reading, "Supermajority Amendment Threshold as Consensus Safeguard").
narrative_ontology:topic_domain(supermajority_threshold__consensus_safeguard_reading, "constitutional/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__consensus_safeguard_reading, '66eb88df-48b8-4250-ba07-e138502feeed').
narrative_ontology:cs_kernel_codification('66eb88df-48b8-4250-ba07-e138502feeed', fixed_text).
narrative_ontology:cs_authority_grounding('66eb88df-48b8-4250-ba07-e138502feeed', lineage).
narrative_ontology:cs_interpretation_layer_present('66eb88df-48b8-4250-ba07-e138502feeed').
narrative_ontology:cs_reading_relation('66eb88df-48b8-4250-ba07-e138502feeed', supermajority_threshold__minoritarian_veto_reading, coexists_with).
narrative_ontology:cs_reading_relation('66eb88df-48b8-4250-ba07-e138502feeed', supermajority_threshold__adaptive_gradient_reading, influences).
narrative_ontology:cs_axiom('66eb88df-48b8-4250-ba07-e138502feeed', foundational, consensus_ensures_constitutional_quality).
narrative_ontology:cs_axiom_status(consensus_ensures_constitutional_quality, holdable).
narrative_ontology:cs_axiom_grounding('66eb88df-48b8-4250-ba07-e138502feeed', consensus_ensures_constitutional_quality, deontological).
narrative_ontology:cs_axiom('66eb88df-48b8-4250-ba07-e138502feeed', secondary, supermajority_friction_prevents_churn).
narrative_ontology:cs_axiom_status(supermajority_friction_prevents_churn, holdable).
narrative_ontology:cs_axiom_grounding('66eb88df-48b8-4250-ba07-e138502feeed', supermajority_friction_prevents_churn, instrumental).
narrative_ontology:cs_reference_frame('66eb88df-48b8-4250-ba07-e138502feeed', constitutional_stability_through_consensus_filter).
narrative_ontology:cs_drift_state('66eb88df-48b8-4250-ba07-e138502feeed', contemporary_polarization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('66eb88df-48b8-4250-ba07-e138502feeed', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, constitutional_continuity).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(supermajority_threshold__consensus_safeguard_reading, blocking_minority_coalition).
narrative_ontology:constraint_vindicates(supermajority_threshold__consensus_safeguard_reading, democratic_deliberation_quality_thesis).
narrative_ontology:constraint_vindicates(supermajority_threshold__consensus_safeguard_reading, constitutional_stability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actors seeking constitutional amendment must assemble and maintain a supermajority coalition across diverse constituencies. They negotiate, persuade, and invest resources in building consensus. If they succeed in clearing the threshold, they have demonstrated both the amplitude and durability of their preference — the supermajority requirement makes them prove the change reflects something deeper than momentary passion.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, supermajority_coalition_at_amendment_moment, agenda_setter,
    powerful, biographical, mobile, national).

% A coalition that cannot or will not assemble a supermajority faces the cost of being unable to amend the constitution even when they hold a bare majority. They must either build broader consensus or accept the constitutional status quo. They experience the threshold as a veto — their preference for change is blocked — but the reading frames this as necessary friction, not extraction.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, blocking_minority_coalition, payer,
    moderate, biographical, constrained, national).

% The constitutional text itself is the entity that benefits from the supermajority requirement: it resists casual revision, persists across electoral cycles, and maintains its authority across generational transitions. The threshold is the mechanism that keeps the constitution from becoming a mere law subject to simple-majority flux.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, constitutional_text, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(supermajority_threshold__consensus_safeguard_reading, constitutional_text).

% The voters and constituencies whose preferences the threshold mediates. They experience both the protection of constitutional stability and the friction of being unable to amend the constitution when they hold only a simple majority. The reading assumes this mixed experience reflects a justified choice in favor of quality over majoritarian velocity.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, present_generation_polity, observer,
    organized, biographical, mobile, national).

% Not present at amendment moments but inherit the constitution's stability or instability. The reading assumes future generations benefit from constitutional amendments that reflect genuine, deep consensus rather than transient majorities that would fragment under changed conditions.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, future_generations, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(supermajority_threshold__consensus_safeguard_reading, future_generations).

% Constitutional design experts across jurisdictions who evaluate amendment procedures. They observe the tradeoff between amendment ease (responding to real change) and stability (resisting churn), and their analysis informs what threshold level is optimal. They do not directly benefit from or pay the cost of any particular supermajority rule.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, comparative_constitutional_scholars, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__consensus_safeguard_reading, diffuse).
narrative_ontology:fixing_cost_class(supermajority_threshold__consensus_safeguard_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the constitutional community around a shared, stable framework: the supermajority requirement is the mechanism that ensures only amendments with broad, durable consensus can alter the foundational text, preventing constitutional instability from simple-majority cycling.
% TRANSFER_FUNCTION: Transfers decision-making power FROM simple majorities (who can change policy via regular legislation) TO supermajority coalitions (who can change the constitutional rules themselves). This is a power distribution, not a wealth transfer, but it means that actors commanding only simple-majority support cannot unilaterally alter the constitutional structure.
% ABSENT_VOICES: Blocked minorities — those who want amendment but cannot assemble a supermajority — would object if heard during the amendment debate. They are present as participants but structurally underheard because they lack the numbers the threshold requires. The threshold itself silences their preference for change, though it does not silence their voice in ordinary politics.
% DISAPPEARANCE_RATIONALE: If the supermajority threshold disappeared and amendment became possible via simple majority, constitutional turnover would accelerate dramatically. The constitution would become subject to electoral cycling: each majority government could rewrite foundational terms to entrench its power. The stable institutional framework the supermajority requirement sustains would fragment. Future generations would inherit constitutional instability rather than continuity.
% FOUNDING_PROBLEM: Early democratic republics faced the risk that constitutions would become prizes for majoritarian factions, rewritten with each electoral shift. A supermajority threshold was designed to solve this by making the constitution difficult enough to change that only amendments reflecting genuine, broad, persistent consensus could succeed.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and political theorists outside the benefiting parties (future generations cannot testify) attest that constitutional cycling and instability are real historical risks documented across democracies. Comparative constitutional analysis supports that amendment thresholds do produce greater stability. However, minoritarian-veto critics atttest that the same mechanism can entrench privilege against majoritarian will, and adaptive-gradient theorists attest that the threshold's legitimacy depends on calibration to actual consensus-formation rates — the founding problem is incompletely solved and depends on how the threshold is set.
narrative_ontology:disappearance_verdict(supermajority_threshold__consensus_safeguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__consensus_safeguard_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__consensus_safeguard_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(supermajority_threshold__consensus_safeguard_reading, 'none', 1).

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
 *   Extractiveness is LOW (0.28 by interval end) because the supermajority threshold is presented as a quality filter on collective decision-making, not as a mechanism extracting from specific targets. Its beneficiaries are abstract (constitutional continuity, future generations) rather than organized actors. Suppression is minimal (0.12) because the threshold operates through deliberative friction, not coercion — a blocking minority retains the option to build broader consensus or accept the status quo; there is no enforcement machinery preventing them from persuading others. Theater is very low (0.08) because the threshold's function (filtering for real consensus) matches its stated purpose. The measurement series shows slight drift upward over the 60-year interval, reflecting increased contestation about whether the threshold is being applied fairly across partisan lines (adaptive-gradient and minoritarian-veto critiques rising, suggesting the constraint's legitimacy is being challenged), but the baseline metrics remain stable. All metrics are authored on a shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces a symmetric seat structure: no seat should compute as systematically exploited. The blocking-minority seat (constrained exit, moderate power) experiences friction but not extraction — they retain the option to build consensus. The supermajority-coalition seat (powerful, mobile exit) faces the burden of coalition-building but benefits from the authority their consensus carries. Future generations (non-agent) benefit from stability. The key ambiguity is whether the threshold's calibration has drifted, such that it now entrenches privilege (minoritarian-veto reading) or diverges from actual consensus-formation rates (adaptive-gradient reading). This reading assumes the threshold performs its stated function; the sibling readings document when that assumption breaks.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: Beneficiaries listed are abstract (constitutional_continuity, future_generations, both non-agents or non-participating seats). No concrete actor is listed as a beneficiary — the constitutional order itself benefits, which means no specific power atom benefits from the extraction. Victims array is empty because no group is systematically targeted for extraction. The blocking-minority payer seat is constrained (cannot amend) but not victimized (retains options, retains voice). This structure produces diffuse directionality — the threshold operates across all power atoms symmetrically, filtering for consensus rather than favoring any particular seat. No override needed; the structural derivation (zero beneficiary concentration, zero victim asymmetry, mobile/constrained exits for different seats) produces d-values scattered around the symmetric midpoint.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy risk: The founding problem is constitutional instability from majoritarian cycling. This reading claims the supermajority threshold solves that problem by making amendments difficult enough that only persistent, broad consensus can succeed. The measurement trajectory shows mild upward drift in extractiveness and theater (t=60 vs t=0), suggesting increased contestation about whether the threshold still serves its founding function or has become an entrenched veto. The mismatch consumer (founding_problem_status=contested x disappearance_verdict=world_rearranges) flags a potential mandatrophy: if the threshold is no longer filtering for genuine consensus but merely blocking change preferred by bare majorities, it has become a snare or piton rather than a rope. The reading does NOT claim mandatrophy has resolved — it claims the founding problem remains live but is increasingly contested. The sibling readings (minoritarian-veto, adaptive-gradient) offer competing diagnoses of whether and how the threshold has degraded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_threshold_calibration,
    'Is the supermajority threshold (66.7% or other specific fraction) calibrated to the actual rate at which new constitutional consensus forms in this polity, or is it arbitrary/inherited?',
    'Empirical study of amendment-effort trajectories: track which proposed amendments fail at the threshold, which succeed, and whether those that succeed show evidence of genuine, durable consensus vs. temporary coalition. Compare against historical consensus-formation rates in the polity.',
    'If the threshold is well-calibrated to actual consensus rates, the reading''s framing holds. If the threshold is systematically too high (blocking changes that reflect genuine consensus) or too low (admitting changes that lack durability), the constraint is either snare-like (blocking justified change) or failing its stated function (not filtering for real consensus). This would support the adaptive_gradient_reading''s claim that legitimacy depends on calibration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consensus_threshold_calibration, empirical, 'Whether the threshold''s level matches the actual consensus-formation rate in the polity, or is arbitrary.').

omega_variable(
    blocking_minority_entrenchment_risk,
    'Can a stable blocking minority use the supermajority threshold to entrench a privileged status quo against majoritarian will over time, converting historical power into permanent constitutional veto?',
    'Comparative constitutional analysis: examine cases where a demographically stable minority (religious, ethnic, regional, class-based) has used supermajority requirements to block amendments that would alter their position, across multiple amendment cycles. Test whether the same minority blocks consistently and whether the blocking correlates with their historical power rather than the merits of the proposed amendments.',
    'If blocking minorities consistently entrench privilege, the constraint is snare-like (pure extraction from the blocked majority) rather than a quality filter. This would validate the minoritarian_veto_reading''s structural claim. If blocking is episodic and involves different coalitions on different amendments, the rope framing (blocking friction is necessary for consensus) holds more plausibly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(blocking_minority_entrenchment_risk, empirical, 'Whether the threshold enables stable minorities to entrench privilege, or whether blocking is episodic and coalition-based.').

omega_variable(
    theater_ratio_interpretation_ambiguity,
    'The measured theater_ratio (0.08, low) assumes the supermajority requirement''s stated function (filtering for consensus) matches its actual function. But what if constitutional actors increasingly invoke the consensus-safeguard framing as cover for blocking change they oppose for partisan or interest-based reasons?',
    'Qualitative analysis of constitutional debate transcripts and expert testimony at amendment moments: how often do actors invoke ''deep consensus'' language, and does that language track the actual empirical consensus on the proposed amendment? If actors invoke it when empirical consensus is shallow or divided, theater_ratio should rise.',
    'Rising theater_ratio (performative consensus-talk replacing actual consensus-filtering) would indicate the constraint is degrading from rope to piton — still maintained, but mostly theatrical, with the real function atrophied. This tracks the mild measurement drift upward (t=0 to t=60) visible in the series and suggests ongoing mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_interpretation_ambiguity, conceptual, 'Whether the consensus-safeguard framing is still functionally grounding the threshold''s operation, or has become rhetorical cover for blocking.').

omega_variable(
    kernel_reading_underspecification,
    'This reading instantiates ''consensus_safeguard_reading'' of the supermajority_threshold kernel. But within this reading, what counts as ''deep, persistent democratic consensus''? Is consensus measured by the size of the supermajority (bigger = deeper)? By durability over time? By diversity of constituents in the coalition? By intensity of preference? The reading does not specify the measurement basis.',
    'Committer frame: this is a conceptual ambiguity internal to the reading''s framing. The adaptive_gradient_reading handles it by claiming the measurement depends on empirical study of consensus-formation rates. The minoritarian_veto_reading handles it by treating ''consensus'' language as rhetorical cover. This reading assumes ''deep consensus'' is a meaningful filter but does not specify what makes it deep. Resolving requires the reading to adopt a metric for consensus depth and defend it against the alternatives.',
    'Without a metric for consensus depth, the reading cannot defend itself against the claim that the supermajority requirement is arbitrary (adaptive) or that it merely blocks minorities from building larger coalitions (minoritarian). This ambiguity is internal to the reading''s structure and cannot be resolved empirically — it requires committer frame clarification of what ''deep, persistent consensus'' means in the reading''s framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underspecification, conceptual, 'The reading frames the supermajority as ensuring ''deep consensus'' but does not specify the measurement basis for consensus depth.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__consensus_safeguard_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement_basis(supe_tr_t0, observed).
narrative_ontology:measurement(supe_tr_t10, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement_basis(supe_tr_t10, observed).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement_basis(supe_tr_t20, observed).
narrative_ontology:measurement(supe_tr_t30, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 30, 0.07).
narrative_ontology:measurement_basis(supe_tr_t30, observed).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 40, 0.075).
narrative_ontology:measurement_basis(supe_tr_t40, observed).
narrative_ontology:measurement(supe_tr_t50, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 50, 0.08).
narrative_ontology:measurement_basis(supe_tr_t50, observed).
narrative_ontology:measurement(supe_tr_t60, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 60, 0.08).
narrative_ontology:measurement_basis(supe_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(supe_be_t0, observed).
narrative_ontology:measurement(supe_be_t10, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement_basis(supe_be_t10, observed).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement_basis(supe_be_t20, observed).
narrative_ontology:measurement(supe_be_t30, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 30, 0.26).
narrative_ontology:measurement_basis(supe_be_t30, observed).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 40, 0.27).
narrative_ontology:measurement_basis(supe_be_t40, observed).
narrative_ontology:measurement(supe_be_t50, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement_basis(supe_be_t50, observed).
narrative_ontology:measurement(supe_be_t60, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 60, 0.28).
narrative_ontology:measurement_basis(supe_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(supe_su_t0, observed).
narrative_ontology:measurement(supe_su_t10, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 10, 0.09).
narrative_ontology:measurement_basis(supe_su_t10, observed).
narrative_ontology:measurement(supe_su_t20, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement_basis(supe_su_t20, observed).
narrative_ontology:measurement(supe_su_t30, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 30, 0.11).
narrative_ontology:measurement_basis(supe_su_t30, observed).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 40, 0.115).
narrative_ontology:measurement_basis(supe_su_t40, observed).
narrative_ontology:measurement(supe_su_t50, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 50, 0.12).
narrative_ontology:measurement_basis(supe_su_t50, observed).
narrative_ontology:measurement(supe_su_t60, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 60, 0.12).
narrative_ontology:measurement_basis(supe_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__consensus_safeguard_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(supermajority_threshold__consensus_safeguard_reading, 0.12).
narrative_ontology:affects_constraint(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold__minoritarian_veto_reading).
narrative_ontology:affects_constraint(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold__adaptive_gradient_reading).

% DUAL FORMULATION NOTE:
% supermajority_threshold is a contested kernel instantiated by three distinct constraint stories: consensus_safeguard_reading (THIS story — low ε, diffuse benefits, rope framing), minoritarian_veto_reading (higher ε, concentrated victims, snare framing), and adaptive_gradient_reading (moderate ε, calibration-dependent, rope-with-conditions framing). Each reading has its own ε value, beneficiary/victim structure, and classification because they make incompatible structural claims about what the mechanism does. They are not three measurements of one constraint — they are three constraints that ride the same constitutional kernel. The sibling readings are authorized via network.affects_constraints links; the three stories together form the constraint family for supermajority_threshold.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
