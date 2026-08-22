% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__consensus_safeguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold_consensus_safeguard_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: supermajority_threshold__consensus_safeguard_reading
 *   human_readable: Supermajority Amendment Threshold (Consensus Safeguard Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The supermajority amendment threshold is a constitutional rule requiring
 *   that amendments to the foundational text command supermajority (typically
 *   2/3 or 3/5) rather than simple-majority support. This reading frames the
 *   threshold as a democratic safeguard: it filters amendments so that only
 *   those commanding broad, durable consensus—persisting across multiple
 *   electoral cycles and demographic shifts—become constitutional law. The
 *   reading treats the threshold as a mechanism for distinguishing genuine
 *   constitutional change from transient majoritarian passion. Competing
 *   readings (the minoritarian-veto reading and the adaptive-gradient
 *   reading) contest this framing: they argue the threshold either entrenches
 *   historical power asymmetries against majoritarian will, or that its
 *   legitimacy depends on empirical evidence that it actually tracks real
 *   consensus formation. This story instantiates ONLY the consensus-safeguard
 *   reading; the sibling readings are separate constraint stories linked by
 *   network effects.
 *
 * KEY AGENTS:
 *   - Constitutional frame beneficiaries: courts, institutional actors, constitutional scholars whose authority derives from constitutional continuity
 *   - Institutional stability holders: government institutions whose legitimacy depends on durable constitutional structure
 *   - Minority interest holders: groups protected by constitutional limitations (2/3 holding veto against simple-majority amendment)
 *   - Transient majoritarian movements: political coalitions commanding majority but not supermajority support, blocked from amending constitution
 *   - Amendment drafters: political actors who directly experience the threshold's filtering effect
 *   - Constitutional courts: agenda-setters who administer and interpret the amendment process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__consensus_safeguard_reading, 0.31).
domain_priors:suppression_score(supermajority_threshold__consensus_safeguard_reading, 0.18).
domain_priors:theater_ratio(supermajority_threshold__consensus_safeguard_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__consensus_safeguard_reading, rope).
narrative_ontology:human_readable(supermajority_threshold__consensus_safeguard_reading, "Supermajority Amendment Threshold (Consensus Safeguard Reading)").
narrative_ontology:topic_domain(supermajority_threshold__consensus_safeguard_reading, "constitutional/political").

domain_priors:requires_active_enforcement(supermajority_threshold__consensus_safeguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__consensus_safeguard_reading, 'bbdd1404-34d0-4fe6-a836-68609111a04d').
narrative_ontology:cs_kernel_codification('bbdd1404-34d0-4fe6-a836-68609111a04d', formalized).
narrative_ontology:cs_authority_grounding('bbdd1404-34d0-4fe6-a836-68609111a04d', lineage).
narrative_ontology:cs_interpretation_layer_present('bbdd1404-34d0-4fe6-a836-68609111a04d').
narrative_ontology:cs_reading_relation('bbdd1404-34d0-4fe6-a836-68609111a04d', supermajority_threshold__minoritarian_veto_reading, coexists_with).
narrative_ontology:cs_reading_relation('bbdd1404-34d0-4fe6-a836-68609111a04d', supermajority_threshold__adaptive_gradient_reading, coexists_with).
narrative_ontology:cs_axiom('bbdd1404-34d0-4fe6-a836-68609111a04d', foundational, supermajority_filters_for_deep_consensus).
narrative_ontology:cs_axiom_status(supermajority_filters_for_deep_consensus, holdable).
narrative_ontology:cs_axiom_grounding('bbdd1404-34d0-4fe6-a836-68609111a04d', supermajority_filters_for_deep_consensus, instrumental).
narrative_ontology:cs_axiom('bbdd1404-34d0-4fe6-a836-68609111a04d', foundational, constitutional_durability_requires_resistant_amendment_process).
narrative_ontology:cs_axiom_status(constitutional_durability_requires_resistant_amendment_process, holdable).
narrative_ontology:cs_axiom_grounding('bbdd1404-34d0-4fe6-a836-68609111a04d', constitutional_durability_requires_resistant_amendment_process, deontological).
narrative_ontology:cs_reference_frame('bbdd1404-34d0-4fe6-a836-68609111a04d', democratic_consensus_requirement).
narrative_ontology:cs_drift_state('bbdd1404-34d0-4fe6-a836-68609111a04d', contemporary_polarization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bbdd1404-34d0-4fe6-a836-68609111a04d', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, constitutional_continuity).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, institutional_stability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, institutional_continuity_holders).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, minority_interest_holders).
narrative_ontology:constraint_victim(supermajority_threshold__consensus_safeguard_reading, minority_interest_holders).
narrative_ontology:constraint_victim(supermajority_threshold__consensus_safeguard_reading, transient_majoritarian_movements).
narrative_ontology:constraint_vindicates(supermajority_threshold__consensus_safeguard_reading, democratic_quality_premium_doctrine).
narrative_ontology:constraint_vindicates(supermajority_threshold__consensus_safeguard_reading, temporal_consistency_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constitutional scholars, courts, and institutional theory advocates who benefit from the threshold because it protects the constitutional frame itself as a stable reference for all subsequent interpretation and governance. The supermajority requirement makes the foundational text durable, which allows interpretation and application to proceed on a stable baseline. They do not collect material gains but benefit from the constraint's role in preserving the framework within which their own authority and expertise are grounded.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, framers_legitimacy_coalition, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(supermajority_threshold__consensus_safeguard_reading, framers_legitimacy_coalition).

% Courts, legislatures, executive branches derive legitimacy and structure from the constitution. The supermajority threshold ensures that the constitutional rules governing their authority and jurisdiction are resistant to revision. They benefit from being able to plan long-term institutional strategy without fear that transient political movements will rewrite the constitutional rules of the game. This allows stable governance architecture across electoral cycles.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, institutional_continuity_holders, beneficiary,
    institutional, generational, mobile, national).

% Groups holding minority rights or minority constitutional protections (religious minorities, regional minorities, historically subordinated groups whose rights are constitutionally entrenched) find those protections shielded from simple-majority amendment by the supermajority threshold. They pay a cost when they have majority support for a new amendment (unable to amend against supermajority opposition); they benefit when threatened by majoritarian pressure to strip their rights (their protections are hard to amend away). Net effect depends on whether amendment pressure comes from their direction or against them.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, minority_interest_holders, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__consensus_safeguard_reading, minority_interest_holders, beneficiary).

% Political coalitions commanding 50–65% voter support but unable to reach supermajority consensus face the amendment threshold as a blocking point. They can propose amendments but cannot enact them without either building broader consensus (time-consuming, difficult, uncertain) or reinterpreting the existing constitution through legislation and courts (indirect, weaker). Their options are constrained to formal amendment channels or creative ordinary-law reinterpretation; revolutionary or extra-constitutional change is available but illegal and delegitimizing.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, transient_majoritarian_movements, payer,
    powerful, biographical, constrained, national).

% Groups whose demographic weight or social influence grows over time (immigrant populations, religious communities expanding, generational cohorts with different values) find themselves unable to amend constitutional structures designed for different demographics until they can form supermajority consensus. They would object that they have no voice in the initial amendment decision and are locked out unless they reach the supermajority bar themselves. Structurally excluded from amendment-drafting conversations until their numbers are sufficient.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, demographic_change_constituencies, excluded,
    powerful, generational, constrained, national).

% Courts administer the amendment process: verify supermajority votes, interpret which changes count as formal amendments vs. ordinary interpretation, manage the constitutional courts' own role in amendment processes. They set the operational agenda for how the threshold functions. They decide hard cases (Does this procedure meet the supermajority requirement? Is this a valid amendment or merely judicial overreach?) and their decisions define the threshold's effective stringency.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Political actors drafting constitutional amendments—legislators, reform movements, activists—directly experience the threshold's filtering effect. They know which amendments failed to achieve supermajority, which succeeded, and what coalition-building efforts were required. They observe whether supermajority coalitions persist across electoral cycles or dissolve. Their testimony and historical record are empirical evidence for evaluating whether the threshold actually filters for deep consensus.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, amendment_drafters, observer,
    organized, biographical, mobile, national).

% Scholars and philosophers theorizing democracy, consensus, and constitutional legitimacy provide frameworks for evaluating whether the threshold ensures what it claims. They assess fit between the legitimation story ('deep persistent consensus') and the constraint's actual operation. They compete over whether the threshold is a safeguard or an entrenchment mechanism. Their work informs constitutional interpretation and amendment strategy.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, democratic_theorists, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__consensus_safeguard_reading, diffuse).
narrative_ontology:fixing_cost_class(supermajority_threshold__consensus_safeguard_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a collective-action problem in constitutional governance: how to distinguish genuine constitutional consensus (persisting across electoral cycles, coalitions, demographic change) from transient majority passion that would later be regretted or reversed. The supermajority requirement coordinates time-binding: it forces consensus to persist across multiple electoral windows, raising confidence that the amendment reflects stable preference rather than temporary alignment.
% TRANSFER_FUNCTION: Transfers amendment authority away from simple majoritarian decision-making and toward supermajority consensus-building. The 'move' is not material goods but political power: the right to unilaterally reshape the constitutional frame is transferred from any coalition commanding 50%+1 to coalitions commanding (typically) 66% or more. Beneficiaries of existing constitutional structures retain blocking power longer; those seeking constitutional change face higher coalition-building burdens.
% ABSENT_VOICES: Demographic groups whose growth over time outpaces their ability to form supermajorities are structurally excluded from amendment-drafting conversations until their numbers are large enough to meet the threshold. Extra-constitutional actors (revolutionary movements, populist coalitions excluded by the ordinary political system) are excluded by definition. Their objection would be: 'The threshold protects a constitutional frame we did not author and that does not represent us, while preventing us from amending it unless we command supermajority consensus—a consensus filter that works against our interests specifically because we are a growing minority.'
% DISAPPEARANCE_RATIONALE: If the supermajority amendment threshold disappeared overnight—if any constitutional change required only simple majority approval—the constitution would become rapidly revisable. Minority protections would face majoritarian pressure; institutional structures would shift as political movements could constitutionally enshrine their platforms; the constitutional text would accumulate amendments tracking electoral cycles rather than deep consensus. The entire framework of constitutional governance, premised on durability and resistance to transience, would reorganize around continuous contestation and revision.
% FOUNDING_PROBLEM: In democracies without supermajority amendment thresholds, constitutional design becomes prey to majoritarian waves: simple-majority movements reshape fundamental law in ways they later regret once the majority shifts. The problem is temporal: majorities are durable across electoral cycles, but not across demographic or ideological change. The supermajority threshold was designed to filter amendments so that only those commanding broad, durable consensus persist.
% FOUNDING_PROBLEM_CORROBORATION: The constraint's beneficiaries—constitutional scholars, institutional stability advocates, and courts invested in constitutional durability—attest the founding problem is live and the threshold addresses it effectively. Critics and amendment advocates counter that the founding problem is overstated: most constitutional amendments in established democracies are stable and rarely reversed (suggesting simple-majority filter works well in practice), and that the threshold creates its own problems (blocking needed reforms, entrenching minority power, preventing demographic representation). Independent political scientists document that reversals of constitutional amendments are rare in either system (high-threshold vs. low-threshold democracies), which contests the claim that simple majority leads to instability.
narrative_ontology:disappearance_verdict(supermajority_threshold__consensus_safeguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__consensus_safeguard_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__consensus_safeguard_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(supermajority_threshold__consensus_safeguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__consensus_safeguard_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.31 at interval end, trending upward mid-interval to 0.35 around t=40) because the threshold imposes real costs on majority movements seeking constitutional change, but does not extract for the benefit of a concentrated beneficiary: the gains accrue to 'constitutional continuity' and 'institutional stability'—diffuse, non-agent propositions. The constraint does concentrate benefits on existing institutional structures and those invested in constitutional durability; this is extraction in the sense that transient majorities bear a cost (inability to amend), but not in the sense of concentrated, zero-sum rent collection. Suppression is low (0.12–0.19) because the constraint's enforcement is procedural and transparent—the courts simply count votes—not coercive or internalized. Theater ratio rises mid-interval (t=20–40) and falls again (t=50) because the threshold's legitimation story becomes more elaborate under pressure: as blocking occurs and majorities are denied amendment, the narrative emphasizing 'deep consensus' and 'transient passion' intensifies (theater), but when amendment eventually succeeds (later in the interval, allowing t=50 to show lower theater as consensus has been demonstrated), the performance subsides. Accessibility collapse is high (0.68) because once supermajority is the rule, simple-majority alternatives are effectively inaccessible—the threshold is the formal rule, and defecting from it requires constitutional change itself (a circular trap). Resistance is moderate (0.52) because the constraint meets real opposition from majority movements and amendment advocates, but the opposition is channeled into orderly constitutional reform efforts rather than extra-constitutional resistance; the threshold is defended by courts and constitutional continuity advocates, so resistance persists without overwhelming enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of institutional stability holders (courts, established government branches), the threshold is a genuine coordination solution to a real problem: preventing constitutional instability from majoritarian waves. From the seat of transient majoritarian movements (political coalitions with 50–65% support), the same constraint operates as an arbitrary blocking mechanism imposed by existing institutional actors to entrench their power. The gap is structural: the institutional seat benefits from predictability and durability; the majoritarian seat benefits from responsiveness and change. The engine computes this divergence from the stakeholder structure (one institutional agenda-setter, multiple political power-holders at the beneficiary/payer boundary) without adjudicating which perspective is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional seat (courts, amendment administrators) has d near 0.0–0.2 (beneficiary: controls the rule, administers it, benefits from stability it ensures). Transient majoritarian movements have d near 0.7–0.85 (target: blocked from amending, must either form supermajorities or accept the status quo). Minority interest holders have d near 0.5 (symmetric: they benefit from the veto when threatened, bear cost when aligned with the excluded majority). Institutional stability holders have d near 0.1–0.3 (partial beneficiary: their legitimacy depends on constitutional durability, but they do not directly capture material gain from the threshold). The override derivation would typically assign high d to the majority-blocking position automatically; no override needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing constitutional instability from majoritarian revision) is CONTESTED: the constraint's beneficiaries attest it is live and the threshold addresses it; critics counter that constitutional reversals are rare in both high-threshold and low-threshold systems, suggesting the problem is overstated. This contest is the crucial input to mandatrophy assessment. If the founding problem is dead (constitutional instability from simple-majority amendment does not occur empirically), then the constraint has become a pure extraction mechanism: it imposes the cost of blocked amendment on majority movements, while conferring the benefit of protected continuity on existing institutional structures, but it solves a problem that no longer exists. The measurement series (rising extractiveness mid-interval, peaking at t=40, then declining) is consistent with this narrative: extractiveness rises as amendment pressure builds and blocking intensifies (visible as theater ratio climbing); extractiveness falls again if amendment eventually succeeds or blocking is abandoned, reducing the felt cost to majorities. Theater ratio remaining positive even at interval end (0.22) suggests the legitimation story persists even when blocking occurs, which is a sign of either institutional resilience or inertial maintenance of a zombie constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_filter_empirical_validity,
    'Does the supermajority threshold actually filter amendments so that only those commanding deep, durable consensus persist? Or do supermajority coalitions form and dissolve as readily as simple majorities, making the threshold a timing device rather than a consensus-quality filter?',
    'Comparative institutional analysis: measure (a) amendment reversal rates in supermajority-threshold systems vs. simple-majority systems over 50+ year periods, (b) coalition stability and persistence across electoral cycles for amendments that succeeded vs. failed to meet supermajority, (c) whether supermajorities that form to amend persist across demographic/ideological change.',
    'If supermajority coalitions are stable (persist across cycles, rarely reversed), the consensus-filter reading is empirically supported; if supermajority coalitions dissolve as readily as simple majorities, the filter claim is false, and the threshold is better classified as pure extraction (blocking without consensus benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_filter_empirical_validity, empirical, 'Whether supermajority requirement actually ensures durable consensus or merely extends coalition-building timescale.').

omega_variable(
    transient_passion_definition_ambiguity,
    'What counts as ''transient majoritarian passion'' vs. ''deep persistent consensus''? Is it a temporal property (how long the majority persists), a coalition property (how durable the coalition remains), or a normative property (whether the underlying preference is stable across value changes)?',
    'Normative analysis with empirical input: determine whether ''transience'' is defined by (a) persistence-across-electoral-cycles (5-10 year threshold?), (b) persistence-across-demographic-replacement (20-30 year threshold?), (c) reversibility-if-contexts-change, or (d) normative entrenchment-in-identity-and-values. Each definition produces different empirical verdict.',
    'If transience = electoral-cycle persistence, most amendments fail the test (requiring supermajority does select for amendments lasting 1–2 election cycles); if transience = generational persistence, the test becomes much harder to satisfy, and the threshold may be overly restrictive. Ambiguity in the definition means the filter''s actual function is unclear.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transient_passion_definition_ambiguity, conceptual, 'The conceptual boundary between transient and durable consensus is undefined, making the filter''s success criteria indeterminate.').

omega_variable(
    entrenchment_vs_safeguard_framing,
    'Is the supermajority threshold a safeguard that protects constitutional durability and filters low-quality amendments? Or is it an entrenchment mechanism that locks in the distribution of power that exists when the threshold is set, making it impossible for majorities to revise it later?',
    'Normative analysis: examine whether the threshold''s primary function is to filter (make better amendments more likely by requiring consensus) or to entrench (make any amendment harder, thereby preserving the status quo regardless of its quality). Empirical signal: if the threshold is frequently criticized by majority movements seeking amendment, and if the status quo it preserves is internally contested, the entrenchment framing gains support.',
    'The consensus-safeguard reading treats the threshold as filtering function; the minoritarian-veto reading treats it as entrenchment. This is not a resolvable empirical disagreement—it is a disagreement about the threshold''s primary purpose and design intention. The omega documents that this reading has chosen to emphasize safeguarding; a different reading emphasizes entrenchment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(entrenchment_vs_safeguard_framing, preference, 'Whether supermajority requirement is understood as quality filter or as entrenchment mechanism—a choice of frame, not resolvable empirically.').

omega_variable(
    minority_protection_vs_minority_veto,
    'Does the supermajority threshold protect constitutional minorities (those holding minority rights or minority-held constitutional protections) or does it empower blocking minorities (those holding the blocking veto, which may or may not be the same as constitutional minorities)?',
    'Historical analysis: identify which groups held supermajority-veto power at different periods in the constitution''s history. Determine whether those groups were the same as those holding constitutional-minority protections (religious minorities, political minorities, regional minorities). If veto-wielders ≠ protected-minorities, the threshold has protected entrenchment, not minority rights.',
    'If blocking minorities and protected minorities are the same, the threshold serves both functions; if they differ, the threshold primarily protects whichever group controls the veto. This determines whether the constraint''s beneficiary structure is genuine (protecting minorities) or actually extractive (protecting a different minority from majority pressure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_vs_minority_veto, empirical, 'Whether supermajority veto protects constitutional minorities or merely blocks revision.').

omega_variable(
    reading_contest_contestability,
    'This is one reading of a contested kernel. The other readings (minoritarian-veto, adaptive-gradient) would author different base_properties.extractiveness and victims sets from the same institutional rule. What structural fact would prove one reading correct and the others wrong?',
    'This omega documents the irreducibility of the contest: the readings differ in their normative frame (is the threshold a safeguard or entrenchment?), their empirical hypothesis (does supermajority actually filter for consensus?), and their beneficiary assignment (who benefits—constitutional continuity or blocking minorities?). The engine will compute per-seat classifications; where those diverge, that divergence is the measurement the corpus takes. Resolving the reading contest is not the engine''s job—it is a matter of constitutional theory and political philosophy.',
    'This omega flags that the consensus-safeguard reading is one among three coherent framings of the same institutional rule. The fact that different readings produce different constraint classifications is not a failure—it is the system functioning as designed, capturing structural indeterminacy in the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_contestability, conceptual, 'Kernel reading contestability: multiple structurally coherent readings produce different constraint classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__consensus_safeguard_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(smjty_consensus_tr_t0, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(smjty_consensus_tr_t0, observed).
narrative_ontology:measurement(smjty_consensus_tr_t10, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(smjty_consensus_tr_t10, observed).
narrative_ontology:measurement(smjty_consensus_tr_t20, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(smjty_consensus_tr_t20, observed).
narrative_ontology:measurement(smjty_consensus_tr_t30, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement_basis(smjty_consensus_tr_t30, observed).
narrative_ontology:measurement(smjty_consensus_tr_t40, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(smjty_consensus_tr_t40, observed).
narrative_ontology:measurement(smjty_consensus_tr_t50, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement_basis(smjty_consensus_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(smjty_consensus_be_t0, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(smjty_consensus_be_t0, observed).
narrative_ontology:measurement(smjty_consensus_be_t10, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement_basis(smjty_consensus_be_t10, observed).
narrative_ontology:measurement(smjty_consensus_be_t20, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement_basis(smjty_consensus_be_t20, observed).
narrative_ontology:measurement(smjty_consensus_be_t30, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 30, 0.33).
narrative_ontology:measurement_basis(smjty_consensus_be_t30, observed).
narrative_ontology:measurement(smjty_consensus_be_t40, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement_basis(smjty_consensus_be_t40, observed).
narrative_ontology:measurement(smjty_consensus_be_t50, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 50, 0.31).
narrative_ontology:measurement_basis(smjty_consensus_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(smjty_consensus_su_t0, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(smjty_consensus_su_t0, observed).
narrative_ontology:measurement(smjty_consensus_su_t10, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 10, 0.14).
narrative_ontology:measurement_basis(smjty_consensus_su_t10, observed).
narrative_ontology:measurement(smjty_consensus_su_t20, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 20, 0.16).
narrative_ontology:measurement_basis(smjty_consensus_su_t20, observed).
narrative_ontology:measurement(smjty_consensus_su_t30, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 30, 0.18).
narrative_ontology:measurement_basis(smjty_consensus_su_t30, observed).
narrative_ontology:measurement(smjty_consensus_su_t40, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 40, 0.19).
narrative_ontology:measurement_basis(smjty_consensus_su_t40, observed).
narrative_ontology:measurement(smjty_consensus_su_t50, supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 50, 0.18).
narrative_ontology:measurement_basis(smjty_consensus_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__consensus_safeguard_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(supermajority_threshold__consensus_safeguard_reading, 0.12).
narrative_ontology:affects_constraint(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold__minoritarian_veto_reading).
narrative_ontology:affects_constraint(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold__adaptive_gradient_reading).

% DUAL FORMULATION NOTE:
% Supermajority threshold kernel decomposes into three structurally distinct constraint readings: (1) consensus_safeguard_reading (this story) — emphasizes democratic-quality filtering and constitutional durability; (2) minoritarian_veto_reading — emphasizes blocking power and entrenchment against majority will; (3) adaptive_gradient_reading — emphasizes empirical calibration and reversibility costs. Each reading instantiates different ε (low for consensus safeguard, moderate-to-high for veto, context-dependent for adaptive). Sibling stories linked via network.affects_constraints. The three readings coexist as live positions in constitutional theory; none logically forecloses the others within established jurisprudence, though partisans dispute which reading best captures the threshold's actual function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
