% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__procedural_integrity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__procedural_integrity_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: refugee_convention_text__procedural_integrity_reading
 *   human_readable: Refugee Convention as Procedural Integrity Safeguard
 *   domain: international_law/migration/human_rights
 *
 * SUMMARY:
 *   The 1951 Refugee Convention establishes a binding procedural requirement:
 *   states must conduct fair, individualized assessment of asylum claims
 *   before refusing them, provide reasons for refusal, and permit appeal.
 *   This constraint embodies one reading of the Convention's text—the
 *   procedural integrity reading—which treats the Convention primarily as a
 *   safeguard ensuring process integrity and fair hearing, while permitting
 *   states considerable latitude in defining the substantive protection
 *   threshold ('well-founded fear,' 'particular social group'). The reading's
 *   core claim is that procedure is non-negotiable even when outcomes may be
 *   narrow. This differs from the expansive humanitarian reading (Convention
 *   as broad humanitarian mandate regardless of procedure) and the
 *   restrictive sovereignty reading (Convention as minimum floor, states
 *   retain discretion to narrow or suspend procedure). The constraint's
 *   extractiveness is moderate (0.42) because it both benefits asylum seekers
 *   with procedural access and extracts from them by permitting narrow
 *   substantive thresholds; it benefits high-capacity states that can afford
 *   procedural machinery and extracts from low-capacity states that cannot;
 *   it benefits the international legal order by enforcing constraint on
 *   state action while extracting legitimacy-cost from states that prefer
 *   discretion.
 *
 * KEY AGENTS:
 *   - asylum_seekers_procedurally_protected: beneficiary of fair hearing right; trapped (cannot exit asylum seeking); zero alternative to seeking protection, must endure procedure
 *   - asylum_seekers_excluded_by_narrow_procedure: victim of substantive exclusion within the procedure; same trapped exit as protected group but receive 'no' at the threshold stage
 *   - states_with_processing_capacity: agenda-setter and partial beneficiary; they design the procedure and benefit from legitimacy their refusals gain; they bear the cost of machinery
 *   - states_with_limited_processing_capacity: victim; capacity-trapped, must comply with procedure they cannot afford or escape via treaty exit (constrained exit)
 *   - regional_refugee_conferences: beneficiary (coordination enabled by procedure); mobile exit (can choose to participate or withdraw from agreements)
 *   - offshore_processing_authorities: agenda-setter in location but payer in mandate (must run full procedure regardless of site)
 *   - international_legal_order: non-agent beneficiary; vindicated proposition (procedure as foundational right)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, 0.42).
domain_priors:suppression_score(refugee_convention_text__procedural_integrity_reading, 0.38).
domain_priors:theater_ratio(refugee_convention_text__procedural_integrity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__procedural_integrity_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__procedural_integrity_reading, "Refugee Convention as Procedural Integrity Safeguard").
narrative_ontology:topic_domain(refugee_convention_text__procedural_integrity_reading, "international_law/migration/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__procedural_integrity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__procedural_integrity_reading, '69c15a03-bf13-4aa5-8c96-be0ee31066e1').
narrative_ontology:cs_kernel_codification('69c15a03-bf13-4aa5-8c96-be0ee31066e1', fixed_text).
narrative_ontology:cs_authority_grounding('69c15a03-bf13-4aa5-8c96-be0ee31066e1', lineage).
narrative_ontology:cs_interpretation_layer_present('69c15a03-bf13-4aa5-8c96-be0ee31066e1').
narrative_ontology:cs_reading_relation('69c15a03-bf13-4aa5-8c96-be0ee31066e1', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('69c15a03-bf13-4aa5-8c96-be0ee31066e1', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('69c15a03-bf13-4aa5-8c96-be0ee31066e1', foundational, procedural_integrity_non_negotiable).
narrative_ontology:cs_axiom_status(procedural_integrity_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('69c15a03-bf13-4aa5-8c96-be0ee31066e1', procedural_integrity_non_negotiable, deontological).
narrative_ontology:cs_axiom('69c15a03-bf13-4aa5-8c96-be0ee31066e1', foundational, substantive_threshold_state_definable).
narrative_ontology:cs_axiom_status(substantive_threshold_state_definable, holdable).
narrative_ontology:cs_axiom_grounding('69c15a03-bf13-4aa5-8c96-be0ee31066e1', substantive_threshold_state_definable, conventional).
narrative_ontology:cs_reference_frame('69c15a03-bf13-4aa5-8c96-be0ee31066e1', fair_individualized_assessment_requirement).
narrative_ontology:cs_drift_state('69c15a03-bf13-4aa5-8c96-be0ee31066e1', contemporary_narrowed_thresholds, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('69c15a03-bf13-4aa5-8c96-be0ee31066e1', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__procedural_integrity_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, asylum_seekers_procedurally_protected).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, international_legal_order).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, asylum_seekers_excluded_by_narrow_procedure).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, states_with_limited_processing_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, regional_refugee_conferences).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, asylum_seekers_procedurally_protected).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, offshore_processing_authorities).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, procedural_due_process_as_foundational_right).
narrative_ontology:constraint_vindicates(refugee_convention_text__procedural_integrity_reading, individualized_assessment_non_waivable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive binding access to fair, individualized assessment of their refugee claim before any refusal can stand. They are entitled to oral hearing, evidence presentation, reasoned decision, and appeal regardless of the claim's ultimate merit. They also bear the constraint's cost: processing delays, administrative burden, uncertainty during adjudication, and the continued exposure that lengthy procedures can impose.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, asylum_seekers_procedurally_protected, beneficiary,
    powerless, immediate, trapped, universal).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, asylum_seekers_procedurally_protected, payer).

% Set and enforce procedural standards: they design interview protocols, evidence thresholds, appeal mechanisms, and determine what counts as 'well-founded fear' within the frame of individualized assessment. They benefit from the legitimacy the procedure confers on their refusals—a procedurally sound 'no' is harder to challenge than an arbitrary one. They bear the cost of running the machinery: trained adjudicators, legal review, appeals infrastructure, and the extended timelines these safeguards require.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, states_with_processing_capacity, agenda_setter,
    institutional, generational, arbitrage, national).

% Face non-negotiable procedural requirements they lack capacity to meet: they cannot afford trained asylum adjudicators, written reasoned decisions, or meaningful appeal mechanisms, yet the reading permits no waiver of substantive review. They may narrow the definition of 'well-founded fear' or 'particular social group' within the procedural frame, but cannot skip the assessment itself. The constraint thus creates a capacity trap: meet the procedure or violate treaty obligations.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, states_with_limited_processing_capacity, payer,
    powerful, generational, constrained, national).

% Receive full procedural access but in a system where states have legally narrowed the threshold—'well-founded fear' requires individualized persecution proof, 'particular social group' is restricted to immutable characteristics with state awareness—such that their claims fail at the substantive stage of the procedure. They get process but no protection; the reading's guarantee is procedural integrity, not outcome. They are in the victim set because the narrowing occurs within a procedurally-governed frame, making the exclusion formally lawful but substantively extractive.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, asylum_seekers_excluded_by_narrow_procedure, payer,
    powerless, immediate, trapped, universal).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, asylum_seekers_excluded_by_narrow_procedure, excluded).

% Coordinate refugee burden-sharing on the basis of comparable procedural standards—states trust each other's procedures and can safely accept transfers and responsibility-sharing agreements. The procedural guarantee is what makes regional arrangements fungible; without it, states would refuse to accept transfers to neighbors with weak review capacity.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, regional_refugee_conferences, beneficiary,
    institutional, generational, mobile, regional).

% Operate processing centers outside the territory of the state accepting refugees, yet remain bound by full procedural requirements: interview, legal representation, evidence, reasoned decision, appeal. The reading permits geographic relocation of processing but not procedural downgrade. They must staff and fund the machinery regardless of location, and accept ongoing judicial review of their decisions.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, offshore_processing_authorities, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, offshore_processing_authorities, payer).

% Argue the procedural reading is insufficient—procedure is meaningless if the substantive threshold (what counts as persecution) has been narrowed to near-zero by state definition. They would advocate for the broader humanitarian reading, which guarantees not just procedure but also an expansive scope (generalized violence, clan persecution, gender-based harm). They are excluded from the agenda-setting of procedural design; their objection is audible but does not reframe the constraint.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, humanitarian_advocates, excluded,
    organized, generational, mobile, global).

% Argue states should have discretion to waive procedure in emergencies or for security reasons, treating the Convention as a floor, not a ceiling. They would advocate for the restrictive reading, which permits states to narrow both definition and procedure. They oppose the procedural integrity claim as over-constraining state choice.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, sovereignty_advocates, excluded,
    institutional, generational, mobile, national).

% The system of treaties and dispute resolution that depends on procedural consistency. When states honor procedural commitments even when outcomes disfavor their interests, the legal order's credibility is reinforced. When states nominally comply but hollow out the procedure, the order erodes. The procedural integrity reading vindicates the non-waivable nature of due process as foundational to international legality itself.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, international_legal_order, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(refugee_convention_text__procedural_integrity_reading, international_legal_order).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__procedural_integrity_reading, states_with_processing_capacity).
narrative_ontology:fixing_cost_class(refugee_convention_text__procedural_integrity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common, reciprocally-accepted procedure for refugee status determination that allows states to coordinate burden-sharing and trust each other's decisions. The shared procedure replaces the coordination cost of bilateral verification—state A can accept transfers from state B's processing centers because both follow identical procedural standards.
% TRANSFER_FUNCTION: Transfers the burden of refugee intake from states with high arrival volumes to states with greater capacity, and transfers the burden of procedural compliance from states with weak institutions to those with stronger ones, via responsibility-sharing and regional processing agreements—all predicated on procedural parity.
% ABSENT_VOICES: Sovereignty-maximalist states that would prefer to retain unilateral discretion over refugee admissions and have no procedural obligation; humanitarian advocates who believe procedure without substantive protection is a cover story for exclusion; developing states with minimal state capacity who find the procedural requirement impossible to meet and who are not in the conversation when the reading is operationalized in high-income countries' policy.
% DISAPPEARANCE_RATIONALE: If the procedural integrity constraint vanished, states would revert to unilateral refusal authority with no obligation to hear the applicant, provide reasons, or accept review. Asylum seekers would have no enforceable right to individualized assessment. Regional refugee conferences would collapse—state A would not trust state B's decisions without procedural guarantees. The international legal order would lose a core instance of binding procedural constraint on state action. The constraint's disappearance would be a major geopolitical rearrangement toward absolute state discretion in borders.
% FOUNDING_PROBLEM: Post-1951, states had no mutual assurance that refugee determination decisions were made fairly or consistently. One state's refusal was another state's potential refugee influx. The Convention's procedural requirement solved this by making 'fair assessment' the condition under which states would accept each other's decisions and coordinate refugee responsibility. Without procedure as the common standard, no interstate coordination was possible.
% FOUNDING_PROBLEM_CORROBORATION: The International Court of Justice and the UN High Commissioner for Refugees attest to the continuing-live status of the procedure-as-coordination problem: states persistently seek to offshore processing while narrowing procedural safeguards, and the resulting disputes (Nairobi regional conferences, Australian offshore processing litigation, U.S. asylum rule changes) show states trying to escape the procedural constraint. The founding coordination problem remains live because the escape attempts persist. However, sovereignty advocates and developing-state governments attest the procedure is now a substantive burden impeding refugee intake, and humanitarian advocates attest the procedure is a hollow formality when states narrow the substantive threshold below meaningful protection. The dispute is over whether the founding problem persists or has inverted.
narrative_ontology:disappearance_verdict(refugee_convention_text__procedural_integrity_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__procedural_integrity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__procedural_integrity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(refugee_convention_text__procedural_integrity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__procedural_integrity_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__procedural_integrity_reading_tests).
:- end_tests(refugee_convention_text__procedural_integrity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.28 to 0.42 over the interval because states progressively narrow the substantive threshold—'well-founded fear' becomes more demanding, 'particular social group' shrinks to immutable characteristics—while maintaining the procedural frame. The procedure becomes a gateway to an ever-narrower substantive gate. Suppression increases (0.22 to 0.38) because maintaining the procedural frame against the escape attempts of states and the objections of humanitarian advocates requires active enforcement: litigation against offshore processing without full procedure, rejection of 'safe country' expedited-refusal regimes, insistence on appellate review. Theater rises modestly (0.10 to 0.22) as procedural compliance becomes increasingly formal—states file reasoned decisions and appeal mechanisms but design them to maximize refusal within the legal frame. The measurements use one shared time grid: every metric is authored at every examined point (0, 5, 10, 15, 20, 25). The temporal story is one of progressive narrowing-within-procedure: the constraint holds its procedural core but the substantive protection it guarantees erodes as states exploit the reading's willingness to separate procedure from outcome.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (states with capacity) should perceive this as rope or mild tangled_rope: they coordinate burden-sharing, run legitimate procedures, and can narrow outcomes within the legal frame. They see the extractiveness as moderate cost for legitimacy. The payer seats perceive it differently: capacity-constrained states see entrapment (procedure they cannot afford, narrowed outcomes anyway); asylum seekers excluded by procedure see the constraint as snare dressed in procedural language. The engine should compute per-seat type divergence: the same constraint looks like tangled_rope from the capacity-rich state seat and snare from the capacity-constrained seat, because their exit options and power atoms differ. The claim/metric gap is intentional: the constraint is CLAIMED as procedural-safeguard (emphasis on coordination, integrity, fairness) while the authored metrics emphasize the extractiveness of narrowed outcomes and the suppression required to maintain the procedural frame against escape attempts. This gap is where the measurement the constraint story exists to take occurs: does the procedural framing hide extraction, or does it genuinely enable coordination despite narrow outcomes?
 *
 * DIRECTIONALITY LOGIC:
 *   The procedural integrity reading creates asymmetric extraction because it requires procedure (benefiting asylum seekers) but permits substantive narrowing (harming them at the threshold stage). From the agenda-setter seat (states with capacity), the constraint is largely beneficial—they can afford procedure and use it to legitimate refusals. From the victim seat (asylum seekers excluded by narrow procedure, capacity-constrained states), the constraint extracts: it requires machinery but permits the results to narrow to near-zero. The reading's structural claim—'procedure is non-negotiable, outcome is flexible'—means beneficiaries gain process but may lose protection; payers lose both resources (to run procedure) and substantive protection (because the procedure frames a narrowable outcome). Directionality for asylum_seekers is complex: beneficiary role (procedure guarantees hearing) and payer role (constrained by narrow threshold) coexist in the same agent. The engine's derivation should identify this: trapped exit makes them a high-d target despite procedural-access benefits; the exit-derived d (high) should dominate the beneficiary-derived d (low), resulting in net-target directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids simple mandatrophy classification (mandate dead but arrangement persists) because the founding coordination problem—states need mutual assurance of fair assessment to coordinate burden-sharing—remains live. The procedural reading does not claim the founding problem has been solved; it claims procedure-as-coordination is the solution that remains valid even when states dispute its scope. However, the reading is vulnerable to a mandatrophy charge from the humanitarian critique: if the substantive protection threshold has narrowed to near-zero, the procedure may have become a stage set for predetermined refusal, and the 'coordination' may be coordination-in-refusal rather than genuine mutual burden-sharing. The resolution distinguishes between functional mandatrophy (the procedure no longer enables actual burden-sharing because substantive narrowing has foreclosed protection) and structural mandatrophy (the procedure is legally required but almost never invoked because states have found workarounds). Evidence for functional mandatrophy: developing countries' objection that procedure traps them, humanitarian advocates' claim that procedure-without-protection is theatricality. Evidence against functional mandatrophy: the ICJ continues to enforce procedural obligations, states continue to litigate over procedure (indicating it has real cost), and burden-sharing conferences continue to reference procedural parity as the basis for responsibility agreements. The theater_ratio trend (rising but staying below 0.25) supports the reading against mandatrophy: if the procedure were pure theater, theater_ratio should approach 0.5–0.7 (equal performance and function). At 0.22, procedure is still mostly doing real work, even as substantive narrowing erodes protection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedure_outcome_independence,
    'Can procedure be truly non-negotiable while outcome remains flexible? Is this a genuine distinction or a separation that collapses under pressure?',
    'Examine cases where states maintain full procedure but narrow outcome to near-zero (e.g., gender-based persecution excluded from ''particular social group''). If asylum seekers appeal procedurally-compliant narrow refusals and courts overturn them on substantive grounds, procedure and outcome are not independent; if courts uphold them, they are separable.',
    'If procedure and outcome are independent, the reading stands: procedure guarantees fair assessment even if result is refusal. If they collapse (narrow outcome reveals procedure as formalism), the reading collapses into snare-disguised-as-rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedure_outcome_independence, empirical, 'Whether the procedural/outcome distinction holds under stress.').

omega_variable(
    capacity_trap_versus_constraint,
    'Does the procedural requirement trap states with limited capacity into non-compliance, or does it function as a valid constraint they must meet?',
    'Study implementation in low-capacity states: do they proportionately withdraw from the Convention, or do they attempt compliance with lower-quality procedures? Do international bodies accept ''we cannot afford full procedure'' as a defense?',
    'If capacity-trapped states are permitted to withdraw or use simplified procedure, the constraint is not actually non-negotiable—it becomes negotiable for the powerless. If they must comply or are forced to try, the constraint is genuinely non-negotiable but distributively unfair.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_trap_versus_constraint, empirical, 'Whether the procedural requirement is genuinely non-negotiable or negotiable under capacity constraints.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the procedural_integrity_reading foreclose the expansive_humanitarian_reading, or do they coexist as competing interpretations?',
    'Examine state practice: do states holding the procedural reading formally reject parties claiming the humanitarian reading, or do both readings remain available as live choices in different jurisdictions?',
    'If foreclosed: the procedural reading is logically stronger and the humanitarian reading is defensible only by rejecting part of the kernel''s authority. If coexisting: both remain live and the contestation is not resolvable by logic—it requires political choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether the procedural and humanitarian readings are logically incompatible or merely different.').

omega_variable(
    extraction_via_narrowed_threshold,
    'Is the rising extractiveness (0.28 to 0.42) due to procedural suppression of escape attempts, or to states'' increasing use of narrow-threshold substantive gates within the procedural frame?',
    'Decompose extractiveness measures: track separately (1) suppression cost (enforcement of procedure against state exit attempts) and (2) substantive-narrowing cost (asylum claims denied under stated procedure). If (2) dominates growth, extraction is outcome-driven not procedure-driven.',
    'If procedure-driven: the constraint''s extractiveness reflects the cost of maintaining fair process and may be justified as coordination cost. If outcome-driven: the constraint''s extractiveness reflects states'' success in using procedure to legitimize narrow refusal, and the reading is serving as cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_via_narrowed_threshold, empirical, 'Whether rising extractiveness reflects procedural enforcement or substantive narrowing.').

omega_variable(
    reading_kernel_authority_grounding,
    'What authority grounds the procedural_integrity_reading''s claim to represent the Convention''s true meaning—treaty text, state practice, international court interpretation, or humanitarian tradition?',
    'Trace the genealogy of the reading: which states, courts, and scholarly traditions authored it? Does it rest on text-as-written, or on an interpretive tradition that may have overwritten the text?',
    'If text-grounded: the reading is stable and contestation is about application. If tradition-grounded: the reading may be overridden by a shift in the interpretive community (e.g., courts shift to humanitarian reading, or states collectively endorse sovereignty reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_authority_grounding, conceptual, 'What grounds the authority of the procedural interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__procedural_integrity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t0, refugee_convention_text__procedural_integrity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(refu_tr_t0, observed).
narrative_ontology:measurement(refu_tr_t5, refugee_convention_text__procedural_integrity_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement_basis(refu_tr_t5, observed).
narrative_ontology:measurement(refu_tr_t10, refugee_convention_text__procedural_integrity_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(refu_tr_t10, observed).
narrative_ontology:measurement(refu_tr_t15, refugee_convention_text__procedural_integrity_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement_basis(refu_tr_t15, observed).
narrative_ontology:measurement(refu_tr_t20, refugee_convention_text__procedural_integrity_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(refu_tr_t20, observed).
narrative_ontology:measurement(refu_tr_t25, refugee_convention_text__procedural_integrity_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(refu_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(refu_be_t0, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(refu_be_t0, observed).
narrative_ontology:measurement(refu_be_t5, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(refu_be_t5, observed).
narrative_ontology:measurement(refu_be_t10, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement_basis(refu_be_t10, observed).
narrative_ontology:measurement(refu_be_t15, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 15, 0.41).
narrative_ontology:measurement_basis(refu_be_t15, observed).
narrative_ontology:measurement(refu_be_t20, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(refu_be_t20, observed).
narrative_ontology:measurement(refu_be_t25, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement_basis(refu_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t0, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(refu_su_t0, observed).
narrative_ontology:measurement(refu_su_t5, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 5, 0.26).
narrative_ontology:measurement_basis(refu_su_t5, observed).
narrative_ontology:measurement(refu_su_t10, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 10, 0.31).
narrative_ontology:measurement_basis(refu_su_t10, observed).
narrative_ontology:measurement(refu_su_t15, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement_basis(refu_su_t15, observed).
narrative_ontology:measurement(refu_su_t20, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement_basis(refu_su_t20, observed).
narrative_ontology:measurement(refu_su_t25, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement_basis(refu_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__procedural_integrity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__procedural_integrity_reading, 0.18).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_status_determination_capacity).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, offshore_processing_governance).

% DUAL FORMULATION NOTE:
% The refugee_convention_text kernel decomposes into three constraint stories, one per sibling reading. The procedural_integrity_reading (this constraint) treats the Convention as anchoring procedural non-negotiability with flexible outcome. The expansive_humanitarian_reading anchors substantive scope with required procedure. The restrictive_sovereignty_reading anchors state discretion with minimum procedure as floor. These three constraints share a referent (the Convention text itself) but model different ε values because the readings attribute different structural weight to procedure, outcome, and state discretion. The three stories are linked via network.affects_constraints; a shift in the interpretive community toward one reading will alter the structural environment of the others (influences relation). No reading forecloses the others—they coexist as competing institutional choices held by different state coalitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(refugee_convention_text__procedural_integrity_reading, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
