% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__intermediate_channels
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__intermediate_channels, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: commerce_clause_scope__intermediate_channels
 *   human_readable: Commerce Clause Scope: Intermediate Channels Reading
 *   domain: constitutional/federalism
 *
 * SUMMARY:
 *   The Commerce Clause of the U.S. Constitution grants Congress power to
 *   'regulate Commerce among the States.' This constraint represents one
 *   reading of that clause: federal power extends to three categories—(1)
 *   channels of interstate commerce, (2) instrumentalities and persons/things
 *   in interstate commerce, (3) activities substantially affecting interstate
 *   commerce—but subject to limiting principles: non-economic activity
 *   requires a jurisdictional element, aggregation applies only to economic
 *   activity, and federal authority cannot rest on attenuated causal chains.
 *   This is the intermediate channels reading, articulated most clearly in
 *   United States v. Lopez (1995) and developed through Morrison and
 *   subsequent cases. It positions itself between a narrow originalist
 *   reading (Commerce = removing barriers to trade) and a broad effects test
 *   reading (any substantially affecting activity, economic or not). The
 *   constraint is a functional compromise: federal authority over national
 *   economic issues is preserved and extensive; state authority over family
 *   law, criminal law, and education is protected by categorical exclusion;
 *   but the categories themselves (economic/non-economic,
 *   substantial/attenuated) are manipulable boundaries where the two readings
 *   contend.
 *
 * KEY AGENTS:
 *   - Federal regulatory authority: sets and enforces the three-prong framework via Commerce Clause legislation and interprets its scope
 *   - State police power: retains authority over non-economic conduct but faces federal preemption where federal commerce power validly applies
 *   - Courts: adjudicate whether statutes fit within the three prongs and whether non-economic activity has the required jurisdictional element
 *   - Congress: enacts legislation claiming commerce power authority and defines what counts as economic and substantially affecting
 *   - State legislatures: retain primary authority over reserved domains but pay when federal authority expands the definition of economic activity
 *   - National economic stakeholders: benefit from uniform federal rules for interstate economic conduct
 *   - Intrastate non-economic regulation seekers: excluded from federal reach unless they establish jurisdictional nexus
 *   - Categorical boundary exploiters: actors who exploit the economic/non-economic boundary to argue federal power is either too broad or too narrow
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, 0.58).
domain_priors:suppression_score(commerce_clause_scope__intermediate_channels, 0.41).
domain_priors:theater_ratio(commerce_clause_scope__intermediate_channels, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, extractiveness, 0.58).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__intermediate_channels, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__intermediate_channels, "Commerce Clause Scope: Intermediate Channels Reading").
narrative_ontology:topic_domain(commerce_clause_scope__intermediate_channels, "constitutional/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__intermediate_channels).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__intermediate_channels, '4e56a61c-8b0d-46d5-be6e-a4f031ff1bec').
narrative_ontology:cs_kernel_codification('4e56a61c-8b0d-46d5-be6e-a4f031ff1bec', fixed_text).
narrative_ontology:cs_authority_grounding('4e56a61c-8b0d-46d5-be6e-a4f031ff1bec', lineage).
narrative_ontology:cs_interpretation_layer_present('4e56a61c-8b0d-46d5-be6e-a4f031ff1bec').
narrative_ontology:cs_reading_relation('4e56a61c-8b0d-46d5-be6e-a4f031ff1bec', commerce_clause_scope__narrow_originalist, forecloses).
narrative_ontology:cs_reading_relation('4e56a61c-8b0d-46d5-be6e-a4f031ff1bec', commerce_clause_scope__broad_effects_test, coexists_with).
narrative_ontology:cs_axiom('4e56a61c-8b0d-46d5-be6e-a4f031ff1bec', foundational, categorical_federal_economic_authority).
narrative_ontology:cs_axiom_status(categorical_federal_economic_authority, holdable).
narrative_ontology:cs_axiom_grounding('4e56a61c-8b0d-46d5-be6e-a4f031ff1bec', categorical_federal_economic_authority, conventional).
narrative_ontology:cs_axiom('4e56a61c-8b0d-46d5-be6e-a4f031ff1bec', secondary, limiting_principles_as_workable_constraints).
narrative_ontology:cs_axiom_status(limiting_principles_as_workable_constraints, holdable).
narrative_ontology:cs_axiom_grounding('4e56a61c-8b0d-46d5-be6e-a4f031ff1bec', limiting_principles_as_workable_constraints, conventional).
narrative_ontology:cs_reference_frame('4e56a61c-8b0d-46d5-be6e-a4f031ff1bec', post_lopez_categorical_structure).
narrative_ontology:cs_drift_state('4e56a61c-8b0d-46d5-be6e-a4f031ff1bec', contemporary_2025, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4e56a61c-8b0d-46d5-be6e-a4f031ff1bec', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__intermediate_channels, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, federal_regulatory_authority).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, state_police_power).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, intrastate_non_economic_regulation_seekers).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, categorical_boundary_exploiters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, national_economic_stakeholders).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, state_legislatures).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, state_legislatures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the three-prong Commerce Clause framework. Can regulate (1) channels of interstate commerce without categorical limit, (2) instrumentalities and persons/things in interstate commerce, (3) economic activities substantially affecting interstate commerce in the aggregate. Must not regulate non-economic activity without jurisdictional nexus and cannot rest authority on attenuated causal chains. The constraint defines the field of legitimate federal regulatory authority as extensive within the economic sphere but bounded by categorical limits.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, federal_regulatory_authority, agenda_setter,
    institutional, generational, analytical, national).

% Retains explicit authority over non-economic local conduct: family law (marriage, divorce, child custody, adoption), criminal law (excluding crimes of violence affecting interstate commerce, which Congress can reach), education policy (excluding education services that affect interstate commerce), health and safety regulation (excluding health conditions affecting interstate commerce). The constraint protects state autonomy in these reserved domains by categorically excluding them from federal reach unless a jurisdictional element is established.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, state_police_power, beneficiary,
    institutional, generational, analytical, regional).

% Face uniform federal rules for economic activities substantially affecting interstate commerce in the aggregate. Benefit from predictable federal regulation and the elimination of state-by-state variation in economic rule-setting. Pay the cost of federal regulatory burden but gain the benefit of national coordination of economic affairs.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, national_economic_stakeholders, beneficiary,
    organized, biographical, constrained, national).

% Seek to use federal authority to regulate intrastate non-economic conduct (gun possession, local education standards, family law, moral/religious regulation) because they believe state authority is insufficient or the conduct has interstate spillovers. Excluded from federal regulatory reach under this reading unless they can establish that the conduct occurs in a channel of interstate commerce or involves an instrumentality. Bear the cost of categorical exclusion from federal authority they wish to access.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, intrastate_non_economic_regulation_seekers, payer,
    moderate, biographical, mobile, local).

% Actors (institutional and private) who exploit ambiguities in the economic/non-economic boundary or in the attenuated causal chain limitation to argue federal power is either too restrictive or too expansive. Institutional boundary exploiters (Congress, federal regulatory agencies) seek to characterize non-economic local conduct as economic or to find substantial interstate effects for remote activities. Private boundary exploiters (regulated entities, states challenging federal authority) seek to argue federal authority is overreaching. Bear the cost of the intermediate reading's nominal limits, which can be worked around but are not entirely absent.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, categorical_boundary_exploiters, payer,
    powerful, generational, arbitrage, national).

% Adjudicate whether particular federal statutes fall within the three-prong Commerce Clause framework and apply the categorical limits. Enforce the constraint's internal logic: non-economic activity requires jurisdictional element, aggregation applies only to economic conduct, attenuated causal chains are rejected. Courts benefit from the framework's role-definition (policing boundaries) but also from its inherent ambiguity (ensures ongoing litigation and judicial authority).
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, courts, observer).

% Enacts legislation purporting to regulate interstate commerce under the three-prong framework. Decides what counts as economic (broadly) and what substantially affects interstate commerce in the aggregate (expansively within the economic sphere). Has primary power to define the scope of its own authority, subject to judicial review for fit within the three prongs. The constraint requires Congress to legislate within categorical bounds: channels and instrumentalities have no categorical limit, economic activities with substantial effects can be reached, non-economic local conduct cannot be reached without jurisdictional nexus.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, congress, agenda_setter,
    institutional, generational, analytical, national).

% Retain primary authority over non-economic regulation within their borders but face federal preemption where Congress validly regulates interstate commerce channels, instrumentalities, or substantially-affecting economic activity. Benefit from the nominal reserved domains (family, criminal, education, local moral/health regulation) but pay when federal authority expands the definition of 'economic' or interprets 'substantially affects' generously, permitting federal preemption of state law.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, state_legislatures, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, state_legislatures, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__intermediate_channels, federal_regulatory_authority).
narrative_ontology:fixing_cost_class(commerce_clause_scope__intermediate_channels, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates regulatory jurisdiction between federal and state governments: federal authority coordinates national economic policy (three prongs with extensive reach into interstate commerce and substantially-affecting economic activity), state authority coordinates local non-economic governance (family law, criminal law, education, health and safety in reserved domains). Without the three-prong framework and categorical limits, every regulation would be contested at the jurisdiction line; with it, there is (ostensibly) a workable allocation mechanism, though the boundary is manipulable.
% TRANSFER_FUNCTION: Moves legitimate regulatory authority from states to federal government for (1) any activity in channels of interstate commerce, (2) any use of instrumentalities of interstate commerce, (3) economic activities substantially affecting interstate commerce in the aggregate. States retain authority over non-economic local conduct. The constraint transfers jurisdictional power and the rents that follow from it: federal authority can impose federal regulatory costs on actors, states cannot regulate the same conduct, and actors operating nationally face uniform federal rules rather than state-by-state variation.
% ABSENT_VOICES: Actors seeking to regulate intrastate non-economic conduct via federal authority are excluded—they would object that the categorical exclusion prevents federal solutions to what they frame as national problems (gun violence, education standards, moral regulation with interstate spillovers). Narrow originalist judges and scholars would object that the three-prong framework grants too much federal power and represents a departure from the Constitution's original meaning. Broad effects test advocates would object that the categorical limits are unprincipled and prevent federal regulation of genuinely national problems. The excluded voices represent live alternative readings of the Commerce Clause, not marginal positions.
% DISAPPEARANCE_RATIONALE: If this constraint (the three-prong framework with categorical limits) vanished, the Constitution would still grant federal commerce power, but its scope would require different articulation and enforcement. The narrow originalist reading would point to historical evidence that the Commerce Clause was meant to authorize only the removal of state-imposed barriers to trade. The broad effects test reading would argue any substantially-affecting activity falls within federal reach. Different coalitions of judges, legal scholars, and policy makers would converge on different interpretations of the same constitutional text. The regulatory world would reorganize around competing visions of federalism, and the boundary disputes that the intermediate reading manages (with decreasing effectiveness) would explode into systematic doctrinal disagreement.
% FOUNDING_PROBLEM: The Constitution grants Congress power to 'regulate Commerce among the States' but does not define the boundaries. When does federal authority to regulate interstate commerce reach intrastate conduct? How far can Congress reach into local affairs in the name of national economic coordination without obliterating state autonomy? The pre-1995 broad effects test permitted federal regulation of virtually any activity with a colorable connection to interstate commerce, making the federalism constraint nominal. The intermediate reading's founders (primarily the Lopez majority and subsequent cases) sought to reinstate limiting principles while preserving federal authority over genuine national economic issues.
% FOUNDING_PROBLEM_CORROBORATION: The federal judiciary attests the founding problem is live: United States v. Lopez (1995), United States v. Morrison (2000), National Federation of Independent Business v. Sebelius (2012), and subsequent cases affirm that non-economic activity requires a jurisdictional element, aggregation applies only to economic activity, and attenuated causal chains cannot establish substantial effects. Congress and commentators favoring broad federal authority attest the limiting principles are too strict and prevent addressing national problems. State legislatures and federalism advocates attest the retained reserved domain is essential to constitutional governance. The intermediate reading itself commands support from federal judges and constitutional law scholars, but the limiting principles are increasingly contested as courts and Congress strain against them. No party external to the federalism debate has independently corroborated the founding problem; the problem is internal to constitutional interpretation.
narrative_ontology:disappearance_verdict(commerce_clause_scope__intermediate_channels, contested).
narrative_ontology:founding_problem_status(commerce_clause_scope__intermediate_channels, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__intermediate_channels, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_scope__intermediate_channels, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__intermediate_channels, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__intermediate_channels_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__intermediate_channels_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the constraint both enables federal coordination of national economic affairs and constrains it with categorical limits. The federal authority gains regulatory reach over the entire national economy (high benefit) but forfeits authority over entire domains (family, criminal, non-economic local conduct). States gain clarity about their retained authority but see federal authority expanding into borderline cases and into the economic sphere. Suppression is moderate-low (0.41) because the limiting principles themselves are not routinely violated—courts enforce the categorical structure—but the suppression required is primarily the boundary policing that keeps the limits coherent (rejecting attenuated causal chains, requiring jurisdictional elements for non-economic conduct). Theater is moderate (0.48) and rising slightly (1995–2025 trajectory shows growth from 0.35 to 0.48): the limiting principles are repeatedly cited in opinions but increasingly honored in the breach—Congress legislates under broad substantial effects findings that courts do not closely scrutinize, the economic/non-economic distinction has become unstable under digitalization and modern commerce, and the jurisdictional element requirement has been satisfied by increasingly creative arguments about interstate channels or instrumentalities. The time series captures the intermediate reading's stability as a doctrinal matter from 1995 onward (post-Lopez) while theater increases as the gap between the framework's nominal limits and actual application widens.
 *
 * PERSPECTIVAL GAP:
 *   The federal regulatory authority and courts see this reading as a successful balance: federal power adequate to national economic problems, state power preserved for reserved domains, clear categorical rules. State legislatures and non-economic regulation seekers see it as a federal extraction of jurisdictional authority masked by nominal limits that are routinely circumvented. The categorical boundary exploiters see it as either too restrictive (if they favor broad federal authority) or too expansive (if they favor narrow federal authority). The engine computes different per-seat types from this structural divergence: federal authority computes toward rope (beneficiary of the coordination, beneficiary of the reserved domains), state authority computes toward tangled rope (benefits from the nominal reserved domains but pays as federal authority expands the definition of economic), intrastate non-economic regulation seekers compute toward snare (categorically excluded, bearing the cost of federal authority they wish to access). The reading itself positions moderate extractiveness—not pure coordination (the limits are real constraints) and not pure extraction (federal authority is genuinely limited to three categories and cannot regulate via attenuated causal chains).
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulatory authority: d ≈ 0.2 (beneficiary). Federal authority sets the rules, benefits from the expanded reach (three prongs), and retains analytical exit (can reinterpret the framework through legislation or judicial appointment). State police power: d ≈ 0.5 (symmetric). States benefit from the retained reserved domains and the nominal limits but pay when federal authority expands into the economic sphere or interprets the jurisdictional element loosely. Courts: d ≈ 0.3 (analytical, structural beneficiary of the boundary-maintenance role). Courts benefit from having a framework to apply but also benefit from the framework's inherent ambiguity, which ensures ongoing litigation and judicial authority to police boundaries. National economic stakeholders: d ≈ 0.4 (modest target). They benefit from uniform rules but bear the costs of extensive federal regulation. Intrastate non-economic regulation seekers: d ≈ 0.75 (high target). They are categorically excluded from federal reach; their exit options are to accept state-only authority or to attempt to reframe conduct as economic or to establish a jurisdictional nexus. Categorical boundary exploiters: d ≈ 0.5 (symmetric). They exploit the boundaries in both directions—sometimes arguing federal authority should expand, sometimes that it should contract—and bear the cost of a framework that denies them clean doctrinal footing for their preferred outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint carries the marks of a reading that has partially resolved its founding mandate. The founding problem (1995 onward) was federal Commerce Clause authority unchecked by any limiting principles (the pre-Lopez broad effects test that permitted regulation of non-economic local conduct). The intermediate reading reintroduced limiting principles: the three prongs and the categorical restrictions. The mandate was to preserve state authority over non-economic matters while enabling federal coordination of national economic affairs. The resolution is partial because: (1) the economic/non-economic boundary has eroded under modern commerce—data flows, digital services, and platform economics blur the categories; (2) Congress legislates under broad substantial effects findings that courts do not closely scrutinize, so the limiting principles are more nominal than enforced; (3) the jurisdictional element requirement for non-economic conduct has been satisfied by increasingly creative jurisdictional nexus arguments, permitting federal reach into local conduct that nominally falls outside the three prongs. The theater_ratio rising from 0.35 to 0.48 captures this partial erosion: the limiting principles are still invoked but increasingly as theater—cited in opinions but not decisive in outcomes. The constraint has not fully resolved its mandate because the underlying tension between national economic coordination and state non-economic autonomy remains. The reading persists as a working framework that preserves federal power in the economic sphere while nominally protecting state authority in reserved domains, but the boundary between economic and non-economic has become incoherent under conditions the reading's architects (1995 jurisprudence) did not anticipate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_non_economic_boundary_incoherence,
    'Is the distinction between economic and non-economic activity a stable, principled line, or is it an unstable category increasingly eroded by modern commerce and interdependencies?',
    'Systematic analysis of Commerce Clause cases and Congressional findings: if courts and Congress frequently reclassify conduct from non-economic to economic as economic conditions change (e.g., home-grown wheat, student loan interest, healthcare services), the boundary is incoherent.',
    'If incoherent, the limiting principle that non-economic activity requires jurisdictional element becomes a category error—the premise of the distinction is false. The constraint collapses toward the broad effects test reading (no categorical distinction). If coherent, the intermediate reading''s framework is salvageable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_non_economic_boundary_incoherence, empirical, 'Whether the economic/non-economic boundary is a workable limit or a manipulable fiction.').

omega_variable(
    attenuated_causal_chain_enforcement,
    'Are courts actually enforcing the limiting principle that federal authority cannot rest on attenuated causal chains, or has this principle become merely ornamental language while Congress legislates under increasingly remote substantial effects findings?',
    'Meta-analysis of post-Lopez Commerce Clause opinions: count how many federal statutes were struck down for attenuated causation vs. how many were upheld despite attenuated causal chains. If the ratio is heavily skewed toward upholding (10+ upheld for every 1 struck down), enforcement is nominal.',
    'If enforcement is nominal, the limiting principle is theater—cited but not binding. The intermediate reading''s legal force rests on the courts'' willingness to police the boundaries; if courts abandon or rarely apply the enforcement mechanism, the reading collapses toward the broad effects test.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attenuated_causal_chain_enforcement, empirical, 'Whether the attenuated causal chain limit is actually enforced or merely cited as cover.').

omega_variable(
    jurisdictional_element_doctrinal_drift,
    'Are courts accepting increasingly creative and remote ''jurisdictional elements'' (connections to channels or instrumentalities of interstate commerce) that functionally permit federal regulation of intrastate non-economic conduct?',
    'Review of post-Lopez cases finding jurisdictional elements: if courts find sufficient nexus to interstate commerce channels/instrumentalities for conduct that is nominally local and non-economic (e.g., gun possession in a schoolyard affecting interstate education services), the doctrine is drifting.',
    'If drift is substantial, the nominal protection of non-economic local conduct is eroding. The intermediate reading''s retained state authority is being hollowed out by doctrinal expansion of what counts as a sufficient nexus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_element_doctrinal_drift, empirical, 'Whether jurisdictional element requirements are tightening or loosening over time.').

omega_variable(
    reading_foreclosure_via_axiom_override,
    'Which sibling reading does this intermediate reading logically foreclose, if any? Does the narrow originalist reading (Commerce = removing barriers) contradict the intermediate reading''s three-prong structure, or do they coexist as incompatible but live interpretive traditions?',
    'Doctrinal analysis: the narrow originalist reading denies that the Commerce Clause grants affirmative federal power to regulate (only power to prevent state barrier-erection). The intermediate reading grants affirmative power to the three categories. These are contradictory premises. A single framework cannot hold both—a judge cannot simultaneously believe the Clause grants power to regulate channels and believe it grants no such power. This is FORECLOSES.',
    'If the narrow originalist reading forecloses this intermediate reading, they are genuine alternatives, not coexisting traditions. Courts commit to one or the other; legislatures operating under one framework deny the other''s legitimacy. If they coexist, the constraint is unstable because both readings claim to interpret the same constitutional text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_via_axiom_override, conceptual, 'Whether the intermediate reading forecloses or coexists with the narrow originalist reading.').

omega_variable(
    committer_axiom_contestation,
    'The intermediate reading rests on the foundational axiom that federal authority can be extensive within the economic sphere but limited to that sphere (categorical_federal_economic_authority). Is this axiom genuinely holdable as constitutional law, or has it been overridden by doctrinal developments that deny the distinction''s workability?',
    'Examine whether contemporary constitutional scholarship, judicial opinions, and congressional practices still treat the economic/non-economic distinction as a meaningful limit, or whether it is now viewed as a failed project. If courts stop citing Lopez and its progeny, or explicitly overrule them, the axiom is overridden.',
    'If overridden, this reading''s foundational premise is no longer maintainable. The reading itself becomes a historical artifact (intermediate reading that was tried but failed). If still holdable, the axiom remains live despite contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_axiom_contestation, conceptual, 'Whether the foundational axiom of categorical federal economic authority remains holdable or has been overridden.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__intermediate_channels, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_scope__intermediate_channels, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(comm_tr_t2000, commerce_clause_scope__intermediate_channels, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_scope__intermediate_channels, theater_ratio, 2005, 0.42).
narrative_ontology:measurement(comm_tr_t2010, commerce_clause_scope__intermediate_channels, theater_ratio, 2010, 0.45).
narrative_ontology:measurement(comm_tr_t2015, commerce_clause_scope__intermediate_channels, theater_ratio, 2015, 0.47).
narrative_ontology:measurement(comm_tr_t2020, commerce_clause_scope__intermediate_channels, theater_ratio, 2020, 0.48).
narrative_ontology:measurement(comm_tr_t2025, commerce_clause_scope__intermediate_channels, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(comm_be_t1995, commerce_clause_scope__intermediate_channels, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_scope__intermediate_channels, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_scope__intermediate_channels, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(comm_be_t2010, commerce_clause_scope__intermediate_channels, base_extractiveness, 2010, 0.57).
narrative_ontology:measurement(comm_be_t2015, commerce_clause_scope__intermediate_channels, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(comm_be_t2020, commerce_clause_scope__intermediate_channels, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement(comm_be_t2025, commerce_clause_scope__intermediate_channels, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1995, commerce_clause_scope__intermediate_channels, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement(comm_su_t2000, commerce_clause_scope__intermediate_channels, suppression_requirement, 2000, 0.39).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_scope__intermediate_channels, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(comm_su_t2010, commerce_clause_scope__intermediate_channels, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(comm_su_t2015, commerce_clause_scope__intermediate_channels, suppression_requirement, 2015, 0.41).
narrative_ontology:measurement(comm_su_t2020, commerce_clause_scope__intermediate_channels, suppression_requirement, 2020, 0.41).
narrative_ontology:measurement(comm_su_t2025, commerce_clause_scope__intermediate_channels, suppression_requirement, 2025, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__intermediate_channels, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__intermediate_channels, 0.12).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__broad_effects_test).

% DUAL FORMULATION NOTE:
% The commerce_clause_scope kernel decomposes into three constraint stories, each representing a distinct reading of the Commerce Clause's proper scope. The intermediate_channels reading describes federal power reaching (1) channels of interstate commerce, (2) instrumentalities and persons/things in interstate commerce, (3) activities substantially affecting interstate commerce, subject to categorical limits. The narrow_originalist reading describes federal power as limited to removing state-imposed barriers to interstate trade. The broad_effects_test reading describes federal power as extending to any substantially affecting activity with no categorical distinction. All three read the same constitutional text; they differ in what the text permits. They are linked by network.affects_constraints to show doctrinal interdependence: the intermediate reading influences both siblings by establishing the baseline three-prong structure that all subsequent doctrinal development must either accept or reject.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
