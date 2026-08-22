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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: commerce_clause_scope__intermediate_channels
 *   human_readable: Commerce Clause Intermediate Channels Reading
 *   domain: constitutional/federalism
 *
 * SUMMARY:
 *   The intermediate-channels reading of the Commerce Clause instantiates a
 *   doctrine that Federal power extends to (1) channels of interstate
 *   commerce, (2) instrumentalities and persons/things in interstate
 *   commerce, and (3) activities substantially affecting interstate commerce,
 *   subject to limiting principles: non-economic activity requires a
 *   jurisdictional element, aggregation applies only to economic activity,
 *   and excessively attenuated causal chains defeat the substantial-effects
 *   test. This reading sits between narrow originalism (commerce means direct
 *   interstate trade) and broad effects test (commerce includes any economic
 *   activity with spillovers). The intermediate reading coordinates national
 *   economic regulation with federalism by maintaining categorical
 *   boundaries: economic activities are regulable if substantially affecting
 *   interstate commerce; non-economic activities are excluded from federal
 *   reach unless connected to an interstate channel. These boundaries are
 *   doctrinally asserted to be stable limiting principles but are empirically
 *   unstable, creating extractive pressure on the principle of state
 *   sovereignty and doctrinal coherence.
 *
 * KEY AGENTS:
 *   - Federal legislative authority: sets the agenda, administers the economic/non-economic distinction, determines substantial-effects threshold
 *   - State regulatory authority: constrained by federal preemption in economic spheres, preserved in non-economic domains
 *   - Supreme Court: maintains the doctrine, refines boundaries through landmark decisions
 *   - Lower federal courts: apply limiting principles case-by-case
 *   - Doctrinal coherence and state sovereignty boundary: pay the extraction cost via manipulable distinctions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, 0.58).
domain_priors:suppression_score(commerce_clause_scope__intermediate_channels, 0.42).
domain_priors:theater_ratio(commerce_clause_scope__intermediate_channels, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, extractiveness, 0.58).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__intermediate_channels, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__intermediate_channels, "Commerce Clause Intermediate Channels Reading").
narrative_ontology:topic_domain(commerce_clause_scope__intermediate_channels, "constitutional/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__intermediate_channels).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__intermediate_channels, 'f2311829-c907-4e0e-a06b-1f53db0a1d8c').
narrative_ontology:cs_kernel_codification('f2311829-c907-4e0e-a06b-1f53db0a1d8c', fixed_text).
narrative_ontology:cs_authority_grounding('f2311829-c907-4e0e-a06b-1f53db0a1d8c', lineage).
narrative_ontology:cs_interpretation_layer_present('f2311829-c907-4e0e-a06b-1f53db0a1d8c').
narrative_ontology:cs_reading_relation('f2311829-c907-4e0e-a06b-1f53db0a1d8c', commerce_clause_scope__narrow_originalist, coexists_with).
narrative_ontology:cs_reading_relation('f2311829-c907-4e0e-a06b-1f53db0a1d8c', commerce_clause_scope__broad_effects_test, coexists_with).
narrative_ontology:cs_axiom('f2311829-c907-4e0e-a06b-1f53db0a1d8c', foundational, categorical_limits_bind_federal_commerce_power).
narrative_ontology:cs_axiom_status(categorical_limits_bind_federal_commerce_power, holdable).
narrative_ontology:cs_axiom_grounding('f2311829-c907-4e0e-a06b-1f53db0a1d8c', categorical_limits_bind_federal_commerce_power, deontological).
narrative_ontology:cs_axiom('f2311829-c907-4e0e-a06b-1f53db0a1d8c', foundational, federalism_structure_preserved_via_non_economic_carve_out).
narrative_ontology:cs_axiom_status(federalism_structure_preserved_via_non_economic_carve_out, holdable).
narrative_ontology:cs_axiom_grounding('f2311829-c907-4e0e-a06b-1f53db0a1d8c', federalism_structure_preserved_via_non_economic_carve_out, deontological).
narrative_ontology:cs_reference_frame('f2311829-c907-4e0e-a06b-1f53db0a1d8c', national_economic_coordination_federalism_balance).
narrative_ontology:cs_drift_state('f2311829-c907-4e0e-a06b-1f53db0a1d8c', contemporary_regulatory_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f2311829-c907-4e0e-a06b-1f53db0a1d8c', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__intermediate_channels, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, federal_legislative_authority).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, national_uniform_commerce_regulation).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, state_autonomy_in_local_spheres).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, doctrinal_coherence).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, state_sovereignty_boundary).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, state_regulatory_authority).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, state_regulatory_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Congress exercises regulatory authority over channels of interstate commerce (highways, shipping routes, telecommunications networks), instrumentalities in use in interstate commerce (railroads, airlines, telecommunications carriers), and intrastate economic activities substantially affecting interstate commerce in the aggregate. Can regulate the economic activity itself but not non-economic local conduct without a jurisdictional nexus (e.g., cannot directly regulate school zone firearms possession without showing the activity substantially affects interstate commerce). Administers the doctrinal line between economic and non-economic activity, and determines what counts as substantially affecting interstate commerce.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, federal_legislative_authority, agenda_setter,
    institutional, generational, analytical, national).

% Retains police powers over family law, criminal law, local land use, education, and health/safety in traditional domains. Cannot regulate interstate commerce directly or discriminate against interstate commerce. Experiences federal preemption in economic spheres where Congress acts under Commerce Clause authority. Benefits from the categorical exclusion of non-economic local conduct (family law, criminal procedure, education curriculum) from federal reach, but pays the cost of excluded authority in economic and regulatory matters crossing state lines.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, state_regulatory_authority, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, state_regulatory_authority, beneficiary).

% The institutional goal of uniform national rules for economic activity crossing state lines. Benefits from the intermediate reading because it permits federal regulation of economic activities substantially affecting interstate commerce while respecting state autonomy over local non-economic conduct. The coordination function of the Commerce Clause doctrine is to enable national coordination on economic issues while preserving federalism space.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, national_uniform_commerce_regulation, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__intermediate_channels, national_uniform_commerce_regulation).

% The institutional goal of state authority over matters of local concern not substantially affecting interstate commerce. Benefits from the categorical exclusion of non-economic activity (family law, education, criminal procedure) from federal Commerce Clause reach, preserving federalism structure and subsidiarity.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, state_autonomy_in_local_spheres, beneficiary,
    analytical, generational, analytical, regional).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__intermediate_channels, state_autonomy_in_local_spheres).

% The constraint that doctrinal categories must track consistent principles across cases. The intermediate reading requires maintaining an economic/non-economic distinction and an aggregation principle that applies only to economic activity. These distinctions are unstable: whether an activity is economic depends on framing (is education economic? does it substantially affect interstate commerce?); the aggregation principle can be manipulated (low threshold for what counts as substantial effect). The doctrine bears the extraction cost of maintaining these unstable boundaries.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, doctrinal_coherence, payer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__intermediate_channels, doctrinal_coherence).

% The principle that states retain a domain of sovereignty separate from federal authority. The intermediate reading asserts this boundary exists via categorical limits (non-economic activity is excluded unless jurisdictional element present). But the boundary is manipulable: what counts as substantially affecting interstate commerce, whether aggregation is appropriate, and what degree of attenuation defeats the causal chain are judgment calls that can shift the boundary without changing the stated doctrine.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, state_sovereignty_boundary, payer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(commerce_clause_scope__intermediate_channels, state_sovereignty_boundary).

% Apply the intermediate reading in cases challenging federal regulatory authority. Must determine whether an activity is economic, whether it substantially affects interstate commerce, and whether the nexus is too attenuated. Produce the case-by-case body of law that interprets the limiting principles and applies them to novel regulatory claims.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, lower_federal_courts, observer,
    institutional, biographical, constrained, regional).

% Establishes and revises the intermediate reading through landmark decisions (Wickard, Raich, Morrison, Lopez). Clarifies the boundaries of federal power, refines what counts as economic/non-economic activity, and determines the degree of attenuation that defeats the substantial-effects test. The Court's doctrine-setting authority is the engine through which this reading is maintained and evolved.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% State governments, federalism advocates, and civil liberties organizations argue that Congress has used the substantial-effects test to regulate non-economic activity without genuine jurisdictional showing (school zone firearms, Violence Against Women Act gender-motivated crimes, drug possession in hospitals). They are excluded from the rule-making process but participate in litigation and lobbying to constrain the intermediate reading toward narrow originalism.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, congress_regulatory_overreach_advocates, excluded,
    organized, biographical, constrained, national).

% Federal agencies, congressional committees, and civil rights advocates argue that federal regulatory authority should extend further—to non-economic activity with national spillovers (climate, gun violence, discrimination), and that the limiting principles (non-economic carve-out, aggregation limitation, attenuation doctrine) are inconsistent with the functional reach of interstate commerce. They argue for a broader reading but are constrained by Court doctrine.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, commerce_expansion_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a doctrinal framework enabling Congress to coordinate uniform national economic regulation while preserving state authority over local non-economic matters. Solves the coordination problem of how to permit national rules for interstate commerce without collapsing federalism: channels and instrumentalities have a direct interstate nexus (low coordination cost); economic activity is regulated if substantially affecting interstate commerce (moderate coordination cost via aggregation principle); non-economic activity is categorically excluded unless a specific jurisdictional element connects it to interstate commerce (preserves federalism boundary).
% TRANSFER_FUNCTION: Moves regulatory authority from states to Congress in economic spheres where interstate commerce is affected; preserves state authority in non-economic local spheres. States pay the cost of preemption in economic matters; Congress and national interests benefit from uniform national rules. The transfer is masked as categorical distinction (economic/non-economic) rather than acknowledged as a power allocation.
% ABSENT_VOICES: State legislatures excluded from federal rule-making but can lobby and litigate; federalism advocates excluded from doctrinal design; civil rights advocates seeking to expand federal reach are constrained by Court doctrine; non-governmental entities affected by federal regulation (business, labor, environmental groups) participate through litigation and administrative comment but do not set the federal agenda.
% DISAPPEARANCE_RATIONALE: If the intermediate reading disappeared overnight, the world would rearrange: either Congress would operate under narrow originalism (federal power dramatically shrinks to direct channels and instrumentalities, states regain regulatory space) or under broad effects test (federal power dramatically expands to reach non-economic activity with spillovers, federalism collapses). The intermediate reading is the standing arrangement; its disappearance would force rapid reallocation of regulatory authority.
% FOUNDING_PROBLEM: The need to permit Congress to establish uniform national rules for interstate commerce (preventing balkanization, solving coordination failures on tariffs and trade barriers) while preserving federalism structure and state authority over local matters. The Commerce Clause must coordinate national economic regulation without swallowing all state sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: The intermediate reading is justified by citing Wickard's aggregation principle and the national market for agricultural commodities (1942), and by the necessity of federal coordination on interstate commerce and national economic policy. Critics argue the founding problem has been solved (balkanization is not a live threat given modern markets and the Supremacy Clause) and the doctrine now operates as cover for expansive federal power. Federalism scholars from outside the benefiting parties attest the founding problem was real in 1787–1942 but is now substantially solved; judicial decisions (Lopez, Morrison) from the Court itself suggest the founding problem's salience has declined.
narrative_ontology:disappearance_verdict(commerce_clause_scope__intermediate_channels, contested).
narrative_ontology:founding_problem_status(commerce_clause_scope__intermediate_channels, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__intermediate_channels, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.58) because the intermediate reading permits substantial federal regulatory reach in economic matters (high extraction in that domain) but preserves state authority in non-economic matters (low extraction in that domain). The mean reflects the hybrid structure. Suppression is moderate (0.42) because the limiting principles are judicially enforced (Some suppression of broader federal power) but Congress regularly tests the boundaries and lower courts allow federal jurisdiction to reach far into intrastate economic activity (suppression is not tight). Theater ratio is low-to-moderate (0.28) because the doctrinal machinery has real limiting function but a growing share of the discourse concerns whether the limits are meaningful—whether economic/non-economic distinction actually constrains federal power or merely provides rhetorical cover. The measurement series show extraction and theater rising from time 0 to 35 (expansion of federal reach under broad effects reasoning, increasing skepticism about whether limits bind) and then declining at time 40 (Lopez and Morrison decisions reverse the trend by striking down federal statutes as non-economic activity lacking jurisdictional element, reasserting the limiting principles). This cyclical pattern reflects the tension between doctrinal stability and functional reach: periods of expansion are followed by Court correction, which reasserts the categorical limits and temporarily stabilizes the doctrine.
 *
 * PERSPECTIVAL GAP:
 *   The federal legislative seat and the state regulatory seat compute differently because they occupy opposite structural positions relative to the constraint. Congress administers the categorical distinctions and benefits from broad reach in economic matters; states are constrained by preemption in economic matters and preserved in non-economic matters. The intermediate reading's stability depends on maintaining the economic/non-economic distinction as a real limiting principle. If the distinction is stable and binding, the constraint is a genuine coordination mechanism (rope) with federalism safeguards. If the distinction is unstable and manipulable, the constraint is extractive (snare or tangled-rope) with theatrical limiting principles. The measurement series (extraction rising to t=35, then declining) suggests the distinction has been unstable and expanding, followed by Court correction at t=35+ (Lopez, Morrison) that reasserts the limits. This cyclical pattern is itself a signal of the gap: the doctrine oscillates between expansion and correction because the underlying categorical principle cannot hold steady.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal legislative authority is the agenda-setter: it benefits from expansive Commerce Clause authority and administers the limiting principles. State regulatory authority is the payer: constrained by preemption in economic spheres, benefits from categorical exclusion of non-economic activity. The apparent beneficiaries (national uniform commerce regulation, state autonomy in local spheres) are not agents but vindicated propositions—institutional goals the doctrine claims to serve. The true victims are doctrinal coherence and the state sovereignty boundary: they pay the cost of the unstable economic/non-economic distinction and the manipulable substantial-effects test. From the federal legislative perspective (d near 0.0, beneficiary end), this is a coordination mechanism enabling national economic policy. From the state perspective (d near 1.0 for preempted economic matters, lower d for preserved non-economic authority), the same structure is extractive in economic spheres and autonomy-preserving in others. The engine should compute different types per seat: federal sees rope or tangled-rope coordination; state sees snare in preempted economic matters, rope in preserved non-economic matters.
 *
 * MANDATROPHY ANALYSIS:
 *   The intermediate reading is susceptible to mandatrophy because the founding problem (enabling national economic regulation while preserving federalism) may be solved or functionally obsolete. The founding problem arose in 1787–1940 when state balkanization of commerce was a live threat and federal coordination was necessary for national markets. By 2026, balkanization is not a live threat (modern supply chains are integrated nationally; the Dormant Commerce Clause and Supremacy Clause already prevent discriminatory state regulation). The categorical limits are maintained theatrically rather than functionally: the Supreme Court occasionally strikes down federal statutes (Lopez, Morrison) to signal that limits exist, but Congress routinely stretches the substantial-effects test and lower courts permit jurisdiction to reach far into intrastate activity. The doctrine has not collapsed (it is still cited and followed) but the functional force of the limiting principles has atrophied. This is the hallmark of mandatrophy: a constraint maintained by periodic performance (Supreme Court decisions reasserting limits) rather than by structural necessity. The intermediate reading persists because the alternative readings (narrow originalism would require wholesale redesign of federal regulatory authority; broad effects test would formally eliminate federalism limits) are politically costly. So the intermediate reading persists by theater—occasional decisions striking down statutes, frequent doctrinal reaffirmations—while functional federal reach continues to expand.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_noneconomic_stability,
    'Is the economic/non-economic distinction a stable limiting principle that reliably constrains federal authority, or is it a manipulable category whose boundaries shift with political pressure and doctrinal framing?',
    'Longitudinal analysis of Commerce Clause cases: do courts consistently apply the economic/non-economic distinction, or do they reclassify activities (gun possession, healthcare mandates, environmental protection) as economic when Congress wishes to regulate them?',
    'If stable, the intermediate reading is a genuine coordination mechanism with real federalism safeguards; if unstable, the categorical limit is theatrical and the constraint is extractive. Classification would shift from tangled-rope (hybrid coordination/extraction with limits that sometimes bind) to snare (extraction masked as categorical constraint).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_noneconomic_stability, empirical, 'Whether the economic/non-economic distinction constrains federal power or provides rhetorical cover for expansive authority').

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem (need for federal coordination on national markets while preserving federalism) still live, or has it been solved by modern markets, the Supremacy Clause, and the Dormant Commerce Clause?',
    'Historical analysis: would eliminating the intermediate reading and reverting to narrow originalism cause detectable harms (state balkanization, tariff barriers, coordination failures) that require federal intervention? Or would modern institutional arrangements (integrated national supply chains, administrative coordination, international trade law) handle coordination without federal Commerce Clause authority?',
    'If the founding problem is solved, the constraint meets mandatrophy: federal authority persists via periodic theater (Supreme Court decisions reasserting limits) rather than functional necessity. Classification would shift from tangled-rope to piton (degraded coordination maintained by inertia and performance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding problem is still live or has been functionally solved').

omega_variable(
    limiting_principle_manipulation,
    'Can Congress and lower courts manipulate the aggregation principle, attenuation doctrine, and jurisdictional element requirement to reach activities Congress wishes to regulate, while maintaining the appearance that limiting principles constrain federal authority?',
    'Case-by-case analysis: does Congress regularly test the limits by making jurisdictional findings and aggregation arguments that courts accept? Or do courts reject federal authority when the limiting principles are genuinely exceeded?',
    'If limiting principles are manipulable, the suppression metric understates actual federal reach (the constraint does not suppress federal authority as much as the measurement suggests). The theater ratio understates the performative maintenance of limits. Classification would shift toward snare (effective suppression is lower than measured; the limits are cover rather than constraint).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(limiting_principle_manipulation, empirical, 'Whether limiting principles are reliable constraints or manipulable covers for federal expansion').

omega_variable(
    sibling_reading_coexistence,
    'Can the intermediate reading coexist as a live reading of the Commerce Clause kernel, or does it logically collapse into either narrow originalism or broad effects test under sufficient doctrinal pressure?',
    'Constitutional theory: if the economic/non-economic distinction is unstable and the aggregation principle is manipulable, does the intermediate reading constitute a stable alternative, or is it a transitional position that eventually bifurcates into its sibling readings?',
    'If coexistence is stable, the three readings represent genuinely different constitutional visions that can be held by different judges/scholars. If the intermediate reading is unstable, it may foreclose itself via its own internal contradictions, collapsing into one of the sibling readings (broad effects test) as the categorical limits erode under political pressure and case-by-case manipulation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether the intermediate reading is a stable constitutional position or a transitional stance between the polar readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__intermediate_channels, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_scope__intermediate_channels, theater_ratio, 0, 0.12).
narrative_ontology:measurement(comm_tr_t5, commerce_clause_scope__intermediate_channels, theater_ratio, 5, 0.15).
narrative_ontology:measurement(comm_tr_t10, commerce_clause_scope__intermediate_channels, theater_ratio, 10, 0.18).
narrative_ontology:measurement(comm_tr_t15, commerce_clause_scope__intermediate_channels, theater_ratio, 15, 0.21).
narrative_ontology:measurement(comm_tr_t20, commerce_clause_scope__intermediate_channels, theater_ratio, 20, 0.24).
narrative_ontology:measurement(comm_tr_t25, commerce_clause_scope__intermediate_channels, theater_ratio, 25, 0.27).
narrative_ontology:measurement(comm_tr_t30, commerce_clause_scope__intermediate_channels, theater_ratio, 30, 0.31).
narrative_ontology:measurement(comm_tr_t35, commerce_clause_scope__intermediate_channels, theater_ratio, 35, 0.34).
narrative_ontology:measurement(comm_tr_t40, commerce_clause_scope__intermediate_channels, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_scope__intermediate_channels, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(comm_be_t5, commerce_clause_scope__intermediate_channels, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(comm_be_t10, commerce_clause_scope__intermediate_channels, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(comm_be_t15, commerce_clause_scope__intermediate_channels, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(comm_be_t20, commerce_clause_scope__intermediate_channels, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(comm_be_t25, commerce_clause_scope__intermediate_channels, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(comm_be_t30, commerce_clause_scope__intermediate_channels, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(comm_be_t35, commerce_clause_scope__intermediate_channels, base_extractiveness, 35, 0.66).
narrative_ontology:measurement(comm_be_t40, commerce_clause_scope__intermediate_channels, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_scope__intermediate_channels, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(comm_su_t5, commerce_clause_scope__intermediate_channels, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(comm_su_t10, commerce_clause_scope__intermediate_channels, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(comm_su_t15, commerce_clause_scope__intermediate_channels, suppression_requirement, 15, 0.43).
narrative_ontology:measurement(comm_su_t20, commerce_clause_scope__intermediate_channels, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(comm_su_t25, commerce_clause_scope__intermediate_channels, suppression_requirement, 25, 0.47).
narrative_ontology:measurement(comm_su_t30, commerce_clause_scope__intermediate_channels, suppression_requirement, 30, 0.49).
narrative_ontology:measurement(comm_su_t35, commerce_clause_scope__intermediate_channels, suppression_requirement, 35, 0.51).
narrative_ontology:measurement(comm_su_t40, commerce_clause_scope__intermediate_channels, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__intermediate_channels, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__intermediate_channels, 0.12).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, dormant_commerce_clause_state_sovereignty).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, federal_regulatory_preemption__economic_spheres).

% DUAL FORMULATION NOTE:
% The commerce_clause_scope kernel has three readings: narrow_originalist (federal power confined to direct interstate trade channels), intermediate_channels (federal power extended to substantial effects on interstate commerce, subject to categorical limits), and broad_effects_test (federal power extended to any economic activity substantially affecting interstate commerce). Each reading instantiates a different constraint with different victim sets, different limiting principles, and different extractiveness. The intermediate reading (this file) is the standing doctrine as of 2026 but is unstable: the functional reach of federal authority continues to expand while categorical limits are periodically reasserted theatrically. The three readings are linked via network.affects_constraints because the Court's doctrine-setting moves between readings affect the constitutional structure available to all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_scope__intermediate_channels, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
