% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__facility_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_delegation__facility_constraint_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: caa_section_111d_delegation__facility_constraint_reading
 *   human_readable: Section 111(d) 'Best System' Limited to Facility-Level Measures
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   Section 111(d) of the Clean Air Act requires EPA to set emissions
 *   standards based on the 'best system of emissions reduction' for existing
 *   power plants. This constraint instantiates ONE READING of that contested
 *   kernel: the facility constraint reading interprets 'best system' to mean
 *   measures implementable at individual power plants (heat-rate
 *   improvements, carbon capture retrofits) — NOT generation-shifting, coal
 *   retirement, or grid-wide renewable substitution. Under this reading, EPA
 *   cannot mandate that states move away from coal or retire plants; only
 *   efficiency and capture are in scope. This protects the coal sector from
 *   forced transformation and preserves state autonomy over energy mix, but
 *   it also imposes a regulatory ceiling that climate advocates argue is
 *   inconsistent with the Act's climate urgency and statutory language. The
 *   sibling reading (systemic_transformation_reading) interprets 'best
 *   system' to authorize EPA to mandate generation-shifting and early
 *   retirement as necessary to achieve the best achievable emissions rate.
 *   This story generates the constraint as the facility reading experiences
 *   and enforces it. The claim/metric gap is deliberate: the claim is
 *   tangled_rope (coordination of state/EPA/coal sector interests, plus
 *   asymmetric extraction from climate advocates); the metrics describe a
 *   substantially extractive, heavily enforced operation with rising theater
 *   (the 'clean coal' and 'facility-level' framing intensifies even as the
 *   underlying extraction plateaus). The engine measures that gap.
 *
 * KEY AGENTS:
 *   - Coal sector operators: beneficiary, protected from forced retirement, operate under facility-level standard
 *   - States preserving energy mix authority: beneficiary/agenda-setter, retain federalism, no mandate for generation shift
 *   - EPA interpreting agency: agenda-setter, narrow delegated authority constrained by facility-level reading
 *   - Climate advocates and environmental groups: payer, trapped by regulatory ceiling, cannot achieve grid-wide decarbonization
 *   - Public health constituencies: payer (powerless), bear health costs of coal persistence
 *   - Renewable energy developers: payer, face constrained market due to coal protection
 *   - Federal courts: observer, arbiter of statutory interpretation
 *   - Congress/statutory text: observer (non-agent), the kernel being read
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, 0.68).
domain_priors:suppression_score(caa_section_111d_delegation__facility_constraint_reading, 0.71).
domain_priors:theater_ratio(caa_section_111d_delegation__facility_constraint_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(caa_section_111d_delegation__facility_constraint_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__facility_constraint_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__facility_constraint_reading, "Section 111(d) 'Best System' Limited to Facility-Level Measures").
narrative_ontology:topic_domain(caa_section_111d_delegation__facility_constraint_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__facility_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__facility_constraint_reading, 'c80b3275-1f9a-4b6c-a640-f35f05cb2889').
narrative_ontology:cs_kernel_codification('c80b3275-1f9a-4b6c-a640-f35f05cb2889', fixed_text).
narrative_ontology:cs_authority_grounding('c80b3275-1f9a-4b6c-a640-f35f05cb2889', lineage).
narrative_ontology:cs_interpretation_layer_present('c80b3275-1f9a-4b6c-a640-f35f05cb2889').
narrative_ontology:cs_reading_relation('c80b3275-1f9a-4b6c-a640-f35f05cb2889', caa_section_111d_delegation__systemic_transformation_reading, coexists_with).
narrative_ontology:cs_axiom('c80b3275-1f9a-4b6c-a640-f35f05cb2889', foundational, epa_authority_bounded_by_facility_feasibility).
narrative_ontology:cs_axiom_status(epa_authority_bounded_by_facility_feasibility, holdable).
narrative_ontology:cs_axiom_grounding('c80b3275-1f9a-4b6c-a640-f35f05cb2889', epa_authority_bounded_by_facility_feasibility, deontological).
narrative_ontology:cs_axiom('c80b3275-1f9a-4b6c-a640-f35f05cb2889', foundational, generation_shifting_outside_delegated_scope).
narrative_ontology:cs_axiom_status(generation_shifting_outside_delegated_scope, holdable).
narrative_ontology:cs_axiom_grounding('c80b3275-1f9a-4b6c-a640-f35f05cb2889', generation_shifting_outside_delegated_scope, conventional).
narrative_ontology:cs_reference_frame('c80b3275-1f9a-4b6c-a640-f35f05cb2889', clean_air_act_facility_centric_delegation).
narrative_ontology:cs_drift_state('c80b3275-1f9a-4b6c-a640-f35f05cb2889', contemporary_climate_urgency_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c80b3275-1f9a-4b6c-a640-f35f05cb2889', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, coal_sector_operators).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__facility_constraint_reading, states_preserving_energy_mix_authority).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, climate_advocates_and_environmental_groups).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, public_health_constituencies).
narrative_ontology:constraint_victim(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate existing coal plants and coal mining operations. Under the facility constraint reading, they are protected from forced retirement or generation-shifting mandates; their exposure is limited to heat-rate improvements and carbon capture retrofits at individual sites. They can challenge those retrofit costs as excessive but retain the option to operate existing plants. They collect policy certainty: the regulatory ceiling is predictable and does not require business model transformation.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, coal_sector_operators, beneficiary,
    institutional, generational, arbitrage, national).

% Retain authority over electricity generation mix decisions. The facility constraint reading interprets Section 111(d) as authorizing EPA to set emissions standards but not to mandate the fuel mix or technology transitions that would achieve system-wide decarbonization. States can set their own renewable portfolio standards, carbon pricing, or retirements if they choose; EPA cannot require it. They benefit from preserved federalism and the political flexibility to delay transition.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, states_preserving_energy_mix_authority, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(caa_section_111d_delegation__facility_constraint_reading, states_preserving_energy_mix_authority, agenda_setter).

% Interprets and enforces the Clean Air Act Section 111(d) requirements. Under this reading, the agency's mandate is narrow: set a performance standard based on the best system of emissions reduction achievable at individual facilities, subject to the feasibility and cost constraints of facility-level retrofits. The agency cannot reach beyond the fence-line to mandate generation-shifting, renewable substitution, or early retirement. It administers the rule but operates within a constrained delegated authority.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, epa_interpreting_agency, agenda_setter,
    institutional, biographical, constrained, national).

% Seek grid-wide decarbonization and early coal retirement to limit warming. They pay by accepting a regulatory ceiling that does not mandate the generation-shifting they believe necessary. Their recourse is litigation (challenging the reading as inconsistent with statutory text and climate urgency) or legislative change (pushing Congress to amend Section 111(d)); both are costly and uncertain. They are constrained by the reading's interpretation of delegated authority.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, climate_advocates_and_environmental_groups, payer,
    organized, generational, constrained, national).

% Bear the public health costs (respiratory disease, premature mortality, climate impacts) of the regulatory ceiling. Under the facility constraint reading, they cannot exit: they have no seat at the rulemaking table, no direct recourse in the administrative process, and no political power to shift the statutory reading. They are trapped by the interpretation even if it increases their health burden.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, public_health_constituencies, payer,
    powerless, biographical, trapped, national).

% Compete for market share against coal plants that are protected from forced retirement. The facility constraint reading limits EPA mandate to within-fence retrofits (heat-rate, capture); it does not mandate the generation-shifting that would accelerate coal retirement and create market space for renewables. They face a constrained market where coal persistence is policy-protected, limiting their growth opportunities and pricing power.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, renewable_energy_developers, payer,
    moderate, biographical, constrained, national).

% Adjudicate challenges to EPA rulemaking and constitutional delegations. Courts have upheld narrow readings of Section 111(d) on several occasions (e.g., Utility Air Regulatory Group v. EPA, 2014; West Virginia v. EPA, 2022); they are the arbiter of whether the facility constraint reading is a permissible statutory interpretation or an unlawful cap on delegated authority.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__facility_constraint_reading, federal_courts, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(caa_section_111d_delegation__facility_constraint_reading, coal_sector_operators).
narrative_ontology:fixing_cost_class(caa_section_111d_delegation__facility_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform, predictable standard for coal plant emissions: facility-level heat-rate and capture retrofits are required; states and operators know the scope and cost of compliance; the constraint avoids conflicting state mandates by preempting generation-shifting in federal law.
% TRANSFER_FUNCTION: Transfers regulatory predictability and coal sector protection to coal operators and states (who are protected from forced retirement and grid-wide mandates); transfers the burden of climate delay and public health costs to climate advocates, environmental groups, renewable developers, and powerless public health constituencies (who cannot achieve the grid-wide decarbonization they believe necessary).
% ABSENT_VOICES: Future generations who will bear the climate and health consequences of delayed decarbonization; coal workers who would benefit from just-transition support if retirements were mandated (they have no seat in the rulemaking and cannot voice the need for transition assistance); developing nations and climate-vulnerable populations who bear the global climate burden.
% DISAPPEARANCE_RATIONALE: If the facility constraint reading disappeared and Section 111(d) were interpreted to authorize generation-shifting mandates, the electrical grid would reorganize rapidly: coal plants would retire accelerated, renewable deployment would expand, grid stability would shift to new technologies, and state energy policies would align with federal climate requirements rather than operating within a narrow facility-level ceiling. The coal sector, state autonomy, and regulatory predictability would be substantially altered.
% FOUNDING_PROBLEM: Early implementation of the Clean Air Act required states and EPA to set emissions standards for coal plants without destabilizing the grid or imposing impossible retrofit costs. Section 111(d) needed an interpretation that set achievable pollution limits while preserving state authority and avoiding forced retirements that could cause electricity shortages.
% FOUNDING_PROBLEM_CORROBORATION: Coal sector and coal-dependent states attest the founding problem is still live: grid stability and electricity costs justify the facility constraint reading. EPA and the courts have endorsed narrow interpretations in several rulemakings (Utility Air Regulatory Group v. EPA, 2014; West Virginia v. EPA, 2022). Climate advocates and environmental economists attest the founding problem was solved by the 1990s (acid rain program, efficiency gains, renewable cost collapse) and the reading now persists as regulatory protection for coal, not as grid necessity. That dissent comes from credentialed scientists, economists, and policy analysts outside the coal sector.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__facility_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__facility_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__facility_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(caa_section_111d_delegation__facility_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__facility_constraint_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__facility_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__facility_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(caa_section_111d_delegation__facility_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) and rising steadily through the interval because the facility constraint interprets delegated authority in a way that benefits coal sector beneficiaries while imposing costs on climate advocates who cannot exit: they cannot shift EPA mandate, cannot force retirement, cannot achieve the grid transformation they believe necessary for climate goals. The constraint extracts regulatory certainty from climate constituencies and delivers it to coal. Suppression is even higher (0.71) because the constraint's persistence depends on active legal enforcement: climate advocates challenge the reading repeatedly in courts and EPA rulemakings; the facility constraint reading is maintained through litigation defense, judicial deference doctrine (Chevron, now weakened but historically operative), and congressional inaction. The courts have upheld narrow readings multiple times (Utility Air Regulatory Group v. EPA, 2014; West Virginia v. EPA, 2022). Theater is moderate and rising (0.28 → 0.42): EPA's public framing emphasizes 'best achievable technology' and 'cost-effectiveness,' which are real technical concepts, but the constraint's actual function is to preserve coal while appearing to regulate it. The gap between 'best system' rhetoric and 'facility-only' reality is the theater. The measurement series are on one shared grid (every metric at every time point). Early readings (0–20) are observed from case law, EPA documents, and litigation records; later readings (25–35) are projected because the interval extends beyond the dataset cutoff. Extractiveness plateaus around t=25 because the constraint reaches equilibrium: the reading is codified in case law (West Virginia v. EPA, 2022), and neither coal advocates nor climate advocates expect rapid change. Theater also plateaus: the 'clean coal' framing does not deepen further.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (coal, states) should compute the constraint as coordination: they see a stable, predictable regulatory framework that solves the problem of how to set emissions standards without destroying the coal economy or the electricity grid. From their seat, the constraint is genuinely cooperative — coal and states negotiated through the regulatory process and won a reading they prefer. The payer seats (climate advocates, public health) compute the same constraint as extraction: from their perspective, the facility ceiling is a regulatory taking — EPA is prevented from exercising delegated authority in a way they believe statutory law requires and climate urgency demands. The engine computes this divergence from power/exit/beneficiary/victim data without being told what to find. The claimed type (tangled_rope) bridges the gap: genuine coordination solved (how to regulate coal without destabilizing the grid), plus asymmetric extraction (climate advocates cannot exit, coal operators are protected). The metrics support this: high extractiveness and suppression indicate that the coordination benefit is concentrated in coal beneficiaries while the extraction is concentrated in climate victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Coal sector operators and states are the structural beneficiaries: they collect regulatory certainty and protection from generation-shifting mandates. Their exit options are strong (arbitrage, mobile) — they can adapt to future rule changes or exit coal markets if economics shift. They sit near the beneficiary end of the directionality spectrum (d ≈ 0.2–0.3). Climate advocates and environmental groups are the structural victims: the constraint extracts from them by preventing EPA action they believe legally required and climate-necessary. Their exit options are constrained (litigation, legislative change, advocacy) — all costly and uncertain with low expected value. They sit near the target end (d ≈ 0.75–0.85). EPA itself is agenda-setter but constrained: it administers the rule but operates within the facility-level ceiling. Its directionality is near symmetric (d ≈ 0.5) because it benefits from the clarity and political insulation the reading provides while bearing the cost of perpetual litigation and erosion of institutional legitimacy. Public health constituencies are powerless (d ≈ 0.90, full target): they bear the costs of coal persistence but have no seat in the rulemaking and no exit option. Renewable developers are moderate payer (d ≈ 0.65): they pay by facing delayed market expansion but retain arbitrage options (lobby, develop in friendly states, export technology).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (early CAA implementation: set emissions standards without forcing grid destabilization or impossible retrofit costs) was genuine and urgent in the 1970s and 1980s. By the 2000s, the problem was substantially solved: renewable costs collapsed, grid integration improved, and coal's economic case weakened independent of regulation. The facility constraint reading persists not because the founding problem remains live but because the coal sector and coal-dependent states have captured the statutory interpretation and the regulatory process. The divergence between founding_problem_status (contested: coal sector says 'live,' others say 'dead') and disappearance_verdict (world_rearranges: if the reading disappeared, coal retirement would accelerate and the grid would reorganize) is the mandatrophy signal: the constraint persists not for the coordination it solved but as regulatory protection for an incumbent industry. The theater rise (0.28 → 0.42) documents this drift: enforcement increasingly focuses on defending the facility-level ceiling against climate advocates rather than on solving the grid stability problem. EPA's spending on litigation defense and the volume of challenge briefs document the suppression ratchet. A genuine coordination constraint would show stable or declining theater (the problem solved once, no need to re-argue); a constraint drifting into pure extraction shows rising theater (increasingly performative defense of the reading against its critics).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statutory_system_referent_ambiguity,
    'Does ''best system of emissions reduction'' in Section 111(d) refer to measures implementable at individual facilities, or to the optimal system of electricity generation and management needed to achieve the lowest achievable emissions rate?',
    'Statutory interpretation through textualist analysis, historical legislative intent (1970 CAA debates and 1990 amendments), and administrative law doctrine (Chevron deference, now weakened; major questions doctrine). A Supreme Court decision clarifying the scope of EPA''s delegated authority under Section 111(d) would resolve it.',
    'If the statutory ''system'' includes generation-shifting, EPA would have authority to mandate coal retirement and renewable substitution; the constraint would shift from tangled_rope to snare (EPA enforcing a transformation victims cannot exit). If the statute is read to prohibit generation-shifting, the facility constraint persists and the regulatory ceiling remains binding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_system_referent_ambiguity, conceptual, 'The scope of ''system'' determines the scope of EPA authority.').

omega_variable(
    coal_sector_capture_of_statutory_reading,
    'Is the facility constraint reading a permissible interpretation of Section 111(d), or a manifestation of coal sector regulatory capture that narrows statutory language to benefit incumbents?',
    'Post-capture analysis: compare the facility reading''s textual and historical support to the systemic reading''s support; assess whether the reading has drifted to protect coal beyond what statutory language permits. Examine litigation patterns: if climate advocates and EPA economists consistently argue the facility reading is a misreading, and courts nonetheless uphold it, that asymmetry signals capture, not law.',
    'If the reading is a legitimate interpretation of ambiguous statutory language, the constraint is tangled_rope (genuine coordination + asymmetric extraction). If the reading is a captured narrowing, the constraint approaches snare (pure extraction from climate advocates through regulatory ceiling); the constraint would be reclassified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coal_sector_capture_of_statutory_reading, empirical, 'Whether the facility constraint reading reflects statutory fidelity or coal sector capture.').

omega_variable(
    climate_advocates_structural_exit_option,
    'Are climate advocates'' exit options (litigation, legislative amendment, state-level action) genuinely constrained, or do they have sufficient arbitrage to pursue grid-decarbonization goals outside the federal Section 111(d) regime?',
    'Measure the trajectory of climate action in states that have adopted unilateral carbon pricing, renewable portfolio standards, or coal retirement mandates despite the federal facility constraint. If states can achieve substantial decarbonization independently, climate advocates are not truly trapped; if state action is insufficient and bottlenecked by coal persistence in federal-regulated plants, the exit constraint is real.',
    'If arbitrage exists (state action unlocks decarbonization), climate advocates'' directionality is lower (d ≈ 0.55–0.65, constrained but not fully targeted); the constraint is more extractive overall but less extractive from climate advocates. If arbitrage fails (state action cannot overcome federal coal protection), directionality is higher (d ≈ 0.75–0.85, nearly fully targeted); the constraint is more extractive from climate advocates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_advocates_structural_exit_option, empirical, 'Whether climate advocates can achieve decarbonization goals outside the federal facility constraint.').

omega_variable(
    suppression_mechanism_legal_vs_economic,
    'Does suppression of alternative readings (the systemic_transformation_reading) arise from legal doctrine (Chevron deference, statutory construction rules) or from economic power (coal sector resources in litigation)?',
    'Track changes in suppression after 2022 Dobbs/major-questions doctrine rulings that shifted judicial deference away from Chevron. If suppression weakens post-Dobbs, legal doctrine was the primary suppression mechanism; if suppression persists, economic power (coal sector litigation funding, political pressure) sustains it.',
    'If legal doctrine is primary, the suppression is more structural and stable (built into the interpretation-confirmation loop); if economic power is primary, the suppression is more contingent and can be disrupted by litigation success. The constraint''s durability depends on which suppression mechanism dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_legal_vs_economic, empirical, 'Whether suppression is driven by legal doctrine or economic power.').

omega_variable(
    kernel_codification_status_and_revision_pathway,
    'Is the facility constraint reading a stable codification of Section 111(d), or is it a contestable reading that Congress can reverse through amendment or EPA can shift through new rulemaking?',
    'Legislative action: Congress amends Section 111(d) to explicitly authorize or prohibit generation-shifting. Administrative action: EPA promulgates a new rule under a new administration that interprets Section 111(d) to authorize generation-shifting; courts decide whether the new rule is a permissible reinterpretation. Judicial action: Supreme Court clarifies the statutory scope in the next major challenge.',
    'If the reading is codified and revision requires constitutional amendment-level consensus, it persists indefinitely and extraction continues. If the reading is contestable and can be shifted by electoral/administrative change, the constraint''s durability is temporary and climate advocates retain a real exit option (wait for administration change, challenge through new litigation). This determines whether the constraint is a stable tangled_rope or a piton (inert, awaiting disruption).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_codification_status_and_revision_pathway, preference, 'The stability and reversibility of the facility constraint reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__facility_constraint_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t0, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(caa__tr_t0, observed).
narrative_ontology:measurement(caa__tr_t5, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(caa__tr_t5, observed).
narrative_ontology:measurement(caa__tr_t10, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(caa__tr_t10, observed).
narrative_ontology:measurement(caa__tr_t15, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(caa__tr_t15, observed).
narrative_ontology:measurement(caa__tr_t20, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(caa__tr_t20, observed).
narrative_ontology:measurement(caa__tr_t25, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(caa__tr_t25, projected).
narrative_ontology:measurement(caa__tr_t30, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(caa__tr_t30, projected).
narrative_ontology:measurement(caa__tr_t35, caa_section_111d_delegation__facility_constraint_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(caa__tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(caa__be_t0, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(caa__be_t0, observed).
narrative_ontology:measurement(caa__be_t5, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement_basis(caa__be_t5, observed).
narrative_ontology:measurement(caa__be_t10, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement_basis(caa__be_t10, observed).
narrative_ontology:measurement(caa__be_t15, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(caa__be_t15, observed).
narrative_ontology:measurement(caa__be_t20, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(caa__be_t20, observed).
narrative_ontology:measurement(caa__be_t25, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(caa__be_t25, projected).
narrative_ontology:measurement(caa__be_t30, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(caa__be_t30, projected).
narrative_ontology:measurement(caa__be_t35, caa_section_111d_delegation__facility_constraint_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(caa__be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t0, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(caa__su_t0, observed).
narrative_ontology:measurement(caa__su_t5, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement_basis(caa__su_t5, observed).
narrative_ontology:measurement(caa__su_t10, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(caa__su_t10, observed).
narrative_ontology:measurement(caa__su_t15, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(caa__su_t15, observed).
narrative_ontology:measurement(caa__su_t20, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(caa__su_t20, observed).
narrative_ontology:measurement(caa__su_t25, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(caa__su_t25, projected).
narrative_ontology:measurement(caa__su_t30, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(caa__su_t30, projected).
narrative_ontology:measurement(caa__su_t35, caa_section_111d_delegation__facility_constraint_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(caa__su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__facility_constraint_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(caa_section_111d_delegation__facility_constraint_reading, 0.12).
narrative_ontology:affects_constraint(caa_section_111d_delegation__facility_constraint_reading, caa_section_111d_delegation__systemic_transformation_reading).

% DUAL FORMULATION NOTE:
% Section 111(d) of the Clean Air Act 'best system of emissions reduction' decomposes into two structurally distinct constraints, each with its own ε and stakeholder structure, because the statutory referent ('best system') admits two materially different readings that produce different beneficiary/victim sets and different extraction profiles. The facility_constraint_reading (this file) interprets 'system' narrowly: facility-level measures only, protecting coal and states, extracting from climate advocates. The systemic_transformation_reading (sibling file) interprets 'system' broadly: generation-shifting and retirement permitted, protecting climate action, extracting from coal sector. These are not the same constraint viewed from two angles — their ε values differ substantially (facility reading: 0.68; systemic reading: ~0.30 for coal sector but ~0.75 for EPA authority). The readings have separate epistemologies, separate winners/losers, separate empirical status. They are linked via network.affects_constraints because they are siblings of a single kernel, but each is its own story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(caa_section_111d_delegation__facility_constraint_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
