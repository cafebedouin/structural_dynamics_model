% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__capture_substrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__capture_substrate_reading, []).

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
 *   constraint_id: ietf_openness_commitment__capture_substrate_reading
 *   human_readable: IETF Standards Process as Capture Substrate (Resource Advantage → Encoded Gatekeeping)
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   The IETF standards process represents a critical case where openness, in
 *   principle, masks gatekeeping in practice. The constraint operates through
 *   a dual mechanism: (1) the resource asymmetry between large technology
 *   operators and smaller implementers creates structural power imbalance,
 *   and (2) the open standards framework legitimizes outcomes that
 *   operationalize this imbalance by encoding vendor preferences into
 *   technical specifications. This reading instantiates the 'capture
 *   substrate' interpretation of the IETF's openness commitment: the
 *   institution functions as a mechanism through which resource advantage
 *   translates into encoded gatekeeping. Large operators benefit from the
 *   legitimacy and interoperability of open standards while using resource
 *   concentration to ensure that 'optional' technical features, architectural
 *   choices, and extension mechanisms favor their own implementations. Small
 *   implementers and users of competing platforms face genuine coordination
 *   costs (must implement the standard) alongside extraction (the standard
 *   incorporates features and assumptions that privilege large operators).
 *   The theater ratio has risen over the interval as the gap between the
 *   IETF's procedural openness (anyone can participate, consensus is sought,
 *   RFCs are published openly) and the functional closure (outcomes align
 *   with resource-concentrated preferences) has widened. This constraint
 *   exists in active contest with two sibling readings of the same kernel:
 *   the commons_stewardship reading (IETF as genuine global coordination
 *   institution serving technical stewardship norms) and the
 *   legitimacy_erosion reading (IETF losing credibility as it is perceived as
 *   captured). This story narrates the capture_substrate reading: the
 *   institution's openness is simultaneously its legitimacy mechanism and the
 *   medium through which capture operates.
 *
 * KEY AGENTS:
 *   - Large Platform Operators (Google, Amazon, Apple, Meta): Institutional/arbitrage — Primary beneficiaries. Sufficient resources to maintain participation and influence working group direction. Can encode preferred features as 'optional' and rely on market dominance to make them de facto mandatory.
 *   - Small Implementers and Emerging-Market Network Operators: Powerless/trapped — Primary victims. Cannot exit the standards process (interoperability requires participation) but lack resources to influence outcomes. Face hidden technical costs of reverse-engineering de facto requirements.
 *   - Open Standards Advocacy Coalition (EFF, Mozilla, smaller foundations): Organized/constrained — Secondary actors. Can prevent worst-case proprietary enclosure but are outmatched in resources and cannot set positive agenda.
 *   - IETF Governance (Working groups, consensus procedures, RFC publication): Institutional/arbitrage — Performs openness while operationalizing closure. Theater increases as resource asymmetry increases.
 *   - User Privacy Commons / Global Internet Users: Powerless/trapped (not direct participants) — Indirect victims. Standards designed for operator convenience often include features (telemetry, dependency graphs, centralized trust anchors) that disadvantage user privacy.
 *   - Analytical Observer: Analytical/analytical — Can recognize the capture dynamics but risks naturalizing them as inevitable features of global coordination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, 0.52).
domain_priors:suppression_score(ietf_openness_commitment__capture_substrate_reading, 0.58).
domain_priors:theater_ratio(ietf_openness_commitment__capture_substrate_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__capture_substrate_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__capture_substrate_reading, "IETF Standards Process as Capture Substrate (Resource Advantage → Encoded Gatekeeping)").
narrative_ontology:topic_domain(ietf_openness_commitment__capture_substrate_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__capture_substrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__capture_substrate_reading, 'c2490c6a-dbec-4c85-a27c-a73d33b046f5').
narrative_ontology:cs_kernel_codification('c2490c6a-dbec-4c85-a27c-a73d33b046f5', fixed_text).
narrative_ontology:cs_authority_grounding('c2490c6a-dbec-4c85-a27c-a73d33b046f5', extraction).
narrative_ontology:cs_interpretation_layer_present('c2490c6a-dbec-4c85-a27c-a73d33b046f5').
narrative_ontology:cs_reading_relation('c2490c6a-dbec-4c85-a27c-a73d33b046f5', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('c2490c6a-dbec-4c85-a27c-a73d33b046f5', ietf_openness_commitment__legitimacy_erosion_reading, influences).
narrative_ontology:cs_axiom('c2490c6a-dbec-4c85-a27c-a73d33b046f5', foundational, open_participation_masks_resource_gatekeeping).
narrative_ontology:cs_axiom_status(open_participation_masks_resource_gatekeeping, holdable).
narrative_ontology:cs_axiom_grounding('c2490c6a-dbec-4c85-a27c-a73d33b046f5', open_participation_masks_resource_gatekeeping, empirically_contingent).
narrative_ontology:cs_axiom('c2490c6a-dbec-4c85-a27c-a73d33b046f5', foundational, legitimate_encoding_of_vendor_preferences_in_standards).
narrative_ontology:cs_axiom_status(legitimate_encoding_of_vendor_preferences_in_standards, holdable).
narrative_ontology:cs_axiom_grounding('c2490c6a-dbec-4c85-a27c-a73d33b046f5', legitimate_encoding_of_vendor_preferences_in_standards, instrumental).
narrative_ontology:cs_reference_frame('c2490c6a-dbec-4c85-a27c-a73d33b046f5', rough_consensus_and_running_code).
narrative_ontology:cs_drift_state('c2490c6a-dbec-4c85-a27c-a73d33b046f5', contemporary_tech_concentration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c2490c6a-dbec-4c85-a27c-a73d33b046f5', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, multinational_technology_companies).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, small_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, developing_nation_network_operators).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, user_privacy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL IMPLEMENTERS (SNARE) — Cannot exit the IETF process or its outputs; standards become de facto mandatory for interoperability. Trapped by the global coordination requirement. Large operators have encoded proprietary extensions and gatekeeping logic into RFC specifications that appear open but operationalize extraction. Small implementers bear the cost of reverse-engineering underdocumented technical choices and conforming to de facto requirements that favor established vendors.
constraint_indexing:constraint_classification(ietf_openness_commitment__capture_substrate_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ACADEMIC AND MID-TIER ORGS (TANGLED ROPE) — Benefit from open standards as a coordination substrate (genuine value: interoperability, shared development cost) while being constrained by resource barriers to participation. Cannot afford to maintain delegation of engineers to IETF working groups. Constrained by the need to implement standards designed by vendor-dominated processes. Experience mixed coordination benefit (standards work) and moderate extraction (upward redistribution of technical design power).
constraint_indexing:constraint_classification(ietf_openness_commitment__capture_substrate_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE PLATFORM OPERATORS (ROPE) — Primary beneficiaries. Experience the IETF process as a coordination mechanism that enables their dominance. Large operators (Google, Amazon, Apple, Meta) can afford sustained participation: dedicated standards engineers, influence over working group direction, ability to encode proprietary extensions as 'optional features' that become de facto requirements. The open standards framework legitimizes their technical choices while creating barriers to competition. Their arbitrage position (can exit to proprietary standards but choose not to, extracting value from the 'open' framing) creates positive effective extraction.
constraint_indexing:constraint_classification(ietf_openness_commitment__capture_substrate_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN STANDARDS ADVOCACY (TANGLED ROPE) — Organized agents (EFF, Mozilla, smaller open-source foundations) see the IETF process as a coordination substrate that requires defensive participation. They extract value by preventing the worst-case proprietary enclosure while being constrained by resource limitations relative to large operators. Active enforcement via advocacy, but with limited direct power over RFC content. Experience genuine coordination benefit (preventing wholesale proprietary lock-in) alongside constrained extraction (their defensive priorities are overridden by vendor-driven technical choices).
constraint_indexing:constraint_classification(ietf_openness_commitment__capture_substrate_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: IETF GOVERNANCE (PITON) — The institutional machinery of the IETF itself — working groups, consensus procedures, RFC publication — is substantially performative. Theater emerges from the consensus ritual: documents undergo extensive review and consensus-building, but the technical outcomes are already determined by resource concentration. Small participants see the process as democratic and open; large operators have already shaped working group composition and agenda. The governance structure persists through institutional inertia and the legitimacy claim of 'rough consensus and running code,' but the functional verification of these principles degrades as resource asymmetry increases.
constraint_indexing:constraint_classification(ietf_openness_commitment__capture_substrate_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INEVITABLE CONCENTRATION (MOUNTAIN) — From a civilizational perspective, the IETF process could be viewed as an immutable feature of how global technical coordination functions: any global standards body will concentrate power among those with resources to participate. The 'openness' of IETF (anyone can join) is treated as a natural law of institutional design. However, this reading naturalizes what is actually a contingent institutional choice. The false summit detection signal: the constraint has identifiable beneficiaries (large operators), clear victims (small implementers), and active enforcement (resource gatekeeping disguised as technical complexity). The mountain classification masks capture dynamics.
constraint_indexing:constraint_classification(ietf_openness_commitment__capture_substrate_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__capture_substrate_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ietf_openness_commitment__capture_substrate_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ietf_openness_commitment__capture_substrate_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ietf_openness_commitment__capture_substrate_reading, TR),
    TR >= 0.70.

:- end_tests(ietf_openness_commitment__capture_substrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Large operators capture the standards process through sustained participation, resource concentration, and ability to frame technical choices. The extraction is moderate (not 0.70+) because the open standards framework does provide genuine coordination benefits that reduce total system friction and because small operators and users do receive some benefit from interoperability. However, the distribution of benefits is highly asymmetric: large operators extract net positive value; small operators and users bear net extraction costs. The measurement trajectory (0.35 → 0.52 over the interval) reflects the historical acceleration of resource concentration in the technology sector and the corresponding widening of the participation gap in standards bodies. Suppression (0.58): Moderate-high. The barriers to meaningful participation are substantial: (1) technical knowledge requirements (working groups assume expertise), (2) resource costs (sustained participation requires dedicated staff), (3) institutional continuity (outcomes depend on sustained engagement over years; small organizations cannot maintain continuity), (4) agenda-setting power (large operators shape working group formation and priorities). These barriers are real but not absolute — suppression is not 0.75+ because alternative participation mechanisms (mailing lists, remote participation, open comment periods) do exist and some small entities do influence outcomes. Theater ratio (0.64): Moderate-high. The consensus procedures (rough consensus and running code) are substantially performative. The theatrical elements: (1) consensus is formally sought but outcomes are determined by resource concentration before formal consensus-seeking begins, (2) 'running code' requirement favors large operators who can afford implementation during the standards development process, (3) procedural openness (anyone can join) obscures functional closure (outcomes reflect participant resources). Theater has risen over the interval as resource asymmetry has increased — early IETF had smaller resource gaps between participants, making consensus procedures functionally more open.
 *
 * PERSPECTIVAL GAP:
 *   Large platform operators perceive the IETF process as coordination (Rope) — standards enable their platforms to interoperate efficiently and they have adequate resources to influence the process. Small implementers perceive it as extraction (Snare) — they are locked in by the requirement to implement standards but have no meaningful influence. The open standards coalition perceives mixed coordination and extraction (Tangled Rope) — they defend against proprietary enclosure (coordination function) while being outmatched by vendor resources (extraction experience). The IETF institutional machinery perceives its own process as degraded but legitimate (Piton) — the consensus procedures are performed but functionally determined by resource concentration. The analytical observer risks seeing inevitability (Mountain) — treating resource concentration in standards bodies as a natural law of institutional design rather than a structural feature that could be altered through participation reforms, funding mechanisms, or governance redesign.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is computed from agent power, exit options, and beneficiary/victim status. Beneficiaries with arbitrage options experience effective extraction chi << base extraction ε because their low d values dampen f(d). Victims with trapped exit experience chi >> ε because their high d values amplify f(d). The pipeline correctly predicts that the same base_extractiveness (0.52) produces wildly different experienced extraction depending on perspective: the beneficiary might perceive the constraint as nearly costless or even profitable coordination (Rope), while the trapped victim perceives it as severe extraction (Snare). This perspectival gap is the diagnostic signal that the constraint is actually tangled rope — it provides genuine coordination benefit (all agents are better off with interoperable standards than with fragmentation) while distributing those benefits asymmetrically (large operators capture most of the benefit through structural power over design outcomes).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing the genuine coordination function (standards enable interoperability, reducing transaction costs for all actors) from the asymmetric distribution of the gains (large operators capture more value than small ones). The key insight: a tangled rope always provides some coordination benefit (otherwise it would collapse into pure extraction/snare). The IETF does solve a genuine coordination problem — enabling global technical standards without centralized command. But it does so in a way that operationalizes extraction for those with fewer resources to participate. The classification resists the false dichotomy between 'pure coordination' and 'pure extraction' by recognizing that institutional mechanisms can simultaneously solve coordination problems and distribute their benefits asymmetrically. The theater_ratio trajectory (rising from 0.48 to 0.64) shows increasing performativity as resource asymmetry increases — the institutional procedures become more theatrical precisely because the functional correlation between openness (consensus, rough consensus, running code) and actual outcomes (large-operator-preferred designs) degrades. A genuine rope would show stable theater (procedures correlate with outcomes); the rising theater signals the tangled rope diagnosis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proprietary_extension_encoding,
    'Are vendor-preferred technical features encoded into RFC specifications as mandatory vs. optional in ways that create de facto proprietary requirements?',
    'Comparative RFC analysis: tracking implementation rates of vendor-preferred optional features vs. technically neutral alternatives; network analysis of which features are implemented by which vendor ecosystems',
    'If high encoding: IETF process operationalizes gatekeeping disguised as open standards — extractiveness approaches 0.65. If low encoding: openness is more substantive — extractiveness drops to 0.35 (pure coordination rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proprietary_extension_encoding, empirical, 'Whether vendor preferences are encoded as de facto requirements in RFCs').

omega_variable(
    resource_asymmetry_translation,
    'What mechanisms translate large operators'' resource advantage (dedicated standards engineers, organizational continuity) into technical design outcomes?',
    'Working group composition tracking over time; correlation between company employee presence and RFC section authorship; analysis of comment incorporation by participant affiliation',
    'If mechanisms are structural (not contingent): capture is a fundamental feature of the process, not a correctable bias — supports high extractiveness (0.52+). If contingent: improved participation tools and transparency could reduce extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_asymmetry_translation, empirical, 'How resource advantage translates to technical design control').

omega_variable(
    openness_performance_gap,
    'Do standards-compliant implementations by small operators functionally interoperate with the de facto implementations of large operators, or do hidden technical choices create compatibility breaks?',
    'Controlled interoperability testing: RFC-compliant code from small developers tested against production deployments from large operators; tracking of bug reports and compatibility issues by implementer type',
    'If high gap: the ''open standards'' claim is theater — small operators cannot actually participate in the coordination. Extractiveness remains 0.52+. If low gap: openness is substantive — extractiveness drops to 0.40 (genuine tangled rope with higher coordination content).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(openness_performance_gap, empirical, 'Functional interoperability gap between small and large operator implementations').

omega_variable(
    kernel_reading_contest,
    'Is the IETF''s openness commitment a codified institution legitimized by technical stewardship norms (commons_stewardship reading) or a substrate for capture that maintains legitimacy by performing openness (this capture_substrate reading)?',
    'Historical analysis of RFC outcomes: do outcomes align with technical merit and global benefit (stewardship hypothesis) or with large operator preferences (capture hypothesis)? Comparative case studies with alternative standards bodies (ISO, proprietary consortia).',
    'If stewardship: the constraint is a genuine rope (low extraction, high coordination). If capture: the constraint is a tangled rope or snare (moderate-to-high extraction masked by openness theater). Terminal classification depends on which reading the evidence supports.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether IETF is stewardship institution or capture substrate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__capture_substrate_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_capture_tr_t0, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ietf_capture_tr_t5, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 5, 0.56).
narrative_ontology:measurement(ietf_capture_tr_t10, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(ietf_capture_be_t0, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ietf_capture_be_t5, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(ietf_capture_be_t10, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__capture_substrate_reading, information_standard).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__legitimacy_erosion_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, dns_root_zone_architecture).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, tls_certificate_authority_trust_model).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'ietf_openness_commitment'. The sibling readings (commons_stewardship, legitimacy_erosion) are structurally distinct constraints with different ε values and different beneficiary/victim structures. They should be authored as separate JSON files and linked via network.affects_constraints. This decomposition respects the ε-invariance principle: the base_extractiveness value (0.52) is specific to the capture_substrate reading; alternative readings would estimate different base_extractiveness values reflecting their different structural analyses. For example, the commons_stewardship reading might estimate ε ≈ 0.25 (genuine coordination, minimal extraction), while the legitimacy_erosion reading might estimate ε ≈ 0.45 (mixed, but trending worse). Each reading is a self-contained constraint story with its own base properties, perspectives, and measurements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
