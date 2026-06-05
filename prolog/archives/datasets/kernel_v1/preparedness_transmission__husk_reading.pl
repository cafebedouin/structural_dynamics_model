% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__husk_reading, []).

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
 *   constraint_id: preparedness_transmission__husk_reading
 *   human_readable: Preparedness Transmission Husk: Ritual Compliance Without Adaptive Capacity
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   This constraint instantiates the 'husk reading' of the
 *   preparedness_transmission kernel. The kernel is the foundational claim
 *   that civil defense institutions maintain societal capacity to respond to
 *   disasters through documented protocols, regular drills, and formal
 *   inspections. The husk reading asserts that this kernel has lost adaptive
 *   function: the protocols have become rituals that produce credibility
 *   ('preparedness score: X%') without producing capacity to handle novel
 *   disaster modes. Organizational memory persists in performative form
 *   (drills are conducted, inspections are documented) but operational
 *   knowledge has hollowed out. The constraint operates through suppression
 *   of awareness: personnel recognize that protocol-rehearsed responses will
 *   not work for current climate scenarios (increasingly intense
 *   precipitation, compound flooding, spatial mismatch between population and
 *   infrastructure) but cannot voice this recognition without violating
 *   hierarchy and risking certification. The extractive flow is toward
 *   bureaucratic continuity (the institution's survival and reputation) and
 *   away from actual preparedness capacity (the community's adaptive
 *   capability). This reading coexists with the 'competence reading'
 *   (institutions CAN update protocols and DO adapt to novel scenarios in
 *   some contexts) and the 'hybrid reading' (mixture of functional capacity
 *   and performative husk). The husk reading is empirically distinguishable
 *   by: (1) post-disaster forensic analysis showing majority of observed
 *   failure modes were not rehearsed in prior drills; (2) slow or absent
 *   protocol revision after identified failures; (3) suppression of critical
 *   knowledge within the institution (personnel know the gap but cannot
 *   speak). The theater ratio trajectory (0.62 → 0.74 → 0.81) documents the
 *   increasing dominance of ritual form over functional content over the
 *   30-year interval.
 *
 * KEY AGENTS:
 *   - Central Government Administration: Primary beneficiary (institutional/arbitrage) — captures political credit for preparedness metrics without bearing cost of actual adaptive innovation
 *   - Bureaucratic Continuity: Systemic beneficiary — the institution preserves its organizational identity, budget lines, and staff roles by performing rather than transforming
 *   - Flood-Exposed Community: Primary victim (powerless/trapped) — relies on stated preparedness guarantees while actual adaptive capacity atrophies; no exit from geographic and legal dependency
 *   - Emergency Response Personnel: Secondary victim (moderate/constrained) — knowledge of protocol gaps suppressed; time and credibility extracted defending brittle procedures
 *   - Actual Preparedness Capacity: Systemic victim — organizational resources diverted from genuine innovation (spatial planning, infrastructure retreat, watershed management) to ritual maintenance
 *   - Cold War Civil Defense Institutional Legacy: Structural anchor — 1950s protocols and organizational identity persist through inertia despite obsolescence for contemporary hazards
 *   - International DRR Community: Organized actor (organized/constrained) — benefits from standardized protocols (coordination) but locked into pre-specified failure modes (extraction)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__husk_reading, 0.52).
domain_priors:suppression_score(preparedness_transmission__husk_reading, 0.68).
domain_priors:theater_ratio(preparedness_transmission__husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(preparedness_transmission__husk_reading, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__husk_reading, snare).
narrative_ontology:human_readable(preparedness_transmission__husk_reading, "Preparedness Transmission Husk: Ritual Compliance Without Adaptive Capacity").
narrative_ontology:topic_domain(preparedness_transmission__husk_reading, "disaster_risk_management/institutional_memory/civil_defense").

domain_priors:requires_active_enforcement(preparedness_transmission__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__husk_reading, 'ba59070b-42a7-46ba-b511-181a20c49997').
narrative_ontology:cs_kernel_codification('ba59070b-42a7-46ba-b511-181a20c49997', fixed_text).
narrative_ontology:cs_authority_grounding('ba59070b-42a7-46ba-b511-181a20c49997', extraction).
narrative_ontology:cs_interpretation_layer_present('ba59070b-42a7-46ba-b511-181a20c49997').
narrative_ontology:cs_reading_relation('ba59070b-42a7-46ba-b511-181a20c49997', preparedness_transmission__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('ba59070b-42a7-46ba-b511-181a20c49997', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('ba59070b-42a7-46ba-b511-181a20c49997', foundational, institutional_suppression_blocks_learning).
narrative_ontology:cs_axiom_status(institutional_suppression_blocks_learning, holdable).
narrative_ontology:cs_axiom_grounding('ba59070b-42a7-46ba-b511-181a20c49997', institutional_suppression_blocks_learning, empirically_contingent).
narrative_ontology:cs_axiom('ba59070b-42a7-46ba-b511-181a20c49997', foundational, cold_war_protocols_inert_under_novel_hazards).
narrative_ontology:cs_axiom_status(cold_war_protocols_inert_under_novel_hazards, overridden).
narrative_ontology:cs_axiom_grounding('ba59070b-42a7-46ba-b511-181a20c49997', cold_war_protocols_inert_under_novel_hazards, empirically_contingent).
narrative_ontology:cs_reference_frame('ba59070b-42a7-46ba-b511-181a20c49997', cold_war_nuclear_civil_defense_mandate).
narrative_ontology:cs_drift_state('ba59070b-42a7-46ba-b511-181a20c49997', contemporary_climate_driven_flooding, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ba59070b-42a7-46ba-b511-181a20c49997', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(preparedness_transmission__husk_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__husk_reading, bureaucratic_continuity).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, actual_preparedness_capacity).
narrative_ontology:constraint_victim(preparedness_transmission__husk_reading, adaptive_response_capability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FLOOD-EXPOSED COMMUNITY (SNARE) — Trapped by geographic exposure and legal reliance on stated preparedness guarantees. The constraint extracts organizational credibility ('we drill regularly, we inspect') while community faces novel flood scenarios the drills do not address. No exit from dependency. Maximum extraction: credibility captures resources and political will that should fund actual adaptive capacity.
constraint_indexing:constraint_classification(preparedness_transmission__husk_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EMERGENCY RESPONSE PERSONNEL (SNARE) — Constrained by employment and certification. Drills and inspections consume operational bandwidth; knowledge of gaps in protocol cannot be voiced (violates chain of command, threatens certification). High suppression: formal penalties for acknowledging the husk, informal penalties for admitting failure modes detected in practice. Extraction: time and credibility spent defending brittle protocols instead of building adaptive capacity.
constraint_indexing:constraint_classification(preparedness_transmission__husk_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTERNATIONAL DRR COMMUNITY (TANGLED ROPE) — Organized actors (UNISDR, IFRC, national coordinating bodies) benefit from standardized protocols enabling inter-jurisdictional coordination ('all countries use same drill template, data is comparable'). But the constraint extracts by locking these actors into pre-specified failure modes — they cannot adapt protocols to regional climate futures without breaking compatibility. Mixed: genuine coordination function (information standard for comparing preparedness across borders) alongside asymmetric extraction (homogenization suppresses regional variation).
constraint_indexing:constraint_classification(preparedness_transmission__husk_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: CENTRAL GOVERNMENT ADMINISTRATION (ROPE) — Experiences constraint as pure coordination. Drills and inspections produce comparable metrics ('preparedness score: 78%') that aggregate into national statistics for budget justification and international compliance reporting. Benefits from ritual compliance: demonstrates fiscal responsibility and regulatory adherence without resource pressure to actually innovate. Arbitrage: can exit or modify the constraint entirely (regulatory authority) but benefits from its continuation.
constraint_indexing:constraint_classification(preparedness_transmission__husk_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: COLD WAR INSTITUTIONAL LEGACY (PITON) — The drill/inspection regime originates in 1950s nuclear attack scenarios. Contemporary climate-driven flooding requires fundamentally different interventions (spatial planning, coastal retreat, watershed management). The Cold War protocols persist through institutional inertia: staff trained in 1970s protocols, budget lines historically labeled 'civil defense drills,' organizational identity fused with the ritual. Theater ratio: 0.81 reflects that 80%+ of inspection time is form-completion and compliance documentation, <20% is genuine failure-mode detection. The institution sees its own process as degraded but cannot exit without existential identity loss.
constraint_indexing:constraint_classification(preparedness_transmission__husk_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, institutional knowledge always decays in the absence of continuous practice. Drills cannot rehearse failure modes that have not yet occurred. Therefore, some gap between stated preparedness and actual capacity is an immutable property of complex systems. However, the structural data reveals this as a false summit: the gap is not evenly distributed. It concentrates in domains (novel climate scenarios, polyglot refugee dynamics) where the institution has minimal incentive to innovate. Where the institution DOES benefit from adaptation (pandemic preparedness, post-2008 financial regulation), capability grows despite similar institutional constraints.
constraint_indexing:constraint_classification(preparedness_transmission__husk_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__husk_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(preparedness_transmission__husk_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(preparedness_transmission__husk_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_transmission__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(preparedness_transmission__husk_reading, TR),
    TR >= 0.70.

:- end_tests(preparedness_transmission__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts credibility and resources from communities that depend on the organization's preparedness claims. The extraction is not total (some genuine capacity remains, and some communities have alternative risk management) but substantial. Suppression (0.68): High. Multiple mechanisms: legal penalties for violating certification protocols; career risk of admitting knowledge gaps; organizational hierarchy that punishes dissent; institutional identity fused with the ritual. Theater ratio (0.81): High. Contemporary drills are documented compliance exercises (form completion, checklist verification, simulation running according to pre-written scenarios) rather than genuine adaptive practice. Measurement data shows steady increase: theater began at 0.62 (1980s had more operational discretion, less documentation burden) and reached 0.81 (2010s are dominated by compliance theater). The theatrical component has grown as institutional bureaucracy expanded and climate hazards diverged from the protocol assumptions. Extractiveness also increased (0.38 → 0.52) over the interval because the gap between protocol coverage and actual disaster diversity widened: novel failure modes emerged faster than protocols adapted.
 *
 * PERSPECTIVAL GAP:
 *   Gap between husk reading (this story) and competence reading: The competence reading assumes post-disaster learning is effective — organizations update protocols when failures are observed. The husk reading assumes learning is blocked by institutional inertia, suppression of critical knowledge, and identity lock. Empirically distinguishable: measure speed of protocol revision post-disaster. If < 5% of identified gaps are addressed within 5 years: husk reading confirmed. If > 70%: competence reading confirmed. If 30-50%: hybrid reading confirmed. The gap is also interpretive: the husk reading takes institutional incentives (preserving budget, avoiding accountability) as the primary constraint on learning. The competence reading takes institutional capacity (expertise available, mechanisms for updating) as primary. These are different causal models of the same phenomenon.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position: beneficiary status (central government, bureaucratic continuity) produces low d (0.1-0.2), experiencing negative or near-zero effective extraction. Victim status (community, personnel) produces high d (0.85-0.95), experiencing maximum extraction. Organized actors with genuine coordination function (DRR community) occupy middle ground (d ≈ 0.50-0.55). The piton perspective (institutional legacy) experiences low d because it has arbitrage-level exit (could dissolve the constraint entirely) but chooses not to (identity lock creates perceived immobility). The analytical observer derives d ≈ 0.72 (canonical for analytical position), which would classify the constraint as Mountain if the observation were genuinely civilizational-scale. However, the false summit test flags this: beneficiaries exist (central government, bureaucratic continuity), and the structural mechanisms (suppression of dissent, career penalties, identity fusion) are contingent on institutional arrangements, not laws of nature.
 *
 * MANDATROPHY ANALYSIS:
 *   The husk reading resolves mandatrophy by asserting that the constraint is NOT coordination despite its appearance as an information standard (drills, inspections, protocols). The coordination function (enabling comparable metrics across jurisdictions) is real but subordinate to the extraction function (concentrating credibility in central government while dispersing risk to communities). The Tangled Rope classification from the DRR community perspective captures this: genuine coordination exists alongside asymmetric extraction. But from the community perspective (Snare) and the legacy institution perspective (Piton), the extraction dominates. The mandatrophy is resolved by recognizing that this constraint serves no authentic coordination function at the civilizational scale — it is not an information standard that actually enables adaptive learning. It is an information theater that produces statistics while suppressing the knowledge required to adapt.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_threshold_ambiguity,
    'At what ratio of drill coverage to actual novel failure modes does preparedness transition from ''legitimate knowledge lag'' to ''extractive husk''?',
    'Post-disaster forensic analysis: measure fraction of observed failure modes that drills had rehearsed vs. novel modes that emerged. Compare across jurisdictions with different drill intensities. If failure mode diversity >> drill scenario diversity even after scaling drill frequency, husk hypothesis confirmed.',
    'If threshold < 20% novel modes: drills are adequate and constraint is rope/tangled_rope. If threshold > 60% novel modes: drills are primarily performative and constraint is snare/piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(husk_threshold_ambiguity, empirical, 'Threshold distinguishing legitimate knowledge lag from extractive performance husk').

omega_variable(
    reading_identity_distinction,
    'Is this ''husk reading'' empirically distinguishable from the ''competence reading'' (assumption: institutions CAN update protocols) and ''hybrid reading'' (mixture of husk + genuine capacity)?',
    'Historical case analysis: identify jurisdictions that attempted protocol revision in response to post-disaster forensics. Measure speed of adoption (husk reading predicts <5% adoption; competence reading predicts >70%; hybrid predicts 30-50%). Track whether revision was innovation or ritual updating.',
    'If empirically indistinguishable: readings are interpretive frames, not structural claims. If distinguishable: readings describe real institutional variation in adaptation capacity. Classification and mandatrophy resolution depend on which reading applies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_distinction, empirical, 'Empirical distinctness of husk vs competence vs hybrid readings').

omega_variable(
    ritual_vs_function_extraction,
    'Is the suppression mechanism (0.68) primarily structural (legal penalties for non-compliance, certification requirements) or internalized (organizational identity fused with ritual, cognitive capture preventing perception of alternatives)?',
    'Field interviews with emergency personnel and administrative staff: probe for awareness of gap between protocol form and adaptive capability. If aware but silent: suppression is structural (fear of penalties). If genuinely unable to perceive gap: suppression is internalized (identity lock). Survey across multiple jurisdictions to estimate ratio.',
    'If structural suppression: husk can be dismantled by regulatory override. If internalized suppression: personnel carry the husk forward even if regulations are relaxed. Classification remains snare either way, but omega resolution affects exit strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_vs_function_extraction, empirical, 'Whether suppression mechanism is structural or internalized').

omega_variable(
    reading_kernel_ambiguity,
    'What is the kernel this reading is instantiating? Is it the Cold War civil defense institutional mandate, the generic concept of ''preparedness,'' or a narrower claim about drill efficacy?',
    'Document analysis: trace the lineage of drill/inspection protocols from original mandate through successive policy documents. Identify what claim each document grounds legitimacy in. If claim has remained stable: kernel is foundational. If claim has shifted: reading selection matters.',
    'If kernel is Cold War mandate: husk reading foreclosed by contemporary climate realities (axiom_overriding). If kernel is generic preparedness: husk and competence coexist (different institutional contexts). If kernel is drill efficacy: husk forecloses competence (empirical falsification).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Identity and stability of the kernel this reading instantiates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__husk_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_husk_theater_1980, preparedness_transmission__husk_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(prep_husk_theater_1995, preparedness_transmission__husk_reading, theater_ratio, 15, 0.74).
narrative_ontology:measurement(prep_husk_theater_2010, preparedness_transmission__husk_reading, theater_ratio, 30, 0.81).

% Extraction over time
narrative_ontology:measurement(prep_husk_extract_1980, preparedness_transmission__husk_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(prep_husk_extract_1995, preparedness_transmission__husk_reading, base_extractiveness, 15, 0.46).
narrative_ontology:measurement(prep_husk_extract_2010, preparedness_transmission__husk_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(prep_husk_suppress_1980, preparedness_transmission__husk_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(prep_husk_suppress_1995, preparedness_transmission__husk_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(prep_husk_suppress_2010, preparedness_transmission__husk_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__husk_reading, information_standard).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__husk_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% preparedness_transmission is a contested kernel with three readings: husk_reading (this constraint), competence_reading (institutional learning works), and hybrid_reading (mixture of both). All three are structurally distinct claims with different extractiveness values. The husk_reading emphasizes institutional inertia and suppression (ε=0.52); competence_reading emphasizes adaptive capacity (ε estimated 0.25-0.35); hybrid_reading acknowledges variation across institutional contexts (ε estimated 0.35-0.50). The three readings coexist as live interpretations held by different institutional actors and research communities. They are distinguished by different assumptions about organizational rationality and different post-disaster learning mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
