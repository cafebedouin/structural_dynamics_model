% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__catastrophe_validation_axis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__catastrophe_validation_axis, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: tsunami_stone_commitment__catastrophe_validation_axis
 *   human_readable: Tsunami Stone Commitment: Catastrophe Validation Axis
 *   domain: disaster_anthropology/commitment_systems
 *
 * SUMMARY:
 *   Japanese coastal communities maintained stone markers inscribed with
 *   warnings about tsunami danger, dating back centuries (evidence from
 *   markers in Aneyoshi, Iwate; Numakunishi; and other sites). The stones
 *   encoded multi-generational knowledge: when tsunamis struck, evacuate to
 *   high ground; do not trust the ocean's calm. During Japan's post-1960
 *   rapid modernization and technological ascendancy, this traditional
 *   knowledge system decayed — stones were moved, their meanings forgotten,
 *   and institutional faith shifted entirely to engineered seawalls and
 *   early-warning systems. In 2011, a magnitude-9.1 megathrust earthquake
 *   triggered a tsunami that overwhelmed engineered defenses. In areas where
 *   the stone markers and their cultural meaning had persisted, evacuation
 *   rates were significantly higher and mortality lower. In areas where
 *   institutional memory had been lost, casualties were catastrophic. The
 *   2011 tsunami thus served as a decisive empirical test of the stone
 *   commitment's validity: it proved the encoding of multi-generational
 *   knowledge in physical form survives and operates with behavioral force
 *   across centuries of technological change. This constraint story
 *   instantiates that reading — the catastrophe as validation event — and is
 *   ONE of three contested readings of the same kernel (the stone commitment
 *   itself).
 *
 * KEY AGENTS:
 *   - Intergenerational memory system (non-agent, beneficiary): the abstract constraint that benefits from the stones' persistent inscription.
 *   - Pre-2011 coastal residents (payer, powerless): lived in trapped circumstance; exit was zero when tsunami struck. Those who heeded stone warnings survived; those who ignored them perished.
 *   - Post-tsunami institutional authorities (agenda-setter, institutional): documented the 2011 event, produced empirical evidence validating the stones' warning. Their interpretation determines whether validation persists in policy.
 *   - Contemporary stone keepers (beneficiary, organized): local communities maintaining physical markers and transmitting their meaning. The 2011 validation restores their stewardship legitimacy.
 *   - Competing modern infrastructure (excluded, powerful): engineered defenses and alert systems. The stones represent an alternative legitimacy claim; 2011 partial validation complicates the exclusive reliance on technology.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__catastrophe_validation_axis, 0.02).
domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, 0.0).
domain_priors:theater_ratio(tsunami_stone_commitment__catastrophe_validation_axis, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, extractiveness, 0.02).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__catastrophe_validation_axis, mountain).
narrative_ontology:human_readable(tsunami_stone_commitment__catastrophe_validation_axis, "Tsunami Stone Commitment: Catastrophe Validation Axis").
narrative_ontology:topic_domain(tsunami_stone_commitment__catastrophe_validation_axis, "disaster_anthropology/commitment_systems").

domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__catastrophe_validation_axis, 'd8fdb5e6-f9f6-4cfa-9b9e-57c8fdc50724').
narrative_ontology:cs_kernel_codification('d8fdb5e6-f9f6-4cfa-9b9e-57c8fdc50724', distributed).
narrative_ontology:cs_authority_grounding('d8fdb5e6-f9f6-4cfa-9b9e-57c8fdc50724', practice).
narrative_ontology:cs_interpretation_layer_present('d8fdb5e6-f9f6-4cfa-9b9e-57c8fdc50724').
narrative_ontology:cs_reading_relation('d8fdb5e6-f9f6-4cfa-9b9e-57c8fdc50724', tsunami_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('d8fdb5e6-f9f6-4cfa-9b9e-57c8fdc50724', tsunami_stone_commitment__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('d8fdb5e6-f9f6-4cfa-9b9e-57c8fdc50724', foundational, empirical_validation_adjudicates_kernel_content).
narrative_ontology:cs_axiom_status(empirical_validation_adjudicates_kernel_content, holdable).
narrative_ontology:cs_axiom_grounding('d8fdb5e6-f9f6-4cfa-9b9e-57c8fdc50724', empirical_validation_adjudicates_kernel_content, empirically_contingent).
narrative_ontology:cs_axiom('d8fdb5e6-f9f6-4cfa-9b9e-57c8fdc50724', foundational, multi_generational_knowledge_transmission_is_natural_law).
narrative_ontology:cs_axiom_status(multi_generational_knowledge_transmission_is_natural_law, holdable).
narrative_ontology:cs_axiom_grounding('d8fdb5e6-f9f6-4cfa-9b9e-57c8fdc50724', multi_generational_knowledge_transmission_is_natural_law, deontological).
narrative_ontology:cs_reference_frame('d8fdb5e6-f9f6-4cfa-9b9e-57c8fdc50724', pre_modern_coastal_knowledge_system).
narrative_ontology:cs_drift_state('d8fdb5e6-f9f6-4cfa-9b9e-57c8fdc50724', post_2011_validation_moment, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('d8fdb5e6-f9f6-4cfa-9b9e-57c8fdc50724', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__catastrophe_validation_axis, intergenerational_memory_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__catastrophe_validation_axis, contemporary_stone_keepers).
narrative_ontology:constraint_victim(tsunami_stone_commitment__catastrophe_validation_axis, pre_2011_coastal_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The abstracted constraint that benefits from stone inscription persistence: a mechanism by which knowledge of tsunami risk is transmitted through centuries. Not a real actor; represents the constraint's function as a nonhuman system.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, intergenerational_memory_system, beneficiary,
    analytical, civilizational, analytical, local).
narrative_ontology:stakeholder_non_agent(tsunami_stone_commitment__catastrophe_validation_axis, intergenerational_memory_system).

% Lived in areas where 2011 tsunami struck with catastrophic force. The stone inscription was present in Aneyoshi, Iwate prefecture, and other coastal areas. Their exit option was zero: tsunami obeys no social negotiation. Those who heeded the stone markers' warning to evacuate to high ground survived; those who ignored it or had not internalized the cultural practice perished.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, pre_2011_coastal_residents, payer,
    powerless, biographical, trapped, local).

% Government agencies, university researchers, and disaster response organizations documented the 2011 tsunami's behavior and survivor patterns. They produced official records, scientific data, and policy recommendations. Their capacity to interpret the validation evidence and translate it into institutional memory determines whether the stone commitment's empirical test result persists in policy or fades.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, post_tsunami_institutional_authorities, agenda_setter,
    institutional, generational, arbitrage, regional).

% Local communities, families, and preservation groups who maintain the physical stones and transmit knowledge of their meaning. They derive legitimacy from the stones' predictive success in 2011; the constraint validates their stewardship role.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, contemporary_stone_keepers, beneficiary,
    organized, biographical, mobile, local).

% Engineered seawalls, early warning systems, and digital alert networks compete to provide tsunami defense. The stone markers represent an alternative legitimacy claim: low-tech, intergenerational, knowledge-based rather than technology-based. Their exclusion from consideration means modern systems bear alone the burden of proof that their approach is superior; the 2011 tsunami partially validated the stone marker approach, complicating that burden.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, competing_modern_infrastructure, excluded,
    powerful, immediate, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tsunami_stone_commitment__catastrophe_validation_axis, diffuse).
narrative_ontology:fixing_cost_class(tsunami_stone_commitment__catastrophe_validation_axis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes and transmits knowledge of tsunami frequency and danger across generations: a mechanism by which a community at risk stores empirical knowledge in a form that persists through centuries of social, linguistic, and technological change without requiring institutional archives, written records, or centralized authority.
% TRANSFER_FUNCTION: No goods move through this constraint; instead, behavioral obligation transfers from the stones' existence and inscription to inhabitants' evacuation decisions. Pre-2011, this was an opaque transfer — compliance or non-compliance was invisible to external observers. Post-2011, the transfer became legible: areas with stone markers showed higher evacuation rates and lower mortality, quantifying the constraint's behavioral force.
% ABSENT_VOICES: Pre-2011 institutional disaster management and modern engineering authorities were largely absent from consideration of traditional stone markers' validity. They might have argued (and some did) that such markers were superstition or folk memory, not reliable hazard information. The 2011 tsunami's empirical validation made their absence from the decision-making process visible in retrospect.
% DISAPPEARANCE_RATIONALE: If the stone commitments and their cultural transmission machinery had been lost before 2011 (as nearly happened during Japan's rapid modernization 1960–2000), the 2011 tsunami would have struck without intergenerational behavioral guidance. Mortality would have been substantially higher in areas where stones had been removed or memory of their meaning lost. The constraint's disappearance would have left coastal communities dependent solely on engineered defenses and alert systems, both of which partially failed on the day.
% FOUNDING_PROBLEM: Coastal communities in Japan experienced catastrophic tsunami at multi-century intervals; any single generation might experience none and lose knowledge of proper response behavior. Stone markers with inscriptions emerged as a mechanism to maintain that knowledge across the inter-event gap, storing behavioral instruction in a physical form that survives cultural forgetting.
% FOUNDING_PROBLEM_CORROBORATION: Seismic and oceanographic evidence confirms that mega-thrust subduction-zone earthquakes and tsunamis strike the Japanese coast at 100–300 year intervals, making multi-generational forgetting a persistent problem. The 2011 tsunami proved that the founding problem remained live: modern Japan almost forgot despite centuries of available stone markers. Archaeological and historical evidence from earlier tsunamis (1611, 1707, 1854) shows similar behavioral patterns — survivors at higher ground, drowning at lower elevations — validating the stone markers' encoding of this knowledge.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__catastrophe_validation_axis, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__catastrophe_validation_axis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__catastrophe_validation_axis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tsunami_stone_commitment__catastrophe_validation_axis, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__catastrophe_validation_axis, 0.02, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, ExtMetricName, E),
    domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This constraint is authored as a Mountain because the physical phenomena it encodes — tsunami behavior, geology, sea-level dynamics — are natural law. The stones are human artifacts, but what they encode (hazard knowledge) is invariant to human preference: tsunamis strike whether or not humans know they will, and evacuation to high ground survives the motion regardless of the cultural practice that transmitted that knowledge. Extractiveness is negligibly small (0.02) because the constraint imposes no distributional cost: no party collects rents from the stones' existence; no party is exploited by adherence to evacuation warnings. Suppression is zero because enforcement is not coercive — the 2011 tsunami's behavior validated the stones' content, eliminating incentive to suppress. Theater ratio was elevated during the modernization era (0.68 by 2000) when stones persisted as symbolic/commemorative artifacts divorced from their actual behavioral force. The 2011 validation event collapsed theater to near-zero (0.05) because the stones' meaning was suddenly and catastrophically operationalized — they went from decoration to life-or-death guidance in real time. Accessibility collapse is high (0.92) because once a mega-thrust tsunami is understood to recur every 100–300 years in a given region, the logic of the evacuation rule becomes inescapable; no credible alternative exists that maintains coastal human settlement while denying tsunami risk. Resistance is near-zero (0.02) because the constraint encounters no organized opposition; the 2011 validation silenced skepticism. The claim/metric independence is maintained: the constraint is CLAIMED as a Mountain (natural law, no distributional extraction) and the metrics describe a system with negligible extractiveness, zero suppression, and high accessibility collapse. These align, and the alignment is appropriate for a genuine natural law.
 *
 * PERSPECTIVAL GAP:
 *   Different institutional seats experience the stone commitment differently post-2011. From the coastal residents' seat (powerless, trapped), the stones are a life-or-death transmission mechanism with no gap — the constraint's operation was stark and immediate. From the institutional authorities' seat (powerful, arbitrage options), the stones represent an analytical puzzle: a traditional system that partially out-performed modern engineered defenses, raising questions about institutional trust and policy design. From the competing modern infrastructure's seat (powerful, excluded), the 2011 validation is an embarrassment and threat — it destabilizes claims to exclusive competence in coastal protection. The engine should compute divergent type classifications across these seats despite the constraint's Mountain candidacy, because institutional seats carry different directionality toward the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is highly asymmetric for this constraint. The intergenerational_memory_system (non-agent beneficiary, analytical seat) has d near 0.0 because it benefits entirely from the stones' persistence and faces no cost. Pre-2011 coastal residents (powerless, trapped) have d near 1.0 because they are entirely at the mercy of the tsunami physics the stones encode; exit is zero. The 2011 tsunami validated the constraint, shifting coastal residents' perception from potential targets to informed survivors — those who heeded the stones benefited from knowledge transmission, effectively flipping their d toward beneficiary-side. Post-tsunami institutional authorities (institutional, arbitrage) have d near 0.5: they benefit from the validation evidence (scientific credibility, policy authority) but bear costs (institutional humility when traditional systems outperform engineering). Stone keepers (organized, mobile) have d near 0.2: they derive legitimacy from stewardship but bear modest maintenance costs, and they have moderate exit options (they could abandon maintenance). The engine's directionality derivation should account for these asymmetries from the structural data: beneficiaries near 0.0, victims/trapped near 1.0, institutional agenda-setters near 0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The stone commitment nominally exhibits mandatrophy dynamics: the founding problem (multi-generational knowledge loss of tsunami hazard) persists, but institutional memory of the stones' purpose nearly vanished during 1960–2000 modernization. The mandate — intergenerational transmission of hazard knowledge — outlived active practice in most areas. However, the 2011 tsunami revitalized both the mandate and the mechanism simultaneously. This is unusual: instead of a decayed constraint persisting by inertia (piton), the catastrophic empirical validation re-animated the commitment. Mandatrophy may yet reassert itself post-2011 if communities lose the reawakened memory of the stones' importance within the next generation; the constraint remains fragile despite its Mountain classification. The threat is institutional forgetting, not structural failure — if the 2011 memory fades before it is encoded into sustained cultural practice and education systems, the stones could return to commemorative status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_legitimacy,
    'Is the stone commitment a purely natural phenomenon (tsunami physics is invariant regardless of human knowledge transmission) or does its efficacy depend on constructed social institutions (human compliance with historically-encoded warnings)?',
    'Counterfactual comparison: areas where stone markers and their cultural memory persisted vs. areas where markers were removed or forgotten, measured by evacuation rate and mortality in the 2011 tsunami. The divergence in outcomes would indicate the extent to which social construction (memory maintenance) amplifies physical constraint (tsunami behavior).',
    'If the constraint is purely natural, it remains a Mountain regardless of institutional decay. If efficacy depends on constructed memory transmission, the constraint degraded during 1960–2000 modernization and was partially restored by 2011 validation. This determines whether the constraint is a permanent fixture or an inertial system dependent on continued cultural work.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_legitimacy, empirical, 'Whether stone commitment efficacy is natural law or culturally contingent.').

omega_variable(
    validation_sufficiency_for_resurrection,
    'Does one empirical validation event (2011 tsunami) suffice to restore a decaying constraint (stone markers and their cultural meaning) to full behavioral force across generations, or does institutional memory require continuous cultural practice, not single-event proof?',
    'Longitudinal tracking of coastal community behavior post-2011: are stone markers being maintained and transmitted to new generations, or is the validation event cited in media/institutional contexts while ground-level practice remains commercialized/forgotten? Do we see sustained behavioral change (renewed evacuation drills, memorial maintenance, education integration) or temporary attention spike that fades?',
    'If single validation suffices, the constraint rebounds to pre-1960 force and the 2011 tsunami is a permanent re-anchoring event. If continuous practice is required, the constraint remains fragile despite validation, and the catastrophe was a temporary rescue operation for institutional memory, not a restoration of stable transmission.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(validation_sufficiency_for_resurrection, empirical, 'Whether catastrophic empirical validation can restore decayed intergenerational commitments.').

omega_variable(
    kernel_reading_indeterminacy,
    'The same 2011 tsunami empirical event can ground three different readings of the stone commitment kernel: (1) behavioral_competence_reading argues the validation proves active norm enforcement persisted; (2) commemorative_husk_reading argues the validation was coincidental — communities that happened to evacuate would have survived anyway; (3) catastrophe_validation_axis argues the 2011 tsunami serves as a decisive binary test of the kernel''s content. Which reading is the empirical event actually supporting, and can that be determined from the data?',
    'Multi-level analysis of 2011 responses across communities: (a) communities with strong stone-marker cultural transmission vs. weak transmission; (b) communities with engineered defenses vs. stone markers only; (c) communities with urban development that removed markers vs. preserved markers. If behavioral_competence reading is correct, transmission strength should predict outcome. If commemorative_husk reading is correct, outcome should track engineered defense quality, not marker presence. If catastrophe_validation_axis is correct, the 2011 event produces a sharp binary: areas that had maintained the stone commitment survived better than areas that had lost it.',
    'This omega names the irreducible committer ambiguity: the three readings share the same kernel (stone markers exist, 2011 tsunami happened) but interpret the empirical validation differently. Only detailed community-level data and ethnographic follow-up can distinguish whether validation was real or coincidental, and what it was validating.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether 2011 tsunami validly adjudicates among the three kernel readings or remains ambiguous.').

omega_variable(
    extractiveness_floor_near_zero,
    'Why is extractiveness near zero (0.02) when the constraint is active and requires enforcement? Is extractiveness genuinely negligible because the constraint imposes no distributional cost, or is the measurement framework failing to capture the intergenerational burden imposed by mandatory knowledge transmission?',
    'Audit whether the stone commitment extracts any resource from contemporary populations: does maintenance cost money? Does memorial practice displace other economic activity? Do evacuation drills create opportunity cost? If yes to any, extractiveness is higher than 0.02. If no, the constraint is genuinely non-extractive and the near-zero value is correct.',
    'If extractiveness is genuinely ~0.0, the constraint is a pure mountain with no distributional asymmetry — accessibility collapse and resistance metrics suffice to characterize it. If extractiveness is actually moderate, the constraint is more like a Rope (coordination with some institutional cost) and the measurement error represents a significant false-negative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_floor_near_zero, empirical, 'Whether near-zero extractiveness accurately represents stone commitment burden, or measurement framework is insufficient.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__catastrophe_validation_axis, 1700, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t1700, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1700, 0.0).
narrative_ontology:measurement(tsun_tr_t1854, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1854, 0.0).
narrative_ontology:measurement(tsun_tr_t1950, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(tsun_tr_t1980, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 1980, 0.45).
narrative_ontology:measurement(tsun_tr_t2000, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 2000, 0.68).
narrative_ontology:measurement(tsun_tr_t2011, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 2011, 0.05).

% Extraction over time
narrative_ontology:measurement(tsun_be_t1700, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1700, 0.02).
narrative_ontology:measurement(tsun_be_t1854, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1854, 0.02).
narrative_ontology:measurement(tsun_be_t1950, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1950, 0.02).
narrative_ontology:measurement(tsun_be_t1980, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 1980, 0.02).
narrative_ontology:measurement(tsun_be_t2000, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 2000, 0.02).
narrative_ontology:measurement(tsun_be_t2011, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 2011, 0.02).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(tsunami_stone_commitment__catastrophe_validation_axis, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__catastrophe_validation_axis, information_standard).
narrative_ontology:boltzmann_floor_override(tsunami_stone_commitment__catastrophe_validation_axis, 0.01).
narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment__behavioral_competence_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The tsunami_stone_commitment kernel encompasses three structurally distinct constraint stories, each a reading of the same persistent human practice. This story (catastrophe_validation_axis) treats 2011 as the decisive empirical adjudication point: areas that maintained stone-marker knowledge survived with lower mortality. The behavioral_competence_reading argues active norm enforcement preserved the stones' force pre-2011; the commemorative_husk_reading argues the stones had become decoration. All three readings must account for the empirical mortality differentials, but they explain them differently — the validating fact is shared; the causal interpretation diverges. This story's reading assumes 2011 empirical data resolves structural claims about the kernel's content; the sibling readings interpret the same data within different causal frames.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
