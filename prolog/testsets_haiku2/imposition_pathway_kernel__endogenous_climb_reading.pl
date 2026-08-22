% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__endogenous_climb_reading, []).

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
 *   constraint_id: imposition_pathway_kernel__endogenous_climb_reading
 *   human_readable: Commitment Displacement via Endogenous Fringe Adoption and Gradual Climb
 *   domain: social/historical
 *
 * SUMMARY:
 *   This constraint describes the process by which commitment changes
 *   (calendar reform, dress codes, administrative practices) appear to be
 *   top-down state impositions but are in fact state ratifications of gradual
 *   adoption that began in fringe populations — merchants, military
 *   modernizers, treaty-port dwellers — before any official decree. The
 *   endogenous_climb_reading asserts that all commitment displacement follows
 *   this hidden-fringe-to-visible-peak pathway: early adopters in
 *   economically or militarily advantaged positions climb the commitment
 *   curve first, often invisibly to the broader population; the state
 *   apparatus later decrees the change, presenting itself as the author when
 *   it is actually the final stage of a climb that began elsewhere. This
 *   reading contests two siblings: the exogenous_override_reading (state
 *   capacity can impose commitment without fringe adoption) and the
 *   hybrid_cascade_reading (state imposition creates artificial fringe that
 *   then climbs organically). The endogenous_climb_reading vindicates the
 *   proposition that cultural change is endogenous to society and that
 *   top-down policy follows (rather than initiates) adoption.
 *
 * KEY AGENTS:
 *   - state_administrative_apparatus: Claims to lead modernization; actually ratifies existing climb and extracts legitimacy from doing so.
 *   - merchant_and_modernizing_class: Adopts Western calendar and dress voluntarily in trade contexts; their climb is invisible to rural populations who experience only the state decree.
 *   - military_modernizers: Adopt Western practices for operational necessity; their adoption becomes evidence the state cites for the inevitability of reform.
 *   - rural_agricultural_population: Trapped by state decree; experiences change as imposed because the fringe adoption by merchant and military classes was invisible to them.
 *   - traditional_cultural_authorities: Excluded from deliberation; would contest the narrative that change is organic if they had voice.
 *   - comparative_historians: Analytical seat that makes visible the pre-decree adoption in archives, correspondence, and commercial records.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__endogenous_climb_reading, 0.42).
domain_priors:suppression_score(imposition_pathway_kernel__endogenous_climb_reading, 0.31).
domain_priors:theater_ratio(imposition_pathway_kernel__endogenous_climb_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__endogenous_climb_reading, rope).
narrative_ontology:human_readable(imposition_pathway_kernel__endogenous_climb_reading, "Commitment Displacement via Endogenous Fringe Adoption and Gradual Climb").
narrative_ontology:topic_domain(imposition_pathway_kernel__endogenous_climb_reading, "social/historical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__endogenous_climb_reading, 'c26be763-67a0-4e7f-ab5f-140a4e5bd80b').
narrative_ontology:cs_kernel_codification('c26be763-67a0-4e7f-ab5f-140a4e5bd80b', distributed).
narrative_ontology:cs_authority_grounding('c26be763-67a0-4e7f-ab5f-140a4e5bd80b', extraction).
narrative_ontology:cs_interpretation_layer_present('c26be763-67a0-4e7f-ab5f-140a4e5bd80b').
narrative_ontology:cs_reading_relation('c26be763-67a0-4e7f-ab5f-140a4e5bd80b', imposition_pathway_kernel__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('c26be763-67a0-4e7f-ab5f-140a4e5bd80b', imposition_pathway_kernel__hybrid_cascade_reading, coexists_with).
narrative_ontology:cs_axiom('c26be763-67a0-4e7f-ab5f-140a4e5bd80b', foundational, fringe_adoption_precedes_decree).
narrative_ontology:cs_axiom_status(fringe_adoption_precedes_decree, holdable).
narrative_ontology:cs_axiom_grounding('c26be763-67a0-4e7f-ab5f-140a4e5bd80b', fringe_adoption_precedes_decree, empirically_contingent).
narrative_ontology:cs_axiom('c26be763-67a0-4e7f-ab5f-140a4e5bd80b', foundational, state_ratifies_rather_than_initiates).
narrative_ontology:cs_axiom_status(state_ratifies_rather_than_initiates, holdable).
narrative_ontology:cs_axiom_grounding('c26be763-67a0-4e7f-ab5f-140a4e5bd80b', state_ratifies_rather_than_initiates, empirically_contingent).
narrative_ontology:cs_reference_frame('c26be763-67a0-4e7f-ab5f-140a4e5bd80b', organic_fringe_driven_adoption_model).
narrative_ontology:cs_drift_state('c26be763-67a0-4e7f-ab5f-140a4e5bd80b', post_decree_state_capture, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c26be763-67a0-4e7f-ab5f-140a4e5bd80b', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, merchant_and_modernizing_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, military_modernizers).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, military_modernizers).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, rural_agricultural_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares calendar reform (lunar to Gregorian) and mandates Western dress codes in official settings. Operates under the assumption that decree drives change, but in fact ratifies a climb that began in treaty ports and merchant districts. The state apparatus gains legitimacy from appearing to lead modernization, and its enforcement machinery (government offices, military drill) becomes the visible peak of a much longer adoption curve.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, state_administrative_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Adopts Western calendar and dress in treaty ports and international trading hubs before any state mandate. They benefit from calendar synchronization with foreign trading partners and dress codes aligned with international commerce. Their early adoption makes the state's later decree appear to enforce something already underway among economically dominant actors.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, merchant_and_modernizing_class, beneficiary,
    powerful, biographical, mobile, national).

% Adopt Western dress and calendars for operational coordination with modern military units. Their adoption is driven by functional necessity (synchronizing with Western-trained officers and coordinating operations on shared timescales) rather than cultural commitment. Once the military adopts, the state apparatus can point to this as evidence of the necessity and inevitability of the reform.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, military_modernizers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__endogenous_climb_reading, military_modernizers, payer).

% Faces the state decree on calendar and dress as externally imposed, even though adoption is technically voluntary for non-official contexts. Their economic cycles (planting, harvest) are rooted in lunar calendars; switching to Gregorian disrupts existing coordination among rural communities. They experience the constraint as top-down mandate because the fringe adoption by merchant and military classes is invisible to them — they see only the official decree.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, rural_agricultural_population, payer,
    powerless, generational, trapped, local).

% Priests, scholars, and custodians of traditional calendar and dress practices are not consulted in the reform process. They would argue that the change is externally imposed against endogenous cultural autonomy, but their voice is excluded from the reform deliberation. They bear the cost of cultural displacement without having been part of the climb.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, traditional_cultural_authorities, excluded,
    moderate, generational, constrained, national).

% Analyze the reform sequence and can document the pre-decree adoption among merchant and military classes. They examine archival evidence of treaty-port practice, personal correspondence, and commercial records to establish the fringe adoption timeline. Their role is to make visible the invisible climb stages.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, comparative_historians, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__endogenous_climb_reading, state_administrative_apparatus).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes timekeeping and formal appearance across state institutions and international trade. A shared calendar enables coordinated governance and commerce; Western dress in official contexts signals participation in modern state administration and international diplomacy.
% TRANSFER_FUNCTION: Moves the legitimacy of cultural change from merchants and military modernizers (who adopted first) to the state apparatus (which decrees change and claims to lead it). The state extracts the authority to direct culture while the fringe bears the cost of initial adoption and cultural disruption.
% ABSENT_VOICES: Traditional cultural authorities and rural communities are excluded from deliberation. They would attest that the change feels imposed rather than climbed, and they would challenge the narrative that merchant-driven adoption represents genuine cultural consensus rather than economic coercion by a more powerful class.
% DISAPPEARANCE_RATIONALE: If the state decree were removed, the climb would continue — merchant and military classes would maintain Western calendars and dress because they solve genuine coordination problems in their domains. Rural adoption would slow or reverse in non-official contexts, but the state's role as the visible architect of change would disappear. The world rearranges because the constraint's actual function (ratifying existing climb) would no longer be confused with its declared function (initiating change from above).
% FOUNDING_PROBLEM: International trade and military modernization require synchronization with Western timekeeping and dress codes; early merchant and military classes adopted voluntarily to solve this coordination problem before the state noticed.
% FOUNDING_PROBLEM_CORROBORATION: Trade historians and military historians document pre-decree adoption in treaty ports and modernized units. Merchants' personal correspondence and commercial ledgers show calendar conversions in use before the official Gregorian reform. Military drill manuals show dress codes aligned with Western standards before the state mandate. The corroboration comes from historical sources external to state archives and state justifications — it is the fringe adoption record that state narratives omit.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_pathway_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__endogenous_climb_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__endogenous_climb_reading_tests).
:- end_tests(imposition_pathway_kernel__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises gradually from 0.15 to 0.42 over the interval, modeling the shift from genuine coordination (early merchant/military adoption solving real problems) to state extraction of legitimacy (the decree and its enforcement). At t=0, extraction is low because fringe adoption is voluntary and solves actual problems (trade synchronization, military efficiency). By t=20, extraction rises as the state apparatus consolidates the narrative of having initiated the change, claiming authority over cultural modernization. By t=40, extraction plateaus at 0.42: the state continues to extract authority and prestige from the reform, but the climb is substantially complete and suppression needs are lower because adoption has become embedded in institutional routine. Theater_ratio rises from 0.05 to 0.28, showing increasing performative component: early merchant adoption is functionally motivated (no theater); late-stage enforcement of the state decree in rural areas is largely theater (the change is already embedded where it matters). Suppression_requirement rises from 0.08 to 0.31, modeling the enforcement burden of convincing populations who were not part of the climb that the change is both inevitable and beneficial. The shared time grid ensures all three metrics are authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus and merchants/modernizers see the constraint as a natural progression of modernization they are leading or participating in — a rope coordinating society toward functional improvement. Rural populations and traditional authorities see the same constraint as an imposed cultural disruption, a snare justified by an elite narrative of inevitability. The engine computes this seat-level divergence from the structural data: high d for rural powerless agents yields snare-class computation; low d for state institutional agents yields rope-class computation. The authored claim (rope) represents the state's own framing; the authored metrics (moderate extraction, rising theater, low-to-moderate suppression) represent the actual structure independent of that framing. The divergence between claim and metrics is exactly what the corpus is built to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_administrative_apparatus (institutional power, arbitrage exit) sits at low directionality (d near 0.2–0.3): it benefits from claim-making and legitimacy extraction while the actual burden of change (retraining, cultural disruption) falls elsewhere. Merchant_and_modernizing_class (powerful power, mobile exit) sits at low directionality (d near 0.15–0.25): they initiated the climb voluntarily and continue to benefit from it; they experience minimal extraction because they drove the adoption. Military_modernizers (organized power, constrained exit) sit near symmetric (d near 0.45–0.55): they benefit from operational efficiency but are constrained by military hierarchy; the state uses their adoption as justification, extracting some value from their involuntary role as exemplars. Rural_agricultural_population (powerless power, trapped exit) sits at high directionality (d near 0.75–0.85): they were not part of the climb, experience the constraint as imposed, and bear cultural disruption costs with no offsetting benefit. The directionality profile explains why different seats should compute different constraint types: from the state's view, the constraint is rope (genuine coordination it steers); from the rural view, it is tangled_rope or snare (extraction disguised as inevitability).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (international coordination and military modernization) is genuinely live, but the constraint's mandate has partially outlived its functional necessity. By t=30, the calendar change and dress codes are embedded in routine; enforcement is increasingly about cultural assertion rather than functional coordination. The theater_ratio rising to 0.28 by t=40 signals this: the state continues to perform leadership of modernization, but the change is self-sustaining through institutional inertia and generational turnover. The classification prevents mislabeling this as pure rope: if the constraint were genuinely coordinating, suppression_requirement would be near zero (voluntary coordination requires no force). The rising suppression requirement (0.08 to 0.31) indicates that as functional need diminishes, enforcement intensity must increase to maintain the change — a classic marker of mandatrophy. The constraint will compute as tangled_rope or snare from rural seats because the metrics show increasing coercion supporting a declining functional problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fringe_visibility_threshold,
    'At what point does fringe adoption become visible and acknowledged in state narratives? Is the invisibility of early adoption structural or narrative-contingent?',
    'Archive analysis of state policy documents over time: when did state policymakers begin citing merchant and military adoption as precedent vs. claiming to initiate change? Comparison with contemporary merchant records and military documents to establish whether state actors had access to knowledge of early adoption.',
    'If invisibility is structural (state lacked access to information), the constraint is rope with a transparent legitimacy gap. If invisibility is narrative (state chose not to acknowledge pre-decree adoption), the constraint reclassifies toward snare — active suppression of alternative historical narratives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_visibility_threshold, empirical, 'Whether the fringe adoption was unknown to state policymakers or deliberately omitted from state narratives.').

omega_variable(
    functional_necessity_decay,
    'At what point does the functional problem (coordinating with Western trade and military standards) cease to drive adoption, and the constraint becomes purely extractive of state legitimacy?',
    'Analysis of adoption curves post-decree: if adoption accelerates dramatically after the state decree (indicating the decree was the primary driver), the constraint is more exogenous_override than endogenous_climb. If adoption curve flattens after the decree (indicating the decree sealed existing adoption rather than initiating new adoption), the constraint is more endogenous_climb.',
    'A steeply accelerating adoption curve post-decree would support the exogenous_override_reading. A plateau or modest rise post-decree would support this reading (endogenous_climb). The functional necessity decay point determines when the constraint transitions from rope (solving a genuine problem) to tangled_rope (problem solved, extraction continues through momentum).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_necessity_decay, empirical, 'Whether the state decree accelerates adoption or ratifies existing climb.').

omega_variable(
    rural_resistance_as_structural_signal,
    'Does rural resistance to the constraint come from genuine cultural preference for traditional calendar/dress, or from rational protest against being excluded from the adoption decision? Is the resistance identity-based or participatory?',
    'Ethnographic and historical analysis of rural protest and adoption patterns: did rural populations adopt when given choice/voice, or did they maintain traditional practices even when legal barriers were removed?',
    'If resistance is identity-based (rural populations prefer traditional practices), the constraint''s extraction is high and the reading remains endogenous_climb (early adopters excluded rural populations from climb). If resistance is participatory (rural populations would have adopted voluntarily if included in deliberation), the constraint is more snare-like (suppressing voice, not just ratifying climb).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rural_resistance_as_structural_signal, conceptual, 'Whether rural resistance reflects cultural preference or exclusion from agency in change process.').

omega_variable(
    reading_vs_alternative_framings,
    'Does this constraint represent a genuine reading of a commitment system dispute, or is the dispute primarily about whether cultural change is good/bad rather than how it propagates?',
    'Examine primary sources from all parties: do state actors, merchants, and rural populations actually dispute the mechanism of change (fringe-to-peak vs. top-down), or do they dispute whether the change itself is desirable? Do they make competing claims about causation, or only about value?',
    'If the dispute is genuinely structural (competing causal theories of propagation), the reading_relations are correct: forecloses vs. coexists_with vs. influences reflect incompatible claims about how change happens. If the dispute is primarily evaluative (good/bad change), the reading may be less a commitment-system kernel and more a preference disagreement, reclassifying the constraint out of the CS-constraint category.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_alternative_framings, conceptual, 'Whether the kernel contest is about mechanism of propagation or about evaluative preferences regarding change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__endogenous_climb_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(impo_tr_t5, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 5, 0.08).
narrative_ontology:measurement(impo_tr_t10, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(impo_tr_t15, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(impo_tr_t20, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(impo_tr_t25, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement(impo_tr_t30, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(impo_tr_t40, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(impo_be_t5, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(impo_be_t10, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(impo_be_t15, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(impo_be_t20, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(impo_be_t25, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 25, 0.4).
narrative_ontology:measurement(impo_be_t30, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 30, 0.41).
narrative_ontology:measurement(impo_be_t40, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(impo_su_t5, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(impo_su_t10, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 10, 0.16).
narrative_ontology:measurement(impo_su_t15, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 15, 0.22).
narrative_ontology:measurement(impo_su_t20, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 20, 0.27).
narrative_ontology:measurement(impo_su_t25, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 25, 0.29).
narrative_ontology:measurement(impo_su_t30, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 30, 0.3).
narrative_ontology:measurement(impo_su_t40, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 40, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__endogenous_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__endogenous_climb_reading, 0.1).
narrative_ontology:affects_constraint(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-part kernel contest about how commitment displacement propagates. The endogenous_climb_reading asserts that all apparent top-down impositions (calendar/dress reforms, administrative changes) are state ratifications of fringe adoption that began in merchant, military, and treaty-port populations. It forecloses the exogenous_override_reading (state capacity can impose without fringe pathway) by asserting the fringe stage is always present and always precedes visible decree. It coexists_with the hybrid_cascade_reading, which agrees the fringe is present but argues the state creates the artificial fringe via military conscription and bureaucratic employment rather than the fringe climbing independently. The three readings share a kernel (how does commitment change propagate) but disagree on causation and agency. Each reading has its own ε: endogenous_climb models moderate extraction (state extracting legitimacy from pre-existing climb); exogenous_override would model lower extraction if state truly initiates (pure coordination); hybrid_cascade would model higher extraction (state artificially creates fringe, then uses climb to justify broader reform). The constraint family is linked via affects_constraints to enable contamination analysis: if one reading's core premise is challenged by evidence, the dependent readings require reclassification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_pathway_kernel__endogenous_climb_reading, powerless, 0.78).
constraint_indexing:directionality_override(imposition_pathway_kernel__endogenous_climb_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
