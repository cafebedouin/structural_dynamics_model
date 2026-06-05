% ============================================================================
% CONSTRAINT STORY: social_revolution_provisions__reservation_architecture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_revolution_provisions__reservation_architecture, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: social_revolution_provisions__reservation_architecture
 *   human_readable: Constitutional Reservation Architecture in Indian Equality Framework
 *   domain: constitutional_law/social_justice/affirmative_action
 *
 * SUMMARY:
 *   The Indian Constitution's reservation architecture represents one reading
 *   of the social revolution kernel — that equality is not merely the absence
 *   of status-discrimination but the active correction of systematic
 *   exclusion through guaranteed access. This reading embeds reservations
 *   (scheduled seats, posts, and places) directly into the equality articles
 *   (Articles 15, 16, 17) as their fulfillment rather than their exception.
 *   The constraint is a tangled coordination-and-extraction hybrid: it
 *   genuinely coordinates around overcoming structural exclusion (Rope from
 *   the beneficiary perspective) while simultaneously extracting from
 *   unreserved competitors at the margin and suppressing formal-equality
 *   doctrinal objections (Snare from the competitor perspective). The textual
 *   design — embedding reservations into the equality framework itself —
 *   forecloses certain contestations while enabling others. The creamy-layer
 *   exclusion and periodic-review mechanisms embed a sunset logic,
 *   positioning the system as temporary coordination (Scaffold from the
 *   advancement perspective) while the formal-equality reader risks
 *   naturalizing this as an immutable principle (false-summit Mountain). The
 *   constraint exhibits increasing suppression over its 75-year interval as
 *   formal-equality objections accumulate and institutional contestation
 *   intensifies, while extractiveness remains moderate because the quota
 *   mechanism retains genuine coordination function.
 *
 * KEY AGENTS:
 *   - Scheduled Castes (SC) Entrants: Primary beneficiary (powerful/arbitrage) — experience reservation as enabling access; net beneficiary within the constitutional frame
 *   - Scheduled Tribes (ST) Entrants: Primary beneficiary (powerful/arbitrage) — similar structural position to SC entrants; quotas solve coordination problem of overcoming institutional barriers
 *   - Other Backward Classes (OBC) Entrants: Primary beneficiary (powerful/arbitrage) — extended to this category through constitutional amendment; coordinate similarly to SC/ST entrants
 *   - Unreserved Competitors at Margin: Primary victim (powerless/trapped) — face absolute exclusion from reserved seats; cannot appeal through formal-equality doctrine due to textual embedding
 *   - Administrative State / Merit Institutions: Institutional actor (institutional/constrained) — must implement reservations while maintaining legitimacy through merit narratives; experiences mixed coordination (must overcome structural barriers) and extraction (must enforce quota while defending meritocracy)
 *   - Dominant-Caste Organized Coalition: Secondary victim (organized/constrained) — organized around formal-equality objections; suppressed alternatives include caste-census data, wealth distribution analysis, structural unemployment
 *   - Creamy Layer / Advanced Groups: Organized actor (organized/mobile) — anticipate advancement beyond reservation eligibility; see the constraint as temporary (Scaffold perspective)
 *   - Formal-Equality Doctrinalists: Analytical reader (analytical/analytical) — risk naturalizing formal equality as immutable principle, obscuring the contingent design choice to embed reservations into the equality articles
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_revolution_provisions__reservation_architecture, 0.38).
domain_priors:suppression_score(social_revolution_provisions__reservation_architecture, 0.52).
domain_priors:theater_ratio(social_revolution_provisions__reservation_architecture, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_revolution_provisions__reservation_architecture, extractiveness, 0.38).
narrative_ontology:constraint_metric(social_revolution_provisions__reservation_architecture, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(social_revolution_provisions__reservation_architecture, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_revolution_provisions__reservation_architecture, tangled_rope).
narrative_ontology:human_readable(social_revolution_provisions__reservation_architecture, "Constitutional Reservation Architecture in Indian Equality Framework").
narrative_ontology:topic_domain(social_revolution_provisions__reservation_architecture, "constitutional_law/social_justice/affirmative_action").

domain_priors:requires_active_enforcement(social_revolution_provisions__reservation_architecture).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(social_revolution_provisions__reservation_architecture, 'a8285d6b-2020-45a7-b70b-07e1c23f5468').
narrative_ontology:cs_kernel_codification('a8285d6b-2020-45a7-b70b-07e1c23f5468', formalized).
narrative_ontology:cs_authority_grounding('a8285d6b-2020-45a7-b70b-07e1c23f5468', lineage).
narrative_ontology:cs_interpretation_layer_present('a8285d6b-2020-45a7-b70b-07e1c23f5468').
narrative_ontology:cs_reading_relation('a8285d6b-2020-45a7-b70b-07e1c23f5468', social_revolution_provisions__personal_law_compromise, coexists_with).
narrative_ontology:cs_reading_relation('a8285d6b-2020-45a7-b70b-07e1c23f5468', social_revolution_provisions__untouchability_abolition_article_17, influences).
narrative_ontology:cs_axiom('a8285d6b-2020-45a7-b70b-07e1c23f5468', foundational, affirmative_equality_requires_structural_correction).
narrative_ontology:cs_axiom_status(affirmative_equality_requires_structural_correction, holdable).
narrative_ontology:cs_axiom_grounding('a8285d6b-2020-45a7-b70b-07e1c23f5468', affirmative_equality_requires_structural_correction, deontological).
narrative_ontology:cs_axiom('a8285d6b-2020-45a7-b70b-07e1c23f5468', foundational, formal_equality_insufficient_to_overcome_systematic_exclusion).
narrative_ontology:cs_axiom_status(formal_equality_insufficient_to_overcome_systematic_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('a8285d6b-2020-45a7-b70b-07e1c23f5468', formal_equality_insufficient_to_overcome_systematic_exclusion, empirically_contingent).
narrative_ontology:cs_reference_frame('a8285d6b-2020-45a7-b70b-07e1c23f5468', affirmative_equality_correcting_exclusion).
narrative_ontology:cs_drift_state('a8285d6b-2020-45a7-b70b-07e1c23f5468', contemporary_formal_equality_contestation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a8285d6b-2020-45a7-b70b-07e1c23f5468', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(social_revolution_provisions__reservation_architecture, social_revolution_provisions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_revolution_provisions__reservation_architecture, scheduled_castes_entrants).
narrative_ontology:constraint_beneficiary(social_revolution_provisions__reservation_architecture, scheduled_tribes_entrants).
narrative_ontology:constraint_beneficiary(social_revolution_provisions__reservation_architecture, obc_entrants).
narrative_ontology:constraint_beneficiary(social_revolution_provisions__reservation_architecture, state_administrative_capacity).
narrative_ontology:constraint_victim(social_revolution_provisions__reservation_architecture, unreserved_competitors_at_margin).
narrative_ontology:constraint_victim(social_revolution_provisions__reservation_architecture, formal_equality_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNRESERVED COMPETITOR AT MARGIN (SNARE) — Faces absolute exclusion from a set proportion of seats through constitutional design. No formal appeal available; the reservation is written into the equality articles themselves, foreclosing contestation via formal equality doctrine. Trapped by meritocratic framing that denies they have any legitimate claim to the reserved seats.
constraint_indexing:constraint_classification(social_revolution_provisions__reservation_architecture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SC/ST/OBC BENEFICIARY ENTRANT (ROPE) — Experiences the reservation as pure coordination: it solves the collective action problem of overcoming structural exclusion through guaranteed access. The quota creates a coordination mechanism where individual merit alone cannot overcome institutional barriers. Net beneficiary experiencing the constraint as enabling rather than extractive.
constraint_indexing:constraint_classification(social_revolution_provisions__reservation_architecture, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ADMINISTRATIVE STATE / MERIT INSTITUTION (TANGLED ROPE) — Experiences mixed coordination and extraction. The reservation system solves a real coordination problem: meritocratic selection alone reproduces historical exclusion (genuine coordination function). Simultaneously, the state extracts legitimacy from the appearance of 'merit' while systematically narrowing the merit pool (asymmetric enforcement burden — must identify qualified SC/ST/OBC candidates while maintaining performance narratives). Active enforcement required; moderate experienced extraction from the institutional perspective.
constraint_indexing:constraint_classification(social_revolution_provisions__reservation_architecture, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DOMINANT-CASTE ORGANIZED COALITION (TANGLED ROPE) — Coordinates around the formal equality frame to contest reservations while maintaining appearance of supporting 'merit.' Experiences extraction: reservations reduce their guaranteed access. Experiences coordination benefit: the constitutional embedding of equality language provides legitimacy cover for their formal-equality objections. Suppressed alternatives: caste-census data, structural unemployment rates, wealth distribution by caste. High suppression but medium extractiveness because the coalition retains partial agency.
constraint_indexing:constraint_classification(social_revolution_provisions__reservation_architecture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMAL EQUALITY NATURAL LAW (MOUNTAIN) — From a civilizational perspective rooted in formal equality doctrine, reservations appear to violate a fundamental principle: equal treatment without regard to status. This perspective reads formal equality as immutable, derived from natural law principles of human dignity. However, the constraint's textual design (written into the equality articles themselves as their fulfillment) forecloses this reading from within the constitutional framework. The formal-equality objection is suppressed not by external coercion but by the doctrinal design itself.
constraint_indexing:constraint_classification(social_revolution_provisions__reservation_architecture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: CREAMY LAYER / ADVANCEMENT COALITION (SCAFFOLD) — As SC/ST/OBC groups advance and accumulate advantage, the reservation system's sunset logic activates: the 'creamy layer' exclusion and periodic review mechanisms are designed to phase out reservations as the original exclusion ceases to operate. This perspective sees the system as temporary coordination with a built-in exit logic. Low effective extraction because the coalition anticipates and participates in the constraint's own termination.
constraint_indexing:constraint_classification(social_revolution_provisions__reservation_architecture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_revolution_provisions__reservation_architecture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_revolution_provisions__reservation_architecture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_revolution_provisions__reservation_architecture, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(social_revolution_provisions__reservation_architecture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The reservation system has a genuine coordination function — it solves the collective action problem that meritocracy alone reproduces structural exclusion. However, it also extracts from unreserved competitors at the margin and suppresses formal-equality objections. The moderate value reflects that extraction is real (competitors lose guaranteed access to a percentage of seats) but bounded (the percentage is constitutionally fixed, and the system contains exit mechanisms like the creamy layer). The measurement trajectory (0.22 → 0.35 → 0.38) shows modest accumulation as contestation intensifies and mandate-creep pressures emerge, suggesting the system may be shifting toward higher extractiveness over generational timescales. Suppression (0.52): Moderate-high. The embedding of reservations into the equality articles itself suppresses certain formal-equality objections by making them appear doctrinally incoherent within the constitutional frame. However, suppression is not total — dominant-caste coalitions mount formal-equality challenges through litigation, and underground contestation persists. Suppression includes structural barriers (data on caste-wise wealth distribution, employment, and intergenerational mobility are often withheld or contested), creating epistemic suppression in addition to doctrinal suppression. Theater ratio (0.38): Moderate. The reservation system has substantial real function (it does enable access for historically excluded groups) but also performative elements (merit narratives that obscure how meritocracy reproduces exclusion; creamy-layer discourse that implies the problem is solved when only a small segment advances). Lower theater than many state institutions reflects that the functional coordination is genuine, not decorative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence from a single structural data set. The beneficiary (SC/ST/OBC entrant) experiences pure coordination (Rope) — the quota solves their access problem. The unreserved competitor experiences pure extraction (Snare) — they face absolute exclusion with no legitimate appeal path. The administrative state experiences mixed coordination and extraction (Tangled Rope) — it must genuinely overcome barriers while managing merit narratives. The dominant-caste coalition experiences constrained extraction (Tangled Rope) — they are suppressed but retain organized agency. The creamy-layer perspective experiences temporary scaffolding with a sunset (Scaffold) — the system is designed to phase out as exclusion ceases. The formal-equality doctrinalist risks seeing immutable principle (false-summit Mountain) — naturalizing a contingent constitutional design choice. This perspectival range (five genuine types, one false summit) arises from the textual design: by embedding reservations into the equality articles, the constitution makes the constraint invisible from certain readings (formal equality) while making it hypervisible from others (the excluded competitor). The gap between natural-law and tangled-rope readings reveals the false summit signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are determined by each agent's structural position within the constraint. SC/ST/OBC beneficiaries experience low d (they are net recipients of the quota mechanism) → low f(d) → low or negative effective extraction chi. Unreserved competitors experience high d (they bear the cost of exclusion) → high f(d) → high effective extraction chi. The administrative state experiences moderate d (mixed beneficiary and victim — both coordinates access and suppresses alternatives) → moderate f(d) → moderate chi. The dominant-caste coalition experiences moderate-high d (they are partially suppressed) → moderate-high f(d) → moderate chi. The formal-equality reader occupies an analytical position (d ≈ 0.72, canonical analytical fallback) → chi scales to moderate. The perspective gap is driven by these different directionality positions mapping to the same base extractiveness (0.38) but producing different experienced extractiveness values through f(d) multiplication.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing its genuine coordination function (enabling SC/ST/OBC access) from its extraction mechanism (suppressing formal-equality objections and excluding unreserved competitors). The tangled-rope classification is the fulcrum: the system cannot be reduced to pure coordination (Rope) because real asymmetric extraction occurs; nor can it be reduced to pure extraction (Snare) because real coordination function exists. The mandatrophy is resolved by clarifying that BOTH functions are structural, not opposed. The system coordinates around overcoming exclusion AND extracts from unreserved competitors AND suppresses formal-equality doctrine — these are simultaneous, not contradictory. The perspective method reveals why mandatrophy exists: the beneficiary sees only coordination, the competitor sees only extraction, the administrative state sees both. The analytical observer's risk is naturalizing formal equality as the immutable principle (Mountain false summit), which would dissolve mandatrophy by declaring the competitor's extraction perspective invalid. The constraint's resolution requires holding both simultaneously: genuine coordination (justified) and genuine extraction (acknowledged).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quota_offset_versus_extraction,
    'Does the reservation quota offset the extractiveness of historic exclusion, or does it constitute a new extraction mechanism disguised as remedy?',
    'Longitudinal wealth distribution data by caste; intergenerational mobility rates comparing SC/ST/OBC entrants vs baseline majority populations; measurement of whether quotas enable access-to-opportunity or merely concentrate SC/ST/OBC advancement within a narrow band while structural exclusion persists at margins.',
    'If offset: classification is Tangled Rope (genuine coordination + remedial asymmetry). If new extraction: classification trends toward Snare (the quota creates new institutional beneficiaries — bureaucrats, elite SC/ST/OBC entrants — while majority of SC/ST/OBC populations remain excluded).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quota_offset_versus_extraction, empirical, 'Whether quotas offset historic exclusion or constitute new extraction').

omega_variable(
    textual_design_suppression_mechanism,
    'Does embedding reservations into the equality articles themselves actually suppress formal-equality objections, or does this doctrinal move generate deeper contestation by appearing to privilege identity over individual merit?',
    'Analysis of constitutional litigation trends; mapping of how formal-equality objections evolve across jurisdictions that use different doctrinal framings (embedded vs statutory vs administrative); examination of whether suppression is structural (objections cannot be mounted) or performative (objections are mounted but lack legitimacy within the framework).',
    'If suppression is structural: mountain perspective''s classification is false summit (formal equality truly foreclosed). If suppression is performative: the constraint contains suppressed alternatives that sustain contestation; formal-equality doctrine retains underground vitality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_design_suppression_mechanism, conceptual, 'Whether textual embedding of reservations suppresses formal-equality objections structurally or performatively').

omega_variable(
    creamy_layer_exit_logic_operationality,
    'Is the creamy layer exclusion (excluding advanced SC/ST/OBC groups from quota benefits) a genuine sunset clause, or is it a fictional exit mechanism that preserves the constraint indefinitely by redefining advancement as continued exclusion eligibility?',
    'Historical analysis of creamy layer applications and outcomes; examination of whether the creamy layer threshold tracks actual advancement-to-parity or maintains a fixed exclusion window; comparison of advancement rates with/without creamy layer enforcement.',
    'If genuine sunset: Scaffold classification confirmed — the system contains its own termination logic. If fictional: the constraint is Piton (appears to have sunset but persists through redefinition) or Snare (the exit mechanism is performative while extraction continues).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creamy_layer_exit_logic_operationality, empirical, 'Whether creamy layer exclusion operates as genuine sunset or fictional exit mechanism').

omega_variable(
    kernel_reading_contestation,
    'What is the precise structural relationship between THIS reading (reservation_architecture as equality''s fulfillment) and the sibling reading (personal_law_compromise as the counterreform stopping at family law)?',
    'Doctrinal analysis of the constitutional text: does the equality-article embedding of reservations logically foreclose the personal-law compromise, or do they coexist as different domains of the social revolution? Historical examination: did constitutional framers intend reservations to be the primary mechanism and personal-law compromise a secondary concession, or vice versa?',
    'If forecloses: the two readings are locked in logical opposition within a single constitutional framework (rare). If coexists_with: both persist as live contestations across different political coalitions. If influences: reservation-architecture shapes the field within which personal-law compromise operates (e.g., equal protection pressures toward uniform civil code).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Structural relationship between reservation_architecture and personal_law_compromise readings of the social revolution kernel').

omega_variable(
    mandate_creep_and_quota_proliferation,
    'Does the reservation architecture create institutional incentive for quota proliferation and mandate creep (expanding categories, increasing percentages) that transforms the mechanism from remedy into extraction framework?',
    'Time-series analysis of reservation percentage expansion; tracking of category expansion (SC/ST/OBC splitting); examination of whether new quota demands emerge predictably in response to each prior round of quota implementation.',
    'If creep detected: extractiveness is underestimated; the constraint is evolving toward Snare as institutional actors (state, dominant-caste coalitions using quotas as negotiation currency) extract value from quota allocation itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_creep_and_quota_proliferation, empirical, 'Whether reservation architecture exhibits mandate creep and quota proliferation dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_revolution_provisions__reservation_architecture, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(srvp_res_theater_1950, social_revolution_provisions__reservation_architecture, theater_ratio, 0, 0.25).
narrative_ontology:measurement(srvp_res_theater_2000, social_revolution_provisions__reservation_architecture, theater_ratio, 50, 0.35).
narrative_ontology:measurement(srvp_res_theater_2025, social_revolution_provisions__reservation_architecture, theater_ratio, 75, 0.38).

% Extraction over time
narrative_ontology:measurement(srvp_res_extractiveness_1950, social_revolution_provisions__reservation_architecture, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(srvp_res_extractiveness_2000, social_revolution_provisions__reservation_architecture, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(srvp_res_extractiveness_2025, social_revolution_provisions__reservation_architecture, base_extractiveness, 75, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(srvp_res_suppression_1950, social_revolution_provisions__reservation_architecture, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(srvp_res_suppression_2000, social_revolution_provisions__reservation_architecture, suppression_requirement, 50, 0.52).
narrative_ontology:measurement(srvp_res_suppression_2025, social_revolution_provisions__reservation_architecture, suppression_requirement, 75, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_revolution_provisions__reservation_architecture, resource_allocation).
narrative_ontology:affects_constraint(social_revolution_provisions__reservation_architecture, social_revolution_provisions__personal_law_compromise).
narrative_ontology:affects_constraint(social_revolution_provisions__reservation_architecture, social_revolution_provisions__untouchability_abolition_article_17).
narrative_ontology:affects_constraint(social_revolution_provisions__reservation_architecture, creamy_layer_exclusion_mechanism).
narrative_ontology:affects_constraint(social_revolution_provisions__reservation_architecture, formal_equality_doctrine_indian_constitution).

% DUAL FORMULATION NOTE:
% The reservation architecture is one reading of the social revolution kernel. The sibling readings (personal-law compromise, untouchability abolition) are separate constraints with different ε values and different beneficiary/victim structures. Personal-law compromise exhibits lower extractiveness (ε ≈ 0.28, Rope) because it preserves community autonomy; untouchability abolition exhibits lower extractiveness (ε ≈ 0.12, Mountain-to-Rope transition) because it is categorical prohibition rather than quota. The three stories are linked via network.affects_constraints to show how different mechanisms of the social revolution interact: the reservation-architecture reading may have only succeeded because the personal-law compromise secured community consent, and both may depend on the untouchability prohibition removing the most extreme status barrier.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(social_revolution_provisions__reservation_architecture, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
