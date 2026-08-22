% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__hybrid_cascade_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: imposition_pathway_kernel__hybrid_cascade_reading
 *   human_readable: Meiji Calendar Reform as State-Manufactured Fringe Cascading to Organic Adoption
 *   domain: historical_sociology/state_formation
 *
 * SUMMARY:
 *   In 1873 the Meiji government decreed adoption of the Gregorian calendar,
 *   initially binding only government employees and military personnel for
 *   payroll and administrative purposes. Over the following decades, calendar
 *   use spread beyond this mandated population into urban commercial and
 *   social life, while rural agricultural communities retained lunisolar
 *   reckoning far longer. This reading treats the mandated population as a
 *   state-manufactured fringe: the decree did not achieve universal adoption
 *   directly, but created a population whose visible, prestige-linked
 *   compliance became the vector for organic climb into the wider society.
 *   The suppression trajectory documents the state's own read of this:
 *   enforcement need falls steadily once the fringe's example begins to be
 *   imitated, distinguishing this reading empirically from exogenous_override
 *   (which predicts constant enforcement) and endogenous_climb (which
 *   predicts no discontinuity at the decree point).
 *
 * KEY AGENTS:
 *   - meiji_state_administrators: agenda_setter (institutional/analytical) — issues and administers the decree
 *   - government_and_military_personnel: payer/beneficiary (moderate/trapped) — the manufactured fringe
 *   - modernizing_elites: beneficiary (powerful/arbitrage) — voluntary early climbers who accelerate diffusion
 *   - rural_agricultural_communities: payer (powerless/constrained) — last-reached population
 *   - traditional_calendar_practitioners: excluded (powerless/trapped) — displaced expertise with no voice in the decision
 *   - conscripted_low_rank_soldiers: payer (powerless/trapped) — embody the friction seam between fringe and majority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__hybrid_cascade_reading, 0.45).
domain_priors:suppression_score(imposition_pathway_kernel__hybrid_cascade_reading, 0.62).
domain_priors:theater_ratio(imposition_pathway_kernel__hybrid_cascade_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__hybrid_cascade_reading, "Meiji Calendar Reform as State-Manufactured Fringe Cascading to Organic Adoption").
narrative_ontology:topic_domain(imposition_pathway_kernel__hybrid_cascade_reading, "historical_sociology/state_formation").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__hybrid_cascade_reading, '102f0e11-6b90-41fb-94e3-0004b450b8f9').
narrative_ontology:cs_kernel_codification('102f0e11-6b90-41fb-94e3-0004b450b8f9', formalized).
narrative_ontology:cs_authority_grounding('102f0e11-6b90-41fb-94e3-0004b450b8f9', extraction).
narrative_ontology:cs_interpretation_layer_present('102f0e11-6b90-41fb-94e3-0004b450b8f9').
narrative_ontology:cs_reading_relation('102f0e11-6b90-41fb-94e3-0004b450b8f9', imposition_pathway_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('102f0e11-6b90-41fb-94e3-0004b450b8f9', imposition_pathway_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('102f0e11-6b90-41fb-94e3-0004b450b8f9', foundational, override_and_climb_are_sequential_not_alternative_mechanisms).
narrative_ontology:cs_axiom_status(override_and_climb_are_sequential_not_alternative_mechanisms, holdable).
narrative_ontology:cs_axiom_grounding('102f0e11-6b90-41fb-94e3-0004b450b8f9', override_and_climb_are_sequential_not_alternative_mechanisms, empirically_contingent).
narrative_ontology:cs_axiom('102f0e11-6b90-41fb-94e3-0004b450b8f9', secondary, manufactured_fringe_is_structurally_distinct_from_organic_fringe).
narrative_ontology:cs_axiom_status(manufactured_fringe_is_structurally_distinct_from_organic_fringe, holdable).
narrative_ontology:cs_axiom_grounding('102f0e11-6b90-41fb-94e3-0004b450b8f9', manufactured_fringe_is_structurally_distinct_from_organic_fringe, conventional).
narrative_ontology:cs_reference_frame('102f0e11-6b90-41fb-94e3-0004b450b8f9', pre_decree_lunisolar_administrative_equilibrium).
narrative_ontology:cs_drift_state('102f0e11-6b90-41fb-94e3-0004b450b8f9', post_meiji_urbanization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('102f0e11-6b90-41fb-94e3-0004b450b8f9', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, meiji_state_administrators).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, modernizing_elites).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, international_trade_partners).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, rural_agricultural_communities).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, traditional_calendar_practitioners).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, conscripted_low_rank_soldiers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, government_and_military_personnel).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, government_and_military_personnel).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__hybrid_cascade_reading, state_capacity_seeds_organic_diffusion).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__hybrid_cascade_reading, compressed_climb_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue the 1873 decree mandating the Gregorian calendar for government employees and military personnel, converting an internal administrative problem (payroll, salary calculation under the old lunisolar system) into a forced adoption event. They administer the fringe directly through payroll, military drill schedules, and civil service documentation, and monitor whether adoption spreads beyond the mandated population.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, meiji_state_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Required by decree to use the new calendar for salary, duty rosters, and official records regardless of personal or community practice. Bear the cost of dual bookkeeping with family and village life still running lunisolar, but gain career advancement, urban social standing, and international-facing prestige from visible compliance. This population is the artificial fringe the decree manufactures.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, government_and_military_personnel, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__hybrid_cascade_reading, government_and_military_personnel, beneficiary).

% Merchants, journalists, and urban professionals who adopt the new calendar early to signal alignment with the modernizing state and to transact more easily with treaty-port foreigners. They benefit from the state-manufactured fringe by joining it voluntarily once it exists, converting an imposed population into a visible, prestigious climb vector others imitate.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, modernizing_elites, beneficiary,
    powerful, generational, arbitrage, national).

% Foreign consulates, shipping firms, and treaty powers gain a synchronized calendar for scheduling, contracts, and diplomatic correspondence with Japan without having to accommodate the lunisolar system. They exert no direct enforcement but their expectations partly motivated the decree and their continued reliance reinforces persistence of the new calendar among the fringe.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, international_trade_partners, beneficiary,
    institutional, generational, arbitrage, global).

% Continue lunisolar timing for planting, festivals, and ritual life for decades after the decree, since agricultural cycles do not bend to administrative convenience. Bear friction costs when dealing with government offices, taxes, and conscription schedules now run on the new calendar, and are the last population the organic climb reaches, if it reaches them at all within the interval.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, rural_agricultural_communities, payer,
    powerless, generational, constrained, regional).

% Calendar-makers, temple almanac publishers, and ritual specialists whose professional and cosmological knowledge is built on the lunisolar system. They are not consulted in the decree and have no seat in the administrative process that displaces their function; their objections surface in almanac-publishing disputes but do not alter the state's course.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, traditional_calendar_practitioners, excluded,
    powerless, generational, trapped, regional).

% Drafted from rural areas into a military apparatus that runs entirely on the new calendar for drill, pay, and leave, while their home communities remain lunisolar. They personally embody the seam between the artificial fringe and the unconverted majority, absorbing the friction cost of translating between the two systems on every leave and remittance.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, conscripted_low_rank_soldiers, payer,
    powerless, immediate, trapped, national).

% Study the Meiji calendar reform as a test case for whether commitment displacement follows a pure top-down override model, a pure organic climb model, or a hybrid in which imposition manufactures the fringe that then climbs. Their classification choice determines which M-set cell the case populates.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__hybrid_cascade_reading, meiji_state_administrators).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes government payroll, military scheduling, and international commerce onto a single dateline, solving a genuine coordination problem between an administratively complex lunisolar intercalation system and the demands of a centralizing bureaucratic state engaging a Gregorian-calendar world.
% TRANSFER_FUNCTION: Moves administrative friction and cultural cost from the state and its international partners onto the conscripted and mandated population (government employees, military personnel), who must absorb the burden of running two calendar systems in parallel until the new one climbs into general use; late-adopting rural communities bear residual friction indefinitely.
% ABSENT_VOICES: Traditional calendar-makers and ritual specialists whose professional livelihood depended on lunisolar reckoning were not part of the 1873 decision process; their objections appear only in almanac-publishing disputes, not in the administrative record that justified the reform.
% DISAPPEARANCE_RATIONALE: If the mandated fringe adoption had never occurred, proponents of the hybrid reading argue Japan's calendar unification would have taken substantially longer or fractured along administrative/rural lines indefinitely (world_rearranges from the state's vantage); proponents of the endogenous_climb sibling would argue the same convergence was already underway through trade contact and would have completed on a similar timescale without the decree (world_unchanged). This story's own reading holds that removing the imposed fringe removes the specific compression mechanism, so within THIS reading's own terms the world rearranges — but the dispute over the counterfactual timeline is real and unresolved.
% FOUNDING_PROBLEM: The Meiji state needed a calendar that supported modern payroll calculation (the lunisolar system requires costly intercalary-month adjustments to salary schedules) and that aligned Japan's official time-reckoning with the Gregorian-calendar states it was negotiating treaties and trade with.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary Meiji finance ministry records (cited in later economic-history scholarship) attest the immediate payroll problem was resolved within a few years of the decree; the international-alignment problem is corroborated by continued diplomatic and trade practice from outside the Japanese state itself. No corroboration exists from rural agricultural communities or calendar-making guilds, whose own records show the lunisolar system persisted in daily and ritual use for decades after the state's stated problem was solved — the persistence of the mandate and its social prestige effects outlived the founding administrative rationale.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__hybrid_cascade_reading, contested).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__hybrid_cascade_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_pathway_kernel__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 0.45, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).
:- end_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.55) reflecting the real cost imposed on the mandated fringe population at the decree's outset, and declines to 0.45 as adoption becomes normalized and the cost of dual-calendar operation diminishes for society as a whole. Theater ratio starts higher (0.4) because early compliance among government/military personnel included substantial performative adoption (public displays of modernization) alongside genuine administrative need, declining as the practice becomes functionally embedded rather than performed. Suppression_requirement is the metric that most directly distinguishes this reading from its siblings: it falls sharply and continuously (0.85 to 0.20) as the artificial fringe's organic climb takes over the diffusion work that enforcement initially had to do alone — this is the signature the hybrid_cascade_reading predicts and neither sibling reading would produce with this shape.
 *
 * PERSPECTIVAL GAP:
 *   From the state administrator's seat, this looks like a rope: a coordination problem solved by decisive action with declining enforcement need as the population climbs to justify it retroactively. From the rural community's seat, it looks tangled: an externally imposed cost with no coordination benefit visible to them for decades, borne while the state and elite population reap the synchronization gains. The engine's computed divergence between these seats is exactly the tangled_rope signature — genuine coordination function (which the state seat sees) coexisting with asymmetric extraction (which the rural seat bears) held together by real enforcement (visible in the initial suppression_requirement spike).
 *
 * DIRECTIONALITY LOGIC:
 *   Meiji state administrators sit at the clear beneficiary/agenda-setter end: they design and enforce the mandate and collect the coordination benefit (unified national administration, credible international standing). Government and military personnel are dual-positioned: they pay the immediate cost of compliance (trapped exit, no choice) but many also gain career and status benefits, which the secondary_role of beneficiary captures. Modernizing elites are pure beneficiaries with arbitrage-grade exit — they adopt voluntarily and can adjust posture as social norms shift, converting the imposed fringe into a diffusion engine. Rural communities and conscripted low-rank soldiers carry the highest effective directionality toward extraction: constrained or trapped exit, powerless position, and generational time horizon meaning the cost of non-synchronization compounds across their lifetimes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (payroll intercalation cost, international administrative synchronization) was substantially resolved within the first decade, corroborated by finance-ministry and diplomatic records external to the beneficiary population. Yet the mandate's social and administrative apparatus persisted and its prestige effects continued reshaping calendar practice for decades after the narrow administrative rationale was satisfied — this is the founding_problem_status: dead pattern, distinguishing the state's justification (solving an administrative problem) from the mechanism's actual persistence (driving cultural displacement well past the point the stated problem required).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_evidence,
    'Does the empirical trajectory of the Meiji calendar case actually match the hybrid_cascade_reading''s predicted signature (discontinuity at decree + declining enforcement + genuine diffusion curve), or would the same historical record fit the endogenous_climb_reading (compressed invisible-fringe climb) or the exogenous_override_reading (state capacity alone, constant enforcement) equally well under different weighting of the archival evidence?',
    'Fine-grained archival reconstruction of calendar adoption rates by region, occupation, and time period, cross-referenced against enforcement records (fines, administrative penalties for non-compliance) to distinguish a genuine declining-enforcement/rising-organic-adoption curve from either a flat enforcement profile (favoring exogenous_override) or evidence that adoption outside government/military was already substantial before the 1873 decree (favoring endogenous_climb).',
    'If the enforcement curve is flat rather than declining, this reading''s central empirical claim collapses and the case reclassifies toward exogenous_override_reading. If pre-decree adoption evidence among traders and urban elites is substantial, the manufactured-fringe premise weakens and the case shifts toward endogenous_climb_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, empirical, 'Whether the historical record actually discriminates between the three kernel readings or is compatible with more than one.').

omega_variable(
    artificial_vs_organic_fringe_boundary,
    'Is the distinction between a ''state-manufactured'' fringe (government/military personnel required to adopt) and an ''organic'' fringe (early voluntary adopters who would have converged anyway) a real structural difference, or a framing choice imposed retrospectively by the M-set methodology?',
    'Comparative analysis against other calendar-reform cases (e.g., Soviet 1918 reform, Ottoman/Turkish 1926 reform) to see whether a manufactured-fringe-then-climb pattern is a distinguishable mechanism across cases or an artifact of how any top-down reform looks in retrospect once framed through the M-set climb vocabulary.',
    'If the artificial/organic fringe distinction does not hold up cross-case, the hybrid_cascade_reading itself may be under-determined as a separate mechanism from its siblings, which would push toward treating all three kernel readings as observationally equivalent framings rather than empirically distinct claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(artificial_vs_organic_fringe_boundary, conceptual, 'Whether the manufactured-fringe vs organic-fringe distinction is a real mechanism or a retrospective framing artifact.').

omega_variable(
    rural_climb_completion_status,
    'Did the organic climb this reading posits ever actually complete for rural agricultural communities, or does ritual/agricultural lunisolar practice persist in parallel to this day, meaning the ''climb completes'' half of the reading''s own title is only true for the urban/administrative population it was measured against?',
    'Ethnographic and calendrical survey of contemporary Japanese ritual and agricultural practice to determine whether dual-calendar operation (Gregorian for administration, lunisolar-derived for festivals and some agricultural timing) constitutes an incomplete climb or a stable equilibrium the M-set framework does not have a category for.',
    'If dual-calendar practice is a stable long-run equilibrium rather than an incomplete climb, the disappearance_verdict and founding_problem_status answers in this story would need revision — the mandate''s social work may never have been ''completed'' but rather permanently rendered partial, which is a different structural claim than either climb-completion or override-alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rural_climb_completion_status, empirical, 'Whether the organic climb this reading describes ever reached completion or produced a permanent dual-system equilibrium instead.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__hybrid_cascade_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(impo_tr_t10, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(impo_tr_t20, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(impo_tr_t30, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement(impo_tr_t45, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 45, 0.28).
narrative_ontology:measurement(impo_tr_t60, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(impo_be_t10, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(impo_be_t20, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(impo_be_t30, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 30, 0.46).
narrative_ontology:measurement(impo_be_t45, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 45, 0.45).
narrative_ontology:measurement(impo_be_t60, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 60, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(impo_su_t10, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(impo_su_t20, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(impo_su_t30, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(impo_su_t45, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 45, 0.28).
narrative_ontology:measurement(impo_su_t60, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 60, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__hybrid_cascade_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of imposition_pathway_kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle. endogenous_climb_reading treats all such cases as compressed invisible-fringe climbs (no true discontinuity); exogenous_override_reading treats state capacity as sufficient on its own with no organic climb mechanism required; this hybrid_cascade_reading claims both mechanisms operate sequentially — override manufactures the fringe, then organic climb completes the diffusion. All three should be evaluated against the same underlying Meiji calendar case; the reading that best fits the archival enforcement-decline and adoption-rate evidence (see omega kernel_reading_selection_evidence) is the empirically preferred one, but all three remain authored as live, structurally distinct constraints rather than collapsed into one averaged ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
