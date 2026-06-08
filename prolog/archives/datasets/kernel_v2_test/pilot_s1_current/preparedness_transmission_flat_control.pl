% ============================================================================
% CONSTRAINT STORY: preparedness_transmission_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission_flat_control, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_transmission_flat_control
 *   human_readable: Post-1953 Flood Preparedness as Transmitted Institutional Commitment
 *   domain: disaster_preparedness/institutional_memory/governance
 *
 * SUMMARY:
 *   Post-1953 flood preparedness represents a commitment to transmit
 *   catastrophe-specific knowledge and institutional capacity across
 *   non-catastrophe generations. The 1953 North Sea flood killed over 1,800
 *   people in the Netherlands and triggered a comprehensive institutional
 *   response: the Delta Works dike systems, hydrological monitoring networks,
 *   emergency protocols, and governance structures specifically designed to
 *   prevent recurrence. Yet the transmission of this preparedness across
 *   non-catastrophe decades (now spanning 40+ years since the 1995
 *   Meuse/Rhine floods, and much longer since 1953) reveals a structural
 *   tension: the constraint persists as performative institutional continuity
 *   rather than as functionally verified protection. During calm periods, the
 *   apparatus continues through budget cycles, regulatory compliance, and
 *   ceremonial maintenance. When catastrophes do occur, they often reveal
 *   failure modes that were not adequately prepared for — indicating that
 *   inter-event transmission has decayed despite institutional continuity.
 *   The constraint exhibits piton characteristics: a former rope (genuine
 *   coordination of flood defense) that has atrophied into theatrical
 *   maintenance, yet the apparatus persists because catastrophe is rare
 *   enough that non-function cannot be decisively proven and because
 *   abandoning the apparatus is politically untenable. At-risk populations
 *   remain trapped by geography and economic dependency, bearing the cost of
 *   any transmission failure while having no exit option and no visibility
 *   into whether institutional activity actually protects them.
 *
 * KEY AGENTS:
 *   - At-Risk Populations: Primary victims (powerless/trapped) — live on floodplains due to economic necessity; cannot exit; depend entirely on preparedness they cannot verify
 *   - Flood Control Apparatus: Primary beneficiary (institutional/constrained) — maintains institutional existence, funding, and regulatory authority through preparedness mandate; budget cycles depend on ongoing operation
 *   - Technical Hydrological Community: Secondary actor (organized/constrained) — provides expertise and knowledge updating; constrained by institutional decision-making velocity and budget allocation; benefits from research funding and career advancement tied to preparedness
 *   - Property Developers and Commercial Interests: Secondary beneficiary (powerful/mobile) — benefit from dike infrastructure investment and regulatory calm; low experienced extraction because they can exit if conditions change
 *   - Political Leadership: Institutional actor (institutional/constrained) — performs preparedness commitment publicly; constrained by political costs of admitting preparedness limits; benefits from appearance of control
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing institutional arrangements as hydrological inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission_flat_control, 0.38).
domain_priors:suppression_score(preparedness_transmission_flat_control, 0.42).
domain_priors:theater_ratio(preparedness_transmission_flat_control, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission_flat_control, extractiveness, 0.38).
narrative_ontology:constraint_metric(preparedness_transmission_flat_control, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(preparedness_transmission_flat_control, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission_flat_control, piton).
narrative_ontology:human_readable(preparedness_transmission_flat_control, "Post-1953 Flood Preparedness as Transmitted Institutional Commitment").
narrative_ontology:topic_domain(preparedness_transmission_flat_control, "disaster_preparedness/institutional_memory/governance").

domain_priors:requires_active_enforcement(preparedness_transmission_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(preparedness_transmission_flat_control, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission_flat_control, flood_control_apparatus).
narrative_ontology:constraint_victim(preparedness_transmission_flat_control, at_risk_populations_between_events).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission_flat_control, flood_control_dike_corps).
narrative_ontology:constraint_beneficiary(preparedness_transmission_flat_control, hydrological_research_community).
narrative_ontology:constraint_beneficiary(preparedness_transmission_flat_control, property_developers_commercial_interests).
narrative_ontology:constraint_beneficiary(preparedness_transmission_flat_control, political_leadership_governance).
narrative_ontology:constraint_victim(preparedness_transmission_flat_control, at_risk_populations_floodplain).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live on floodplains due to economic dependency, job access, and family networks. Cannot afford to relocate. Depend entirely on dike systems and emergency protocols they cannot independently verify. Between catastrophes, their safety depends on institutional preparedness transmission they have no visibility into. If transmission fails and the next catastrophe exceeds dike capacity, they bear the full casualty cost.
narrative_ontology:constraint_stakeholder(preparedness_transmission_flat_control, at_risk_populations_floodplain, payer,
    powerless, biographical, trapped, national).

% Maintains dike systems, hydrological monitoring, emergency protocols, and governance structures. Sets the agenda for preparedness standards and regulatory compliance. Benefits from institutional funding that depends on the preparedness mandate. Constrained by political costs of admitting preparedness limits and by budgetary cycles that discourage radical protocol redesign during calm periods. Performs preparedness transmission through maintenance, training, and regulatory activity.
narrative_ontology:constraint_stakeholder(preparedness_transmission_flat_control, flood_control_dike_corps, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission_flat_control, flood_control_dike_corps, beneficiary).

% Generates flood forecasts, failure-mode analysis, and technical updates to preparedness protocols. Benefits from research funding and career advancement tied to dike-system management and hydrological science. Constrained by institutional decision-making velocity — identifies failure modes that are not rapidly translated into protocol updates or infrastructure changes due to budgetary and political constraints. Expertise is essential to transmission but cannot force implementation of updated protocols.
narrative_ontology:constraint_stakeholder(preparedness_transmission_flat_control, hydrological_research_community, beneficiary,
    organized, generational, constrained, national).

% Build and develop on protected floodplain land because dike infrastructure makes development economically feasible. Benefit from dike investment and flood insurance subsidies. Can exit by developing elsewhere, but the protected floodplain offers lower development costs and established infrastructure. Experience the preparedness apparatus as enabling their operations. Seek regulatory stability rather than costly preparedness upgrades.
narrative_ontology:constraint_stakeholder(preparedness_transmission_flat_control, property_developers_commercial_interests, beneficiary,
    powerful, biographical, mobile, national).

% Performs preparedness commitment publicly through legislation, budget allocation, and regulatory oversight. Benefits from appearance of control and protection. Constrained by political costs of admitting preparedness is limited or that the apparatus needs costly overhaul. Between catastrophes, political incentive is to maintain the appearance of adequacy rather than demand stress-testing. Participates in transmission through funding cycles and regulatory authority.
narrative_ontology:constraint_stakeholder(preparedness_transmission_flat_control, political_leadership_governance, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission_flat_control, political_leadership_governance, beneficiary).

% The rare-event catastrophe itself (extreme flood exceeding dike capacity) is not an agent but is the external forcing that disrupts the preparedness system and reveals failure modes. Catastrophes are absent from the inter-event period and cannot organize or advocate for preparedness changes. Yet the entire apparatus is framed as response to potential catastrophe — a non-agent that shapes institutional structure.
narrative_ontology:constraint_stakeholder(preparedness_transmission_flat_control, catastrophe_event, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(preparedness_transmission_flat_control, catastrophe_event).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintain dike systems, update hydrological forecasts, train emergency response personnel, and govern floodplain land use and infrastructure to reduce casualties from extreme flood events. Real coordination problem: disparate technical domains (hydraulics, civil engineering, meteorology, emergency management) and governmental levels must be integrated to function as a single protective system.
% TRANSFER_FUNCTION: The apparatus transfers institutional continuity and budgetary resources from society (via taxation and insurance) to the flood control agencies and their contractors, who maintain dike systems and implement protocols. It transfers visibility into flood risk away from at-risk populations (they cannot independently verify preparedness) and toward technical experts and governmental agencies. It transfers knowledge from catastrophe events (when the system fails and reveals gap modes) into protocol updates — but this transfer is incomplete and decays during non-catastrophe periods.
% ABSENT_VOICES: Future generations living on floodplains 40-60 years from now are excluded from present-day decisio ns about whether current preparedness is adequate. Populations that have relocated due to past flood risk (who know preparedness failed for them) are outside the current floodplain and not part of the policy conversation. Populations in other flood-prone regions (Bangladesh, Pakistan, Mozambique) whose catastrophe experience might inform preparedness design are excluded from the conversation in the Dutch context.
% DISAPPEARANCE_RATIONALE: If the post-1953 preparedness apparatus disappeared overnight — dikes unmaintained, protocols abandoned, monitoring ceased — the floodplain would rapidly reorganize: property values would collapse, population would relocate, insurance would become unaffordable, development would cease. The apparatus is not a neutral fact but a stabilizing structure that enables settlement and investment in flood-prone areas. Its disappearance would cause immediate economic and social reorganization. This establishes that arrangements depend on the constraint (it is not a natural law but a constructed system).
% FOUNDING_PROBLEM: The 1953 North Sea flood killed over 1,800 people in the Netherlands through dike failure and surge overtopping. The founding problem was specific: inadequate dike height and robustness against storm surge and river flooding, combined with lack of early warning systems and emergency evacuation protocols. The post-1953 response was to redesign dikes as engineered systems with defined safety standards, establish hydrological monitoring, and create governmental coordination mechanisms.
% FOUNDING_PROBLEM_CORROBORATION: The original founding problem (inadequate dike capacity in 1953) is attested by the 1953 event itself and by post-event investigations. However, the status of whether that specific problem still exists is contested. The technical hydrological community attests that current dikes meet design standards against floods with defined return periods (1/1250 year for most dikes, 1/10000 for critical areas). But climate change is increasing precipitation and sea-level rise, shifting the effective return period of design-basis floods. Some researchers (e.g., Deltares studies, climate adaptation literature) attest that the founding problem is partially re-emerging due to hydrological change. The apparatus and political leadership attest the problem is solved (current dikes are adequate). At-risk populations lack technical capacity to attest either way. The corroboration is multi-vocal and contradictory.
narrative_ontology:disappearance_verdict(preparedness_transmission_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission_flat_control, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AT-RISK POPULATIONS (SNARE) — Trapped by geography and economic dependency; cannot exit the floodplain. During non-catastrophe periods (the 30-40 year average return interval), preparedness transmission becomes invisible theater — the apparatus maintains protocols that do not protect against actual failure modes discovered in the last event. The population bears the full cost of any transmission failure (another catastrophe) with no recourse or alternative.
constraint_indexing:constraint_classification(preparedness_transmission_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FLOOD CONTROL APPARATUS (PITON) — The post-1953 institutional structure (dike corps, water boards, hydrological monitoring, emergency protocols) was built in response to catastrophic failure. During non-catastrophe periods (decades of calm), the apparatus performs preparedness transmission as maintenance ritual: annual inspections, regulatory compliance, protocol reviews, training exercises. These activities persist through institutional inertia and budget cycles, not because they demonstrably prevent the failure modes discovered in the last event. The apparatus continues partly because it cannot demonstrate failure (catastrophe is rare) and partly because institutional budgets depend on ongoing operation. Low functional content; high theatrical maintenance.
constraint_indexing:constraint_classification(preparedness_transmission_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECHNICAL HYDROLOGICAL COMMUNITY (TANGLED ROPE) — Genuinely solves the coordination problem of monitoring water systems, maintaining dike integrity, and updating flood models. Constrained by resource limits and institutional budget cycles. Benefits from research funding tied to preparedness frameworks and career advancement through dike-system management. Experiences mixed extraction and coordination: the institutional apparatus depends on their expertise, but institutional inertia prevents rapid protocol updates when new failure modes are identified. Active enforcement required to maintain the apparatus's claim that preparedness is technically adequate when the technical community knows failure modes exist.
constraint_indexing:constraint_classification(preparedness_transmission_flat_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PROPERTY DEVELOPERS (ROPE) — Benefit from dike infrastructure investment and flood insurance subsidies. Can exit (build elsewhere, relocate operations) but find the costs of exit higher than the costs of staying in the floodplain given the historical rarity of catastrophe. Preparedness transmission that maintains dike integrity and regulatory calm is coordination that enables their operations. Low experienced extraction — they are beneficiaries. Institutional constraints prevent them from demanding more aggressive preparedness (which would raise costs and insurance premiums), so they experience the apparatus as stable and adequate.
constraint_indexing:constraint_classification(preparedness_transmission_flat_control, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: POLITICAL LEADERSHIP (PITON) — Inherits the post-1953 institutional apparatus and performs preparedness commitment through regulation, funding cycles, and public statements. Between catastrophes, preparedness appears to be working — no evidence of failure accumulates, regulatory frameworks are in place, the apparatus is funded. The political incentive is to maintain the appearance of preparedness (which costs relatively little during calm periods) rather than to stress-test the apparatus or demand fundamental redesign (which would admit that current protocols are insufficient). Preparedness transmission becomes performance of institutional adequacy rather than actual verification of capacity.
constraint_indexing:constraint_classification(preparedness_transmission_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN CANDIDATE) — From a civilizational/universal perspective, extreme floods are rare, unpredictable events that human systems cannot fully prepare for. The gap between the last catastrophe and the next is inherently filled with uncertainty. No institutional apparatus can guarantee prevention. Preparedness is, from this view, an immutable natural limit: you can reduce risk but not eliminate it, and rare events always find failure modes. This perspective risks naturalizing what is actually a contestable institutional arrangement — treating the current preparedness apparatus as coextensive with what's possible rather than as one particular design choice vulnerable to atrophy.
constraint_indexing:constraint_classification(preparedness_transmission_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(preparedness_transmission_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(preparedness_transmission_flat_control, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(preparedness_transmission_flat_control, TR),
    TR >= 0.70.

:- end_tests(preparedness_transmission_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The flood control apparatus extracts institutional continuity and budgetary resources from the populations it is meant to protect, but the extraction is not as severe as pure snare mechanisms because some genuine protective function persists — dike systems do provide real risk reduction over the baseline. The extraction increases slightly over the 30-year interval (0.28→0.38) as the apparatus becomes more theatrical and less functionally updated. Theater ratio (0.68): High and rising. The apparatus performs preparedness through annual inspections, regulatory reviews, training exercises, and budget allocations, but these activities increasingly become theater rather than functional verification. The rise from 0.35 to 0.68 over 30 years reflects that inter-event institutional activity has become more decoupled from actual failure-mode updating — the apparatus looks increasingly like maintenance ritual. Suppression (0.42): Moderate and stable. At-risk populations face real barriers to exit (economic dependency, geography, political economy of land use) but suppression is not total — some mobility exists for wealthier populations. The suppression remains fairly stable over the interval because the structural barriers to exit do not change much.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence driven by time horizon and exit options. The at-risk population at biographical timescale sees a snare — they are trapped and cannot verify the preparedness apparatus actually protects them. The technical community at generational timescale sees tangled rope — genuine coordination problem (monitoring and maintaining dike systems) mixed with constrained career incentives that prevent rapid protocol updating. The apparatus itself sees piton — it knows its own processes have become increasingly theatrical, but institutional inertia prevents meaningful change during calm periods. The powerful commercial interests see rope — coordination that enables their operations with low extraction because they can exit. The political leadership performs rope publicly but experiences piton privately (knowing that admitting preparedness limits would trigger expensive institutional restructuring). The analytical observer at civilizational scale risks mountain — treating flood preparedness as an immutable natural problem rather than as a contingent institutional arrangement. The perspectival gap between snare (powerless/trapped), piton (institutional), and mountain (analytical) reveals that the same structural phenomenon appears as different types depending on whether you can leave, must maintain the institution, or view from outside.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is computed from the agent's structural position. At-risk populations are victims with no exit (trapped) → high d → maximum experienced extraction (they bear the cost of any transmission failure). The apparatus is a beneficiary with constrained exit (institutional inertia prevents abandonment) → low-to-moderate d → moderate extraction (the apparatus collects institutional resources but is somewhat locked into providing the services it claims). The technical community is a moderate beneficiary with constrained exit (career tied to preparedness frameworks) → low d → low-to-moderate extraction. Property developers are beneficiaries with mobile exit (can build elsewhere) → very low d → negative extraction (they experience the apparatus as providing net benefit). Political leadership is a beneficiary with constrained exit (admitting preparedness failure is costly) → low d → low extraction. The engine derives d automatically from these roles; the structural pattern is that trapped agents experience the constraint as snare while beneficiaries with arbitrage or mobile exit experience rope or low extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The original 1953-era mandate was to prevent casualties from future floods — an ambitious claim framed as achievable through comprehensive dike systems and hydrological science. Over 30+ years of non-catastrophe periods, this mandate has silently atrophied into something closer to 'maintain institutional continuity of preparedness frameworks' without explicit reframing or formal mandate change. The constraint exhibits classic mandatrophy: the original purpose (prevent catastrophe casualties) has become progressively decoupled from actual institutional activity (maintain protocols and budget cycles), yet the apparatus continues to claim the original mandate. The piton classification reflects this: the constraint is atrophied rope (genuine coordination of flood defense) maintained as theatrical performance because the rare-event nature of catastrophe prevents definitive proof of failure and because abandoning the apparatus is politically impossible. Mandatrophy is resolved not through explicit policy change but through the slow decoupling of stated mandate from institutional function during non-catastrophe periods. The apparatus knows it cannot meet the original mandate (prevent all casualties) but continues to perform as if it can. This is not unique to flood preparedness — many institutional commitments to rare-event prevention (nuclear safety, pandemic preparedness, asteroid defense) exhibit the same pattern: strong mandate clarity immediately after a catastrophe, gradual atrophy during calm periods, renewed panic when the next event arrives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    knowledge_decay_mechanism,
    'Is observed preparedness degradation during non-catastrophe periods driven by institutional forgetting, by deliberate rational under-investment given rare-event statistics, or by organizational inability to update protocol without triggering institutional restructuring?',
    'Longitudinal case study: comparison of post-event protocol updates (years 1-3 after a catastrophe) with protocol changes during calm periods (10+ years after an event). Examination of institutional archives and staff interviews about what causes protocol drift. Simulation of hypothetical resource constraints with and without catastrophe reminders.',
    'If forgetting: theater_ratio can be reduced through institutional memory mechanisms (documented protocols, mandatory training, cross-generational apprenticeship). If rational under-investment: the constraint is structural (rare events justify low investment) and theater is expected. If organizational stasis: the apparatus needs redesign (sunset clauses, mandatory review cycles, external audits). Different mechanisms imply different mandatrophy status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_decay_mechanism, empirical, 'Mechanism driving preparedness degradation between catastrophes').

omega_variable(
    transmission_success_measurement,
    'What counts as successful preparedness transmission across non-catastrophe generations? Is transmission measured by institutional continuity of protocol and funding, or by actual reduction in casualty rates conditional on catastrophe occurrence?',
    'Counterfactual comparison: examine casualty figures from successive catastrophes (1953 vs 1995 vs 2021) controlling for population exposure and meteorological severity. Did the apparatus improve its performance across events, or did institutional continuity of protocol coincide with unchanged or worsening casualty outcomes?',
    'If transmission success = institutional continuity: the piton classification is correct, theater is acceptable, and the mandate is performing as intended (maintaining the apparatus). If transmission success = casualty reduction: the apparatus is failing its mandate, and the piton classification obscures a deeper snare (populations are trapped believing in protection that has not materialized).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transmission_success_measurement, empirical, 'Definition of successful preparedness transmission').

omega_variable(
    catastrophe_interval_assumption,
    'Does the 30-40 year average return interval for major floods constitute a reliable temporal anchor for institutional planning, or is it itself a contingent artifact of recent climate patterns and upstream land-use change that could be disrupted without warning?',
    'Paleoclimatic and hydrological reconstruction: extend flood records beyond instrumental era (beyond 1850s) using sedimentary, archaeological, and dendrochronological evidence. Quantify historical variability in return intervals. Model climate-driven and land-use-driven changes to future flood statistics.',
    'If return interval is stable: the rare-event framing justifies low inter-event investment and the piton is appropriate. If return interval is degrading or highly variable: populations are trapped by a false assumption (the last event was not typical), and the snare classification becomes dominant even from powerful perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_interval_assumption, empirical, 'Reliability of catastrophe return interval assumptions').

omega_variable(
    institutional_learning_capacity,
    'When the hydrological or engineering community identifies new failure modes after a catastrophe (e.g., seepage, piping, wave overtopping at specific dike sections), does the institutional apparatus have decision-making velocity to update protocols and infrastructure before the next event, or is generational timescale deployment the structural reality?',
    'Institutional process audit: timeline from failure-mode identification (post-event reports, research publications) to protocol change (regulatory update, mandatory retrofit) to actual infrastructure deployment. Compare decision velocity in the post-1953 era to similar infrastructure-change decisions in other domains (aviation, nuclear). Examine whether political and budgetary cycles permit generational-timescale implementation.',
    'If apparatus can update within 5-10 years: transmission can meaningfully incorporate new knowledge and the apparatus is a genuine rope for technical community. If apparatus requires 20-40 years: transmission is largely ritualistic, and knowledge learned from one catastrophe may not be deployed by the time the next occurs. The constraint becomes a forced mismatch between knowledge timescale and action timescale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_learning_capacity, empirical, 'Institutional decision velocity for preparedness updates').

omega_variable(
    catastrophe_as_knowledge_reset,
    'Does each catastrophe constitute a partial knowledge reset — where institutional knowledge accumulated during the calm period is abruptly invalidated by discovering failure modes that were not anticipated — indicating that inter-event transmission is epistemically fragile rather than accumulative?',
    'Historiographic analysis: for each major flood event, identify the failure modes that surprised the apparatus (not anticipated by pre-event protocols or research). Compare the list of surprises from the 1953 flood to surprises from subsequent events. If each event produces genuinely novel failure modes (not incremental refinement of previously discovered ones), the transmission is epistemically fragile.',
    'If each catastrophe reveals new failure modes: the apparatus is not learning across events; transmission is performative. If failure modes are incremental refinements: the apparatus is making actual progress and transmission has some epistemic content. A fragile transmission mechanism justifies the piton classification despite institutional continuity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_as_knowledge_reset, empirical, 'Whether catastrophes reveal novel vs incremental failure modes').

omega_variable(
    mandatrophy_resolution_status,
    'Has the original 1953-era preparedness mandate (prevent casualties from future floods) been abandoned or transformed, and if so, when and by what explicit decision, or does the apparatus continue to claim the original mandate while knowing it cannot be met?',
    'Policy historiography: examine government statements, legislation, and regulatory frameworks from 1954 to present. Identify points where official mandate is restated, reframed, or explicitly narrowed (e.g., from ''prevent casualties'' to ''minimize exposure'' to ''enable rapid recovery''). Interview policy makers about the relationship between stated mandate and known preparedness limits. Look for unofficial acknowledgment (in technical reports or private communications) that the mandate is aspirational.',
    'If mandate has been explicitly changed: mandatrophy is resolved via formal process, and the piton classification reflects intentional atrophy. If mandate is unchanged but known-insufficient: the apparatus is engaged in performative commitment maintenance under false pretense — this is a snare for the populations depending on it. The classification difference is critical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_resolution_status, empirical, 'Resolution status of the original 1953 preparedness mandate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission_flat_control, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prepflatctl_tr_t0, preparedness_transmission_flat_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(prepflatctl_tr_t10, preparedness_transmission_flat_control, theater_ratio, 10, 0.52).
narrative_ontology:measurement(prepflatctl_tr_t20, preparedness_transmission_flat_control, theater_ratio, 20, 0.65).
narrative_ontology:measurement(prepflatctl_tr_t30, preparedness_transmission_flat_control, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(prepflatctl_be_t0, preparedness_transmission_flat_control, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(prepflatctl_be_t10, preparedness_transmission_flat_control, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(prepflatctl_be_t20, preparedness_transmission_flat_control, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(prepflatctl_be_t30, preparedness_transmission_flat_control, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(prepflatctl_su_t0, preparedness_transmission_flat_control, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(prepflatctl_su_t10, preparedness_transmission_flat_control, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(prepflatctl_su_t20, preparedness_transmission_flat_control, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(prepflatctl_su_t30, preparedness_transmission_flat_control, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission_flat_control, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission_flat_control, dike_construction_deferral).
narrative_ontology:affects_constraint(preparedness_transmission_flat_control, flood_insurance_pricing_opacity).

% DUAL FORMULATION NOTE:
% Preparedness transmission is decomposable into structural stories: (1) the apparatus's real protective function (rope-like) during and immediately after catastrophes, and (2) the apparatus's theatrical maintenance function (piton) during non-catastrophe periods. These have different ε values and different temporal profiles. This story models the long-interval steady state; a companion story would model the post-catastrophe crisis period.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
