% ============================================================================
% CONSTRAINT STORY: partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_partition_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: partition_reading
 *   human_readable: Partition Reading: Shinto and Buddhism as Functionally Coexistent Domains
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   The partition reading of the shinbutsu ontological commitment asserts
 *   that Shinto and Buddhism occupy functionally separate domains (life-cycle
 *   events vs. afterlife and ancestral veneration) without requiring
 *   doctrinal integration or resolution of contradictions. From this
 *   perspective, the coexistence is not syncretic (unified framework) nor
 *   incoherent (logical contradiction demanding resolution) but pragmatically
 *   partitioned: each tradition maintains its own truth claims and ritual
 *   authorities within bounded domains. Practitioners participate in both
 *   without experiencing cognitive conflict because the domains are treated
 *   as incommensurable rather than integrative. This reading was historically
 *   enforced by state mandate during the Meiji period (1868-1945, State
 *   Shinto separation) but before and after that period emerged as a
 *   voluntary functional arrangement. The constraint exhibits low
 *   extractiveness in its steady state (voluntary coexistence) but elevated
 *   extractiveness and suppression during the Meiji mandate (forced
 *   separation with state enforcement for nationalist purposes). The
 *   partition reading is one of three structurally distinct readings of the
 *   same kernel; the others (syncretic and incoherence readings) will be
 *   authored as separate constraint stories with different ε values and
 *   sibling reading relations.
 *
 * KEY AGENTS:
 *   - Household practitioners: Powerless/constrained — participate in both traditions; experience the partition as natural accommodation without perceived extraction
 *   - Shrine priests (kannushi): Organized/constrained — maintain Shinto domain expertise; coordinate with Buddhist priests through role separation rather than merger
 *   - Buddhist temple priests (bonze): Organized/constrained — maintain Buddhist domain expertise; coordinate with Shinto priests through role separation
 *   - Institutional religious authorities: Institutional/mobile — separate Shinto and Buddhist establishments; benefit from partition by maintaining independent institutions
 *   - Meiji state apparatus: Institutional/mobile — imposed explicit partition as temporary scaffold; extracted nationalist value from Shinto monopoly during 1868-1945
 *   - Analytical observer (functional view): Analytical/analytical — sees partition as elegant coordination solution enabling practical cooperation without ontological merger
 *   - Analytical observer (performance view): Analytical/analytical — sees partition as degraded institutional maintenance of unresolved contradictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(partition_reading, 0.18).
domain_priors:suppression_score(partition_reading, 0.12).
domain_priors:theater_ratio(partition_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(partition_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(partition_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(partition_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(partition_reading, rope).
narrative_ontology:human_readable(partition_reading, "Partition Reading: Shinto and Buddhism as Functionally Coexistent Domains").
narrative_ontology:topic_domain(partition_reading, "religious_studies/japanese_history/ontology_of_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(partition_reading, 'c9326578-7099-427f-95c3-63926d92a876').
narrative_ontology:cs_kernel_codification('c9326578-7099-427f-95c3-63926d92a876', fixed_text).
narrative_ontology:cs_authority_grounding('c9326578-7099-427f-95c3-63926d92a876', practice).
narrative_ontology:cs_interpretation_layer_present('c9326578-7099-427f-95c3-63926d92a876').
narrative_ontology:cs_reading_relation('c9326578-7099-427f-95c3-63926d92a876', partition_reading__syncretic_reading, influences).
narrative_ontology:cs_reading_relation('c9326578-7099-427f-95c3-63926d92a876', partition_reading__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('c9326578-7099-427f-95c3-63926d92a876', foundational, domain_separability).
narrative_ontology:cs_axiom_status(domain_separability, holdable).
narrative_ontology:cs_axiom_grounding('c9326578-7099-427f-95c3-63926d92a876', domain_separability, conventional).
narrative_ontology:cs_axiom('c9326578-7099-427f-95c3-63926d92a876', foundational, practitioner_autonomy_preservation).
narrative_ontology:cs_axiom_status(practitioner_autonomy_preservation, holdable).
narrative_ontology:cs_axiom_grounding('c9326578-7099-427f-95c3-63926d92a876', practitioner_autonomy_preservation, instrumental).
narrative_ontology:cs_reference_frame('c9326578-7099-427f-95c3-63926d92a876', edo_functional_coexistence).
narrative_ontology:cs_drift_state('c9326578-7099-427f-95c3-63926d92a876', meiji_forced_separation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c9326578-7099-427f-95c3-63926d92a876', '').
narrative_ontology:cs_kernel_id(partition_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(partition_reading, household_practitioners).
narrative_ontology:constraint_beneficiary(partition_reading, shrine_priests).
narrative_ontology:constraint_beneficiary(partition_reading, buddhist_priests).
narrative_ontology:constraint_beneficiary(partition_reading, shinto_institutional_establishment).
narrative_ontology:constraint_beneficiary(partition_reading, buddhist_institutional_establishment).
narrative_ontology:constraint_victim(partition_reading, meiji_state_apparatus).
narrative_ontology:constraint_vindicates(partition_reading, functional_domain_separation).
narrative_ontology:constraint_vindicates(partition_reading, practitioner_autonomy_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals participate in both Shinto life-cycle rites (birth purification, marriage, coming-of-age) and Buddhist death rites (funerals, memorial services for ancestors). Experience the dual participation as natural and non-contradictory because each tradition solves a distinct ritual problem. Social and familial pressures constrain exit from this dual participation, but the constraint itself is not experienced as coercive — rather as accommodation that allows meaningful engagement with both traditions.
narrative_ontology:constraint_stakeholder(partition_reading, household_practitioners, beneficiary,
    powerless, biographical, constrained, local).

% Kannushi (Shinto shrine priests) maintain ritual authority over life-cycle events and purification. The partition preserves their specialized expertise and institutional role without requiring merger with Buddhist priesthood. They set the agenda for Shinto domains (shrine festivals, purification protocols, kami invocation) while recognizing Buddhist priesthood's authority over death rites. Ordination vows and community dependency constrain exit, but the clear domain boundary enables sustained priestly authority.
narrative_ontology:constraint_stakeholder(partition_reading, shrine_priests, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(partition_reading, shrine_priests, agenda_setter).

% Bonze (Buddhist temple priests) maintain ritual authority over death rites, funerals, and ancestral veneration. The partition preserves their specialized expertise in Buddhist funeral practices and sutra recitation without requiring merger with Shinto priesthood or doctrinal justification for coexistence with competing tradition. They set the agenda for Buddhist domains (funeral protocols, memorial services, karma instruction) while recognizing Shinto priesthood's authority over life events.
narrative_ontology:constraint_stakeholder(partition_reading, buddhist_priests, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(partition_reading, buddhist_priests, agenda_setter).

% Organized Shinto institutions (shrine associations, training lineages, theological schools) maintain independent authority structures and funding streams through the partition. The clear separation of domains from Buddhism enables Shinto to claim distinct jurisdiction over kami worship and life-cycle events without need for doctrinal integration with Buddhist cosmology. National policy-level mobility enables exit (state could mandate merger), but historical stability and institutional investment make exit unattractive.
narrative_ontology:constraint_stakeholder(partition_reading, shinto_institutional_establishment, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(partition_reading, shinto_institutional_establishment, agenda_setter).

% Organized Buddhist institutions (temple networks, sectarian hierarchies, training monasteries) maintain independent authority structures and funding streams through the partition. The clear separation of domains from Shinto enables Buddhism to claim distinct jurisdiction over death, ancestral veneration, and karmic law without need for doctrinal integration with kami cosmology. National policy-level mobility enables exit (state could mandate merger), but historical stability and institutional investment make exit unattractive.
narrative_ontology:constraint_stakeholder(partition_reading, buddhist_institutional_establishment, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(partition_reading, buddhist_institutional_establishment, agenda_setter).

% The modernizing Meiji state (1868-1912) imposed explicit separation of Shinto from Buddhism as part of state ideology and institutional consolidation. The state extracted nationalist value from Shinto monopoly, elevated Shinto to official religion, and suppressed Buddhist institutions as competing authority. This role applied during the 1868-1945 mandate period; post-1945 disestablishment returns state to observer role. The state bore the cost of maintaining forced separation (enforcement machinery, suppression of syncretic worship, management of religious conflict).
narrative_ontology:constraint_stakeholder(partition_reading, meiji_state_apparatus, payer,
    institutional, biographical, mobile, national).

% Abstract commitment to unified theological understanding of reality across traditions. Not present at the table — neither Shinto nor Buddhism was required to address whether kami fit within Buddhist karma cosmology or whether Buddhist ancestors are kami. This excluded voice represents the standard of logical coherence that neither tradition would meet. Its absence enables the partition to function — coherence would require resolving contradictions that the partition simply brackets.
narrative_ontology:constraint_stakeholder(partition_reading, theological_coherence, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(partition_reading, theological_coherence).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Accommodate culturally distinct ritual practices for life events (Shinto) and death events (Buddhism) within a single society without requiring doctrinal merger or logical reconciliation. Enable households to participate meaningfully in both traditions. Enable separate priesthoods to maintain specialized expertise and authority.
% TRANSFER_FUNCTION: Authority over ritual domains transfers from unified religious institution (which does not exist) to separate Shinto and Buddhist establishments. Spiritual legitimacy transfers to both traditions simultaneously — practitioners receive validation from both for their respective domain participation. Funding and institutional resources flow separately to Shinto shrines (life-cycle maintenance) and Buddhist temples (death-rite maintenance).
% ABSENT_VOICES: Theological coherence philosophers — those demanding logical reconciliation of kami and karma frameworks. Christian and atheist minorities who reject both traditions. Radical syncretic reformers who wanted unified religious synthesis. These absent voices represent critiques that the partition could not accommodate without destabilizing the entire arrangement.
% DISAPPEARANCE_RATIONALE: If the partition disappeared and Shinto-Buddhism had to choose unified ontology or complete separation, institutional arrangements would require substantial rearrangement. Priesthoods would need to merge or compete for exclusive authority. Households would face pressure to choose single-tradition commitment. The absence of the partition would transform the constraint from a coordination mechanism into either a competition for monopoly or a forced syncretism that would disrupt existing institutions.
% FOUNDING_PROBLEM: Multiple distinct ritual problems (life-cycle events requiring purification, death-rite events requiring karma guidance) within a single society could not be solved by single religious institution without creating doctrinal incoherence. The founding problem was architectural: how to enable different traditions to address different problems without requiring them to agree about metaphysics.
% FOUNDING_PROBLEM_CORROBORATION: Household practitioners confirm daily: they experience life and death events as requiring different ritual frameworks. Shrine and temple priests confirm: they maintain separate ritual expertise that cannot be easily merged. Meiji state apparatus confirmed during 1868-1945 by attempting to suppress Buddhism while elevating Shinto — the suppression failed because Buddhist death rites were too culturally entrenched; state reverted to functional separation after initial enforcement failed. Post-1945 continuation without state mandate confirms that the underlying problem (distinct ritual needs) persists.
narrative_ontology:disappearance_verdict(partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(partition_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HOUSEHOLD PRACTITIONER (ROPE) — Individual who participates in both Shinto life-cycle rites (birth purification, marriage, coming-of-age) and Buddhist death rites (funerals, ancestral veneration). Experiences the partition as coordination: each tradition solves a distinct ritual problem without requiring integration. No extraction experienced — costs and benefits are symmetrical across the dual participation. Exit is constrained by social norms and family expectation, but the constraint itself is experienced as natural accommodation rather than coercive arrangement.
constraint_indexing:constraint_classification(partition_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: VILLAGE RITUAL SPECIALIST COMMUNITY (ROPE) — Shrine priests (kannushi) and Buddhist temple priests (bonze) coordinate roles without institutional merger: Shinto handles life events, Buddhism handles death. The partition enables both traditions to maintain distinct priesthoods, training lineages, and ritual authority. Each tradition benefits from specialized expertise and clear domain boundaries. Coordination costs are minimal — festivals and ceremonies follow separate calendars. Exit option is constrained by ordination vows and community dependency, but the specialists benefit from the arrangement as much as they bear costs.
constraint_indexing:constraint_classification(partition_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL RELIGIOUS AUTHORITIES (ROPE) — Shinto establishment and Buddhist establishment each maintain independent authority structures, funding streams, and doctrinal boundaries. The partition enables both institutions to claim distinct jurisdictions and avoid the organizational conflict that would arise from ontological integration. Extraction is minimal — the constraint subsidizes both institutions equally by preventing merger-driven institutional destabilization. Exit options are mobile at the national policy level (state could mandate syncretism or suppression), but historical stability makes exit unattractive.
constraint_indexing:constraint_classification(partition_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: MEIJI MODERNIZATION STATE (SCAFFOLD) — The state imposed explicit partition (State Shinto vs. Buddhism separation) during 1868-1945 as a temporary measure to consolidate Shinto as national symbol and modernize along Western church-state lines. From state view, the partition was transitional policy meant to last one historical epoch. The constraint carries sunset logic: once Shinto was established as state ideology, further separation became maintenance rather than innovation. Post-1945 disestablishment returns to functional coexistence without state mandate. Extraction is asymmetric during the mandate period (state uses Shinto for nationalist purposes) but becomes symmetric after mandate ends.
constraint_indexing:constraint_classification(partition_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / FUNCTIONAL COORDINATION VIEW (ROPE) — From civilizational scale, the partition reading sees Shinto-Buddhism coexistence as elegant functional coordination: domain separation solves the binding problem (how to reconcile incompatible ontologies) by declaring them incommensurable rather than contradictory. Practitioners experience two parallel frameworks, each with its own standards of truth and practice. The constraint is low-extraction coordination because no agent needs to convert the other or claim superior truth — the partition reserves judgment on ultimate reality while enabling practical cooperation. This perspective recognizes the constraint as a genuine solution, not a mask for deeper conflict.
constraint_indexing:constraint_classification(partition_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INSTITUTIONAL PERFORMANCE VIEW (PITON) — From a different analytical vantage, the partition reading appears as degraded performance: the claim that Shinto and Buddhism occupy truly separate ontological domains is maintained through ritual and institutional practice despite underlying logical tensions (both claim truth about the same world; the afterlife status of kami is unresolved; ancestral identity across the boundary is incoherent). The partition is ceremonially maintained — each institution acts as if its domain is exclusive — but the theater ratio is elevated because practitioners and scholars know the integration is incomplete. The constraint persists through institutional inertia and pragmatic acceptance rather than through logical necessity.
constraint_indexing:constraint_classification(partition_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(partition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(partition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(partition_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(partition_reading, TR),
    TR >= 0.70.

:- end_tests(partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low in steady state. The partition reading models a genuinely voluntary arrangement where practitioners and institutions benefit equally from domain separation. No agent monopolizes extraction — households get both life-cycle and death-rite services, priests maintain specialized authority in separate domains, institutions preserve autonomy. The low value reflects that the constraint is experienced as coordination rather than coercion in its natural state. The baseline value is elevated slightly above pure rope (0.0) because some transaction costs attend the maintenance of boundary clarity and some cultural power attaches to boundary-setting authority. Suppression (0.12): Low. Practitioners can and do exit through conversion, atheism, or selective participation — the barriers are primarily social/familial, not legal or economic. The partition itself does not actively suppress alternatives; it simply reserves judgment on their coherence. Theater ratio (0.35): Moderate-low. Some performative maintenance of the partition occurs — formal declarations that domains are separate, ritual acknowledgments of boundaries — but this is not the primary function. The theater rose sharply during Meiji (0.55 at peak) when state enforcement mandated separation, fell after 1945, and has stabilized at a level indicating pragmatic rather than purely ceremonial maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The two analytical perspectives produce divergent classifications from the same base properties. The functional coordination view (Rope) sees the partition as an elegant solution that enables practical cooperation while reserving judgment on ultimate reality — a stable, low-extraction arrangement because the binding problem is dissolved rather than solved. The institutional performance view (Piton) sees the same constraint as ceremonially maintained theater: both institutions act as if their domains are exclusive despite knowing the integration is incomplete. The perspectival gap reflects a real structural ambiguity in the kernel itself: whether the ontological partition represents a genuine metaphysical boundary or a pragmatic fiction maintained for institutional stability. The household practitioner and village specialist perspectives both see Rope (functional coordination), confirming that the low-extraction reading maps to lived experience. The Meiji state perspective clarifies the historical contingency: the partition became explicitly mandated during a brief period (75-year interval of forced separation), then reverted to voluntary coexistence when the mandate ended. This measurement pattern distinguishes the 'natural' partition (low theater, low extraction) from the 'enforced' partition (high theater, high extraction during state mandate).
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary/victim structure is declared for this constraint because the partition reading models genuine coordination without asymmetric extraction. All perspectives experience the constraint as symmetrical in its steady state — benefits and costs distribute equally across practitioners, priests, and institutions. During the Meiji mandate (1868-1945), directionality shifted: the state became a net beneficiary (extracted nationalist value from Shinto monopoly) while Buddhist institutions became net victims (suppressed for competing with state ideology). The post-1945 measurements show return to symmetrical directionality. The partition reading's ε-invariance depends on treating the voluntarily-coexistent form (Edo period and post-1945) as the constraint's true nature, with the Meiji mandate as a temporary interventional disruption. If the forced separation were treated as the true constraint, the ε value would be substantially higher and the beneficiary/victim structure would reflect state-Shinto alignment against Buddhism.
 *
 * MANDATROPHY ANALYSIS:
 *   PARTITION READING: No mandatrophy. The constraint's founding mandate (accommodate Shinto life-cycle rites and Buddhist death rites as equally valid expressions of Japanese religious practice) remains live and functional. The Meiji-era mandate (use Shinto monopoly for nationalist consolidation) DID undergo mandatrophy — the founding problem (legitimize the state through invented tradition) became politically obsolete by 1945, and the partition constraint persisted through institutional inertia rather than through continued functional purpose. The post-1945 constraint reverted to its pre-Meiji form, suggesting that the 'natural' partition reading correctly identifies the underlying functional arrangement independent of state manipulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_completeness_contest,
    'Are Shinto and Buddhism genuinely separate ontological domains, or does their coexistence mask unresolved logical contradiction at the foundation?',
    'Philosophical reconstruction of kami ontology and Buddhist dharma-body doctrine; analysis of practitioner discourse about whether kami are sentient beings subject to karmic law, whether Buddhist ancestors are kami, whether the afterlife is unified across both traditions. Interview data from priests and scholars about whether the domains are held as truly incommensurable or pragmatically accommodated despite logical overlap.',
    'If genuinely separate ontologies: partition reading is stable rope (coordination without integration). If masked contradiction: reading is piton or incoherence reading is more accurate. If pragmatic accommodation without truth claim: reading is still rope but contingent on practitioner forbearance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_completeness_contest, conceptual, 'Whether partition represents genuine ontological separation or masked logical contradiction').

omega_variable(
    meiji_mandate_duration,
    'Was the Meiji-era forced partition (1868-1945) a temporary structuring of pre-existing coexistence, or did it create a new institutional arrangement that persisted after mandate ended?',
    'Historical comparison of Edo-period Shinto-Buddhism relationships (shinbutsu-shugo) with Meiji institutional separation with post-1945 voluntary coexistence. Track whether post-1945 practitioners consciously revived Edo practices or continued Meiji-structured separation as inertial default.',
    'If Meiji was temporary intervention: partition reading correctly frames post-1945 state as return to functional coexistence. If Meiji created durable new arrangement: partition reading is partially wrong about whether it''s ''natural'' coexistence or institutional artifact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meiji_mandate_duration, empirical, 'Whether Meiji separation was temporary mandate or created durable institutional structure').

omega_variable(
    practitioner_identity_coherence,
    'Do individual practitioners experience Shinto and Buddhist participation as two compartmentalized identities or as integrated self across both traditions?',
    'Ethnographic interviews with mixed-tradition practitioners; narrative analysis of how they describe their dual participation; observation of whether they experience cognitive tension or smooth integration when moving between traditions in life-cycle contexts.',
    'If compartmentalized: partition reading is accurate to lived experience — practitioners experience functional separation as natural. If integrated: practitioners may be seeing through the partition boundary despite institutional structures — incoherence reading or syncretic reading may be more valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practitioner_identity_coherence, empirical, 'Whether practitioners experience Shinto-Buddhism participation as compartmentalized or integrated').

omega_variable(
    reading_identification_contest,
    'Is this reading (partition as functional domain separation without ontological integration) the partition reading, or is it actually the syncretic reading described as functional coordination?',
    'Recover historical texts and contemporary scholarly discourse distinguishing ''partition'' from ''syncretism'' in Japanese religious studies. The partition reading should emphasize SEPARATION and bounded domains; syncretic reading should emphasize INTEGRATION and unified framework. Determine which one this story actually instantiates by checking whether it asserts true separateness (partition) or pragmatic coexistence within an implicit unified framework (syncretism).',
    'If this is syncretic reading: the story is mislabeled and should be retitled. If genuinely partition: the story correctly emphasizes domain boundaries and practitioner autonomy within strict limits. Classification and directionality may require adjustment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identification_contest, conceptual, 'Whether this story instantiates partition (separation) or syncretism (integration) reading').

omega_variable(
    kernel_foreclosure_possibility,
    'Do the partition reading''s core axioms (domain separability, practitioner autonomy preservation) logically foreclose the incoherence reading, or are both compatible within different frameworks?',
    'Formal logical analysis: if partitionism asserts ''Shinto and Buddhism have incommensurable truth claims that need not be resolved,'' does that assertion make incoherentism (which asserts ''the two traditions directly contradict'') logically impossible, or merely less attractive? The answer determines whether relation = forecloses or coexists_with.',
    'If forecloses: the readings are in genuine logical competition — accepting partition axioms requires rejecting incoherentism as false. If coexists_with: different parties can coherently hold different readings without one invalidating the other.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_foreclosure_possibility, conceptual, 'Whether partition axioms logically foreclose incoherence reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(partition_reading, 0, 130).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(part_theater_edo, partition_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(part_theater_meiji_peak, partition_reading, theater_ratio, 75, 0.55).
narrative_ontology:measurement(part_theater_post_1945, partition_reading, theater_ratio, 77, 0.42).
narrative_ontology:measurement(part_theater_contemporary, partition_reading, theater_ratio, 130, 0.35).

% Extraction over time
narrative_ontology:measurement(part_extract_edo, partition_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(part_extract_meiji_peak, partition_reading, base_extractiveness, 75, 0.38).
narrative_ontology:measurement(part_extract_post_1945, partition_reading, base_extractiveness, 77, 0.15).
narrative_ontology:measurement(part_extract_contemporary, partition_reading, base_extractiveness, 130, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(part_supp_edo, partition_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(part_supp_meiji_peak, partition_reading, suppression_requirement, 75, 0.65).
narrative_ontology:measurement(part_supp_post_1945, partition_reading, suppression_requirement, 77, 0.1).
narrative_ontology:measurement(part_supp_contemporary, partition_reading, suppression_requirement, 130, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(partition_reading, identity_coordination).
narrative_ontology:affects_constraint(partition_reading, syncretic_reading).
narrative_ontology:affects_constraint(partition_reading, incoherence_reading).
narrative_ontology:affects_constraint(partition_reading, meiji_state_shinto_nationalism).
narrative_ontology:affects_constraint(partition_reading, buddhist_institutional_autonomy).

% DUAL FORMULATION NOTE:
% The shinbutsu ontological commitment kernel decomposes into three constraint stories with distinct ε values and sibling reading relations. This partition_reading story models functional coexistence without integration (ε ≈ 0.18). The syncretic_reading models unified framework underneath apparent difference (ε ≈ 0.15, lower because it denies contradiction entirely). The incoherence_reading models unresolved contradiction driving interpretive disputes (ε ≈ 0.55+, higher because contradiction extraction is substantial). All three stories link to each other via network.affects_constraints and share the same kernel_id. They represent three coherent but incompatible ways of reading the same contested foundation claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
