% ============================================================================
% CONSTRAINT STORY: meiji_separation_shock
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_meiji_separation_shock, []).

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
 *   constraint_id: meiji_separation_shock
 *   human_readable: Meiji Shinbutsu Bunri (Kami-Buddha Separation Edict)
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   The Meiji shinbutsu bunri (kami-buddha separation) edict of 1868 mandated
 *   institutional and ritual separation of Shinto shrines and Buddhist
 *   temples, ending over a millennium of syncretic practice (shinbutsu-shugo)
 *   where kami and buddhas were worshipped together in shared sacred spaces.
 *   The edict was part of the Meiji state's modernization program and served
 *   to create State Shinto as an ideological apparatus legitimating imperial
 *   authority. The constraint demonstrates commitment-system collapse
 *   dynamics: a stable kernel (centuries of syncretic practice) subjected to
 *   violent reinterpretation by a new authority structure claiming the
 *   syncretism was never coherent. The critical analytical question is
 *   whether shinbutsu-shugo was a genuine ontological commitment (kami and
 *   buddhas as non-separable aspects of unified reality) or pragmatic
 *   coexistence of distinct traditions. If the former, the Meiji edict
 *   destroyed a coherent alternative religious ontology and the mountain
 *   classification (ontological separability as natural law) is a false
 *   summit naturalizing Western religious taxonomy. If the latter, the edict
 *   formalized existing boundaries and some coordination function is real.
 *   The constraint's temporal trajectory shows initial extreme extraction and
 *   suppression (1868-1878: haibutsu kishaku persecution, property
 *   confiscation, forced conversions) gradually moderating as institutional
 *   boundaries normalized, then collapsing to piton status post-1945 when
 *   State Shinto was disestablished but bureaucratic separation persisted as
 *   inertial performance.
 *
 * KEY AGENTS:
 *   - Syncretic Practitioners: Primary victims (powerless/trapped) — rural villagers whose religious practice was criminalized overnight; cannot exit due to geographic and cognitive constraints
 *   - Buddhist Clergy: Secondary victims (moderate/constrained) — face property loss and career destruction but some adapt by converting to Shinto priesthood or maintaining urban temples under new boundaries
 *   - Meiji State Apparatus: Primary beneficiary (institutional/arbitrage) — captures ideological consolidation and imperial legitimation; controls separation timeline and enforcement scope
 *   - Kokugaku Ideologues: Secondary beneficiaries (organized/mobile) — nativist scholars who see separation as transitional infrastructure for restoring indigenous purity; interpret mandate as temporary with implicit sunset
 *   - Post-War Shrine Administration: Piton maintainer (institutional/constrained) — preserves separation boundaries through bureaucratic inertia after original function (State Shinto legitimation) collapsed in 1945
 *   - Analytical Observer: Risks naturalizing contingent taxonomy (analytical/analytical) — Western religious studies categories (discrete traditions, exclusive membership) imported as universal ontology when syncretic practice may have represented genuinely different cosmological commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meiji_separation_shock, 0.68).
domain_priors:suppression_score(meiji_separation_shock, 0.82).
domain_priors:theater_ratio(meiji_separation_shock, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meiji_separation_shock, extractiveness, 0.68).
narrative_ontology:constraint_metric(meiji_separation_shock, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(meiji_separation_shock, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(meiji_separation_shock, tangled_rope).
narrative_ontology:human_readable(meiji_separation_shock, "Meiji Shinbutsu Bunri (Kami-Buddha Separation Edict)").
narrative_ontology:topic_domain(meiji_separation_shock, "religious_studies/japanese_history/commitment_systems").

domain_priors:requires_active_enforcement(meiji_separation_shock).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(meiji_separation_shock, '40cde0b2-8887-4eed-b71c-8b0a0865b1f2').
narrative_ontology:cs_kernel_codification('40cde0b2-8887-4eed-b71c-8b0a0865b1f2', distributed).
narrative_ontology:cs_authority_grounding('40cde0b2-8887-4eed-b71c-8b0a0865b1f2', extraction).
narrative_ontology:cs_axiom('40cde0b2-8887-4eed-b71c-8b0a0865b1f2', foundational, kami_buddha_ontological_unity).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_unity, overridden).
narrative_ontology:cs_axiom_grounding('40cde0b2-8887-4eed-b71c-8b0a0865b1f2', kami_buddha_ontological_unity, conventional).
narrative_ontology:cs_axiom('40cde0b2-8887-4eed-b71c-8b0a0865b1f2', foundational, indigenous_purity_principle).
narrative_ontology:cs_axiom_status(indigenous_purity_principle, holdable).
narrative_ontology:cs_axiom_grounding('40cde0b2-8887-4eed-b71c-8b0a0865b1f2', indigenous_purity_principle, theological).
narrative_ontology:cs_reference_frame('40cde0b2-8887-4eed-b71c-8b0a0865b1f2', edo_syncretic_equilibrium).
narrative_ontology:cs_drift_state('40cde0b2-8887-4eed-b71c-8b0a0865b1f2', meiji_forced_separation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('40cde0b2-8887-4eed-b71c-8b0a0865b1f2', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(meiji_separation_shock, meiji_state_apparatus).
narrative_ontology:constraint_beneficiary(meiji_separation_shock, kokugaku_ideologues).
narrative_ontology:constraint_beneficiary(meiji_separation_shock, state_shinto_priesthood).
narrative_ontology:constraint_victim(meiji_separation_shock, syncretic_practitioners).
narrative_ontology:constraint_victim(meiji_separation_shock, buddhist_clergy).
narrative_ontology:constraint_victim(meiji_separation_shock, rural_temple_networks).
narrative_ontology:constraint_victim(meiji_separation_shock, lived_religious_practice).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(meiji_separation_shock, buddhist_clergy).
narrative_ontology:constraint_vindicates(meiji_separation_shock, shinto_buddhist_separability_doctrine).
narrative_ontology:constraint_vindicates(meiji_separation_shock, indigenous_purity_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rural villagers whose household altars mixed kami and buddha imagery for centuries suddenly face police inspection and economic sanctions for maintaining family practice. Cannot relocate (tied to agricultural land), cannot exit cognitive framework (religious identity constituted through syncretic practice across generations). Bear full cost of criminalized practice with no alternative.
narrative_ontology:constraint_stakeholder(meiji_separation_shock, syncretic_practitioners, payer,
    powerless, biographical, trapped, national).

% Temple priests face property confiscation during haibutsu kishaku persecution and forced choice between secularization or conversion to Shinto priesthood. Urban temples can survive by separating Buddhist practice from shrine administration; rural temples often destroyed. Mixed position — career damage and property loss offset by new institutional niches for those who successfully adapt to separation regime.
narrative_ontology:constraint_stakeholder(meiji_separation_shock, buddhist_clergy, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(meiji_separation_shock, buddhist_clergy, beneficiary).

% Central government pursuing rapid modernization after centuries of Tokugawa isolation. Sets the separation agenda through legislative edict and enforcement apparatus. Benefits from ideological consolidation — State Shinto legitimates imperial authority and distinguishes Japanese religious identity from foreign (Buddhist/Chinese) influence. Controls separation timeline and can modulate enforcement intensity based on political needs.
narrative_ontology:constraint_stakeholder(meiji_separation_shock, meiji_state_apparatus, agenda_setter,
    institutional, immediate, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(meiji_separation_shock, meiji_state_apparatus, beneficiary).

% Nativist scholars advocating return to indigenous kami-way free from Buddhist contamination. Organized intellectual movement with institutional positions (National Learning academies, government advisory roles). See separation as transitional infrastructure — once foreign elements removed, pure Shinto will emerge naturally and state enforcement can dissolve. Can exit if restoration project fails (maintain academic careers, shift to other nationalist projects).
narrative_ontology:constraint_stakeholder(meiji_separation_shock, kokugaku_ideologues, beneficiary,
    organized, generational, mobile, national).

% Newly professionalized shrine priests managing state-sponsored religious infrastructure. Benefit from bureaucratic positions, state salaries, and institutional authority that separation creates. Control access to shrine resources and ritual practice. Can arbitrage between religious authority and state administrative roles.
narrative_ontology:constraint_stakeholder(meiji_separation_shock, state_shinto_priesthood, beneficiary,
    institutional, biographical, arbitrage, national).

% Village temples that served as community centers, schools, and funeral sites for centuries face destruction during haibutsu kishaku. Physical infrastructure destroyed (buddha statues burned, temple buildings demolished or converted to shrines). Cannot relocate (rooted in specific communities), cannot rebuild (state prohibits reconstruction of syncretic sites). Intergenerational knowledge transmission severed.
narrative_ontology:constraint_stakeholder(meiji_separation_shock, rural_temple_networks, payer,
    powerless, generational, trapped, regional).

% The abstract good of continuous religious tradition spanning over a millennium. Excluded from Meiji policy discourse — no institutional advocate, no representation in government councils designing separation policy. The centuries-long tradition of simultaneous kami-buddha veneration had no seat at the table when the state decided that tradition was incoherent and must be dismantled. Not an agent (cannot collect rents), but its exclusion is structurally significant for understanding how the constraint operated.
narrative_ontology:constraint_stakeholder(meiji_separation_shock, lived_religious_practice, excluded,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(meiji_separation_shock, lived_religious_practice).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Meiji state needed to modernize religious administration as part of broader centralization and nation-building. Before 1868, religious authority was distributed across thousands of semi-autonomous temples and shrines with unclear jurisdictional boundaries. The separation created coherent institutional categories (Shinto shrines under state control, Buddhist temples under different regulatory regime) that enabled modern bureaucratic management, tax policy, and educational standardization. This coordination problem was real — the Tokugawa system could not scale to a centralized nation-state.
% TRANSFER_FUNCTION: The arrangement transfers ideological legitimation (from distributed religious authority to centralized imperial authority), institutional control (from autonomous temples to state-managed shrines), and property (confiscated temple lands and treasures flow to state or reassigned to Shinto priesthood). The transfer runs from syncretic practitioners and Buddhist clergy to the Meiji state apparatus and State Shinto priesthood. Money flows through temple property seizures; work flows through forced secularization and priestly conversions; legitimacy flows through narrative of restored indigenous purity.
% ABSENT_VOICES: The syncretic practitioners themselves — the millions of rural and urban Japanese whose lived religious practice mixed kami and buddha veneration across centuries — had no institutional representation in the policy formation. No peasant councils were consulted; no village assemblies voted on separation. The Meiji oligarchs and Kokugaku advisors who designed the edict treated the practice as incoherent contamination requiring purification, never considering the practitioners' own understanding of their tradition as coherent. The exclusion was not accidental — treating syncretic practice as legitimate would have undermined the indigenous purity narrative justifying the separation.
% DISAPPEARANCE_RATIONALE: If the Meiji separation edict disappeared overnight in 1868, the centuries-stable pattern of syncretic practice would have continued — temples and shrines would have maintained their institutional cohabitation, practitioners would have kept household altars with both traditions, and no violence would have been necessary to create State Shinto. The world clearly rearranged around this constraint: property was confiscated, buildings were destroyed, clergy were forced to convert, and an entire religious ontology was criminalized. The rearrangement extended for decades (haibutsu kishaku persecution through 1880s, State Shinto consolidation through 1945). Post-1945 disestablishment confirms the constraint was constructed rather than natural — when legal enforcement dissolved, syncretic practice partially re-emerged in household religion even as official bureaucratic boundaries persisted.
% FOUNDING_PROBLEM: The Meiji oligarchs faced a legitimacy crisis: how to justify overthrowing the Tokugawa shogunate and centralizing power under the emperor after centuries of distributed feudal authority. The solution was to construct Shinto as indigenous national religion distinguishing Japanese identity from foreign (Chinese/Buddhist) influence, with the emperor as living kami descended from sun goddess Amaterasu. This required dismantling the centuries-stable syncretic practice that treated kami and buddhas as interpenetrating rather than separate. The founding problem was political legitimation, not religious coordination — the separation served state-building, not the practitioners' needs.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (Meiji imperial legitimation via State Shinto) died with Japan's defeat in 1945. The post-war constitution's disestablishment clause explicitly rejected State Shinto and severed the emperor's divine status. Contemporary Japanese historians (Tamamuro Fumio, Helen Hardacre, John Breen) document how post-1945 shrine administration maintains separation boundaries as bureaucratic inertia rather than ideological necessity. The status is corroborated by sources outside the beneficiary set: religious studies scholars, constitutional historians, and post-war shrine priests themselves acknowledge the original function is obsolete even as they maintain the institutional boundaries. Survivor testimony from haibutsu kishaku victims (documented in Ketelaar's 'Of Heretics and Martyrs in Meiji Japan') confirms the violence was politically driven rather than arising from practitioner demand for institutional clarity.
narrative_ontology:disappearance_verdict(meiji_separation_shock, world_rearranges).
narrative_ontology:founding_problem_status(meiji_separation_shock, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SYNCRETIC PRACTITIONER (SNARE) — Rural villagers whose religious practice fused kami veneration and Buddhist rituals over centuries suddenly face state violence for maintaining family altars with both traditions. Cannot exit — geographic immobility, economic dependency on local temple networks, cognitive framework constituted through syncretic practice. The edict's coordination story (clarifying religious boundaries) is pure cover for state ideological extraction.
constraint_indexing:constraint_classification(meiji_separation_shock, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BUDDHIST CLERGY (TANGLED ROPE) — Temple priests face property confiscation (haibutsu kishaku) and forced secularization but also benefit from legal clarity that previously undefined institutional boundaries lacked. Constrained by state power and career risk, but some agency remains — urban temples survive by separating Buddhist practice from shrine administration; some priests convert to Shinto priesthood. Mixed extraction — career damage and property loss offset by new institutional niches for those who adapt.
constraint_indexing:constraint_classification(meiji_separation_shock, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MEIJI STATE APPARATUS (ROPE) — Benefits from ideological consolidation. The separation creates a state-managed Shinto infrastructure that legitimates imperial authority and distinguishes Japanese religious identity from Buddhist (foreign) influence. Experiences the constraint as coordination — solving the genuine institutional problem of modernizing religious administration while building ideological coherence. Net beneficiary with arbitrage-level exit — the state controls the separation timeline and scope.
constraint_indexing:constraint_classification(meiji_separation_shock, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: KOKUGAKU IDEOLOGUES (SCAFFOLD) — Nativist scholars (Motoori Norinaga lineage) who advocated Shinto purification see the edict as transitional infrastructure for restoring indigenous practice. The separation is justified by its temporariness — once foreign (Buddhist) contamination is removed, the pure kami-way will emerge naturally and the state machinery can dissolve. Sunset clause is implicit in the restoration ideology: the purified state does not need enforcement once achieved. Low effective extraction because the organized coalition sees an exit path (successful restoration).
constraint_indexing:constraint_classification(meiji_separation_shock, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: POST-WAR SHRINE ADMINISTRATION (PITON) — After 1945 disestablishment, the separation infrastructure persists through institutional inertia despite its original ideological function (State Shinto legitimation) having collapsed. Shrine-temple boundaries remain administratively enforced not because they solve a coordination problem but because the bureaucratic apparatus maintains the distinction theatrically. The separation ritual continues absent the mandate that created it — a degraded performance of categorical purity that rural practitioners ignore in practice while officially observing.
constraint_indexing:constraint_classification(meiji_separation_shock, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ONTOLOGICAL SEPARABILITY VIEW (MOUNTAIN) — From a civilizational/universal perspective rooted in Western religious studies categories, Shinto and Buddhism are ontologically distinct traditions with separate cosmologies, ritual practices, and institutional histories. The separation merely clarifies what was always structurally true but obscured by centuries of pragmatic cohabitation. This view treats religious categories as natural kinds with discoverable boundaries, seeing the Meiji edict as revealing pre-existing structure rather than constructing new arrangements. However, this is a false summit — the analytical perspective has imported Western religious taxonomy (discrete traditions with exclusive membership) and naturalized it as universal law, when the syncretic practice represented a genuinely different ontological commitment where kami and buddhas were not separate categories requiring reconciliation but interpenetrating aspects of a unified cosmological order.
constraint_indexing:constraint_classification(meiji_separation_shock, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meiji_separation_shock_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(meiji_separation_shock, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meiji_separation_shock, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(meiji_separation_shock, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(meiji_separation_shock, TR),
    TR >= 0.70.

:- end_tests(meiji_separation_shock_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): Substantial. The Meiji state captured ideological legitimation and institutional control through the separation, while syncretic practitioners bore the cost of practice criminalization and Buddhist clergy suffered property confiscation. However, extraction was not total — some coordination function exists (institutional clarity for modernizing state administration) and urban Buddhist temples survived by adapting. The value reflects that career asymmetry and property violence were real but not purely extractive — some of the 'extraction' funded genuine state modernization infrastructure. Suppression (0.82): High. Massive coercive enforcement through haibutsu kishaku persecution, police monitoring of household altars, economic sanctions against non-compliant temples, and geographic immobility preventing exit. Suppression was not total (0.90+) because urban practitioners had some adaptation options and the state eventually moderated violence after initial shock period. Theater ratio (0.45): Moderate. Initial enforcement (1868-1890s) was functionally real — state violence actually destroyed syncretic infrastructure and forced institutional separation. Theater increased over time as practitioners maintained covert syncretic practice while officially complying (omega variable: practiced vs mandated separation). Post-1945, separation became substantially theatrical — bureaucratic boundaries persist without the ideological function that justified them, but the current value (0.45) reflects that some institutional coordination function remains even in piton phase.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates commitment-system collapse dynamics across perspectives. The syncretic practitioner sees pure extraction (Snare) — practice criminalization with no coordination function, only ideological violence. The Buddhist clergy see mixed coordination and extraction (Tangled Rope) — institutional clarity offset by property confiscation and career damage. The Meiji state sees coordination (Rope) — solving genuine modernization problem of creating coherent religious administration while building imperial legitimacy. The Kokugaku ideologues see transitional infrastructure (Scaffold) — temporary enforcement necessary to restore indigenous purity, with implicit sunset when purification succeeds. The post-war shrine administration sees degraded performance (Piton) — separation boundaries maintained through bureaucratic inertia after original function collapsed. The analytical observer risks seeing natural law (Mountain) — ontological separability of Shinto and Buddhism as discoverable fact — but this is a false summit naturalizing Western religious taxonomy. The critical omega is whether shinbutsu-shugo was coherent kernel (genuine syncretism where kami-buddhas are unified) or pragmatic coexistence. If coherent kernel, the Meiji edict destroyed an alternative religious ontology and the mountain classification is definitively false. If pragmatic coexistence, some coordination function is real and the tangled rope classification becomes more defensible.
 *
 * DIRECTIONALITY LOGIC:
 *   The Meiji state apparatus is the clear primary beneficiary — captures imperial legitimation, ideological consolidation, and administrative control. Engine derives low d (beneficiary with arbitrage exit) producing low or negative effective extraction. Syncretic practitioners are primary victims — their religious practice was criminalized and they had no exit option (geographic immobility, cognitive framework constituted through syncretic practice). Engine derives high d (victim with trapped exit) producing maximum effective extraction. Buddhist clergy are secondary victims with mixed position — face property loss and career damage but also benefit from institutional clarity and some adaptation options. Engine derives moderate d (victim with constrained exit but some agency) producing moderate effective extraction. Kokugaku ideologues are secondary beneficiaries who see the constraint as transitional — organized coalition with mobile exit options (can leave if restoration fails) and clear sunset logic. Engine derives low d (beneficiary with mobile exit and scaffold framing) producing low effective extraction. Post-war shrine administration maintains piton status — the classification derives from theater gate rather than high effective extraction. The analytical observer's mountain classification is a false summit — naturalization of Western religious taxonomy as universal ontology when syncretic practice may represent a genuinely different ontological commitment.
 *
 * MANDATROPHY ANALYSIS:
 *   COMMITMENT-SYSTEM COLLAPSE EXEMPLAR: This constraint resolves mandatrophy by showing how a centuries-stable kernel can be violently reinterpreted by a new authority structure claiming the kernel was never coherent. The Meiji edict's coordination story (clarifying religious boundaries for modern administration) is partly real — some institutional clarity was achieved and the state genuinely needed to modernize religious administration. But the coordination is inseparable from ideological extraction — the separation created State Shinto as imperial legitimation apparatus and criminalized a lived religious practice that may have represented a genuinely different ontological commitment (non-Western religious taxonomy where kami and buddhas were not separate categories). The tangled rope classification at the analytical level reflects this irreducible entanglement: coordination function and extractive violence are structurally fused, not separable layers. The false summit detector identifies the mountain perspective as naturalization — treating religious categories as natural kinds with discoverable boundaries when the boundaries themselves were constructed through state violence. The scaffold perspective (Kokugaku ideologues) shows how extractive constraints can embed sunset logic that never arrives — the 'temporary' purification became permanent institutional infrastructure. The piton perspective shows how mandates outlive their functions — post-1945 separation persists as bureaucratic theater after State Shinto collapsed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_coherence_question,
    'Was shinbutsu-shugo a single coherent ontological commitment (genuine syncretism where kami and buddhas are non-separable aspects of one reality) or a pragmatic coexistence of two distinct traditions?',
    'Historical analysis of pre-Meiji ritual texts, temple-shrine architecture, and practitioner testimony. If syncretic practice shows internal logical consistency and non-contradictory cosmology, it is a coherent kernel. If practice shows domain partitioning (kami for this-world benefits, buddhas for afterlife) or strategic ambiguity, it is coexistence masquerading as fusion.',
    'If coherent kernel: the Meiji edict destroyed a genuine alternative religious ontology, and the mountain classification is definitively false summit. If pragmatic coexistence: the edict formalized existing boundaries, and some coordination function (institutional clarity) is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_coherence_question, conceptual, 'Whether shinbutsu-shugo was coherent syncretism or pragmatic coexistence').

omega_variable(
    haibutsu_kishaku_necessity,
    'Was the violent Buddhist persecution (haibutsu kishaku) structurally necessary to achieve institutional separation, or was it extractive excess layered onto a coordination function?',
    'Comparative analysis of religious disestablishment in other modernizing states (Ottoman millet system dissolution, European secularization). Identification of separation pathways that achieved institutional clarity without property destruction or forced conversion.',
    'If necessary: tangled_rope classification holds — genuine coordination required coercive enforcement. If excess: the coordination story is cover for ideological extraction, and snare classification applies more broadly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(haibutsu_kishaku_necessity, empirical, 'Whether violent persecution was necessary for institutional separation').

omega_variable(
    practiced_vs_mandated_separation,
    'Did practitioners actually separate kami and buddha veneration in their lived religious practice after the edict, or did they maintain syncretic practice covertly while observing official boundaries?',
    'Ethnographic analysis of Meiji-era and early 20th century household religious practice; archaeological evidence from family altars; oral histories from rural communities. Measurement of gap between official compliance and actual behavior.',
    'If separation was practiced: the edict achieved its coordination function and institutional boundaries became internalized. If syncretic practice persisted covertly: the edict''s function was purely performative (theater), and the piton classification applies earlier than post-1945.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practiced_vs_mandated_separation, empirical, 'Whether lived practice separated or remained syncretic').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel shinbutsu-shugo itself (the syncretic practice tradition) or the Meiji state''s interpretive claim about what that practice ''really'' was (separable traditions requiring purification)?',
    'If the kernel is the practice tradition, then readings are: (1) coherent syncretism reading (kami-buddhas are unified), (2) pragmatic coexistence reading (separate but harmonious), (3) corrupted purity reading (foreign contamination of indigenous way). If the kernel is the Meiji interpretive doctrine, then readings are variants of purification ideology with different violence levels.',
    'Different framings produce different classification outcomes. Practice-as-kernel makes the Meiji edict an external shock destroying the kernel. Doctrine-as-kernel makes the edict an internal reading of a state ideological commitment. The former makes this a commitment-system collapse; the latter makes it commitment-system enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether kernel is syncretic practice or state purification doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meiji_separation_shock, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meiji_sep_theater_1868, meiji_separation_shock, theater_ratio, 0, 0.15).
narrative_ontology:measurement(meiji_sep_theater_1878, meiji_separation_shock, theater_ratio, 10, 0.25).
narrative_ontology:measurement(meiji_sep_theater_1893, meiji_separation_shock, theater_ratio, 25, 0.35).
narrative_ontology:measurement(meiji_sep_theater_1913, meiji_separation_shock, theater_ratio, 45, 0.5).
narrative_ontology:measurement(meiji_sep_theater_1945, meiji_separation_shock, theater_ratio, 77, 0.7).
narrative_ontology:measurement(meiji_sep_theater_1968, meiji_separation_shock, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(meiji_sep_extract_1868, meiji_separation_shock, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(meiji_sep_extract_1878, meiji_separation_shock, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(meiji_sep_extract_1893, meiji_separation_shock, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(meiji_sep_extract_1913, meiji_separation_shock, base_extractiveness, 45, 0.68).
narrative_ontology:measurement(meiji_sep_extract_1945, meiji_separation_shock, base_extractiveness, 77, 0.55).
narrative_ontology:measurement(meiji_sep_extract_1968, meiji_separation_shock, base_extractiveness, 100, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(meiji_sep_suppress_1868, meiji_separation_shock, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(meiji_sep_suppress_1878, meiji_separation_shock, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(meiji_sep_suppress_1893, meiji_separation_shock, suppression_requirement, 25, 0.8).
narrative_ontology:measurement(meiji_sep_suppress_1913, meiji_separation_shock, suppression_requirement, 45, 0.75).
narrative_ontology:measurement(meiji_sep_suppress_1945, meiji_separation_shock, suppression_requirement, 77, 0.5).
narrative_ontology:measurement(meiji_sep_suppress_1968, meiji_separation_shock, suppression_requirement, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(meiji_separation_shock, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is part of a potential kernel family (shinbutsu-shugo as contested kernel) but the family decomposition is unresolved pending omega variable resolution. If shinbutsu-shugo was a coherent kernel, the family would include: (1) coherent syncretism reading (this story), (2) pragmatic coexistence reading (separate story: domain-partitioned practice with different ε), (3) corrupted purity reading (Kokugaku/State Shinto reading with different victim set and higher ε). If shinbutsu-shugo was pragmatic coexistence rather than coherent kernel, no family decomposition is warranted — this is the only constraint story needed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
