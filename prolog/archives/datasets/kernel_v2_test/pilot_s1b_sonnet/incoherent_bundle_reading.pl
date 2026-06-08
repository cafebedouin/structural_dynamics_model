% ============================================================================
% CONSTRAINT STORY: incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_incoherent_bundle_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: incoherent_bundle_reading
 *   human_readable: Incoherent Bundle Reading of Shinbutsu Substrate
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   The incoherent bundle reading argues that Tokugawa-era shinbutsu fusion
 *   (神仏習合) was not a coherent synthesis but an accumulated bundle of
 *   contradictory religious obligations held together by state enforcement.
 *   The danka system (檀家制度) legally bound households to Buddhist temples for
 *   mortuary rites and census registration, while village ritual calendars
 *   required participation in Shinto agricultural observances. From this
 *   reading, practitioners experienced the fusion as unresolved doctrinal
 *   incoherence rather than as integrated worldview. The bakufu benefited
 *   from the system as a population surveillance mechanism (temple
 *   registration functioned as census and heterodoxy suppression). Temples
 *   benefited from guaranteed parishioners but sacrificed doctrinal autonomy.
 *   Village practitioners bore the cognitive burden of contradictory
 *   commitments with no resolution framework. This reading classifies the
 *   constraint as a snare from most perspectives: state-enforced extraction
 *   that suppresses alternatives (hidden Kirishitan, heterodox Pure Land,
 *   mountain ascetics) while providing no coherent interpretive kernel. The
 *   Meiji shinbutsu bunri (神仏分離) edicts attempted to separate Buddhism and
 *   Shinto, but fusion persisted in village practice through institutional
 *   inertia — the constraint degraded to piton (atrophied function maintained
 *   as performance) rather than dissolving. The key empirical question is
 *   whether practitioners experienced the fusion as resolved synthesis
 *   (supporting syncretic_fusion_reading) or as coerced incoherence
 *   (supporting this reading).
 *
 * KEY AGENTS:
 *   - Village Practitioners: Primary victims (powerless/identity_locked) — bear contradictory ritual obligations with no interpretive resolution; cannot exit danka system without losing civic status
 *   - Heterodox Communities: Secondary victims (moderate/constrained) — Kirishitan survivors, hidden Pure Land practitioners, mountain ascetics suppressed by enforced conformity; exit possible but costly
 *   - Tokugawa Bakufu: Primary beneficiary (institutional/arbitrage) — uses shinbutsu bundle as population surveillance and heterodoxy suppression; experiences pure coordination
 *   - Temple Administrative Hierarchy: Mixed beneficiary/victim (institutional/constrained) — benefits from guaranteed parishioners but bears doctrinal dilution; tangled rope perspective
 *   - Meiji Restorationists: Post-atrophy observers (powerful/mobile) — see degraded bundle persisting as piton after administrative function dissolved
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — classifies as snare based on structural extraction, suppression of alternatives, and lack of coherent kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(incoherent_bundle_reading, 0.58).
domain_priors:suppression_score(incoherent_bundle_reading, 0.68).
domain_priors:theater_ratio(incoherent_bundle_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(incoherent_bundle_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(incoherent_bundle_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(incoherent_bundle_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(incoherent_bundle_reading, snare).
narrative_ontology:human_readable(incoherent_bundle_reading, "Incoherent Bundle Reading of Shinbutsu Substrate").
narrative_ontology:topic_domain(incoherent_bundle_reading, "religious_studies/japanese_history/commitment_systems").

domain_priors:requires_active_enforcement(incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(incoherent_bundle_reading, 'a345ee87-7bd9-4f72-82bb-0c291e9bc006').
narrative_ontology:cs_kernel_codification('a345ee87-7bd9-4f72-82bb-0c291e9bc006', distributed).
narrative_ontology:cs_authority_grounding('a345ee87-7bd9-4f72-82bb-0c291e9bc006', extraction).
narrative_ontology:cs_reading_relation('a345ee87-7bd9-4f72-82bb-0c291e9bc006', incoherent_bundle_reading__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('a345ee87-7bd9-4f72-82bb-0c291e9bc006', incoherent_bundle_reading__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('a345ee87-7bd9-4f72-82bb-0c291e9bc006', foundational, fusion_requires_state_coercion).
narrative_ontology:cs_axiom_status(fusion_requires_state_coercion, holdable).
narrative_ontology:cs_axiom_grounding('a345ee87-7bd9-4f72-82bb-0c291e9bc006', fusion_requires_state_coercion, empirically_contingent).
narrative_ontology:cs_axiom('a345ee87-7bd9-4f72-82bb-0c291e9bc006', secondary, doctrinal_incoherence_is_extractive).
narrative_ontology:cs_axiom_status(doctrinal_incoherence_is_extractive, holdable).
narrative_ontology:cs_axiom_grounding('a345ee87-7bd9-4f72-82bb-0c291e9bc006', doctrinal_incoherence_is_extractive, deontological).
narrative_ontology:cs_reference_frame('a345ee87-7bd9-4f72-82bb-0c291e9bc006', pre_tokugawa_sectarian_autonomy).
narrative_ontology:cs_drift_state('a345ee87-7bd9-4f72-82bb-0c291e9bc006', late_edo_consolidation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a345ee87-7bd9-4f72-82bb-0c291e9bc006', '2026-06-08T14:32:00Z').
narrative_ontology:cs_kernel_id(incoherent_bundle_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(incoherent_bundle_reading, tokugawa_bakufu).
narrative_ontology:constraint_beneficiary(incoherent_bundle_reading, temple_administrative_hierarchy).
narrative_ontology:constraint_beneficiary(incoherent_bundle_reading, danka_system_enforcers).
narrative_ontology:constraint_victim(incoherent_bundle_reading, village_practitioners).
narrative_ontology:constraint_victim(incoherent_bundle_reading, heterodox_communities).
narrative_ontology:constraint_victim(incoherent_bundle_reading, doctrinal_coherence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(incoherent_bundle_reading, temple_administrative_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legally bound to Buddhist temple for mortuary rites and census registration (danka system) while required to participate in Shinto agricultural observances. Experience the shinbutsu fusion as unresolved doctrinal incoherence — contradictory ritual obligations with no interpretive framework for resolution. Cannot exit without losing civic status (temple registration required for travel permits, marriage registration, property transactions). Identity-locked: their social existence is constituted through the dual religious obligations.
narrative_ontology:constraint_stakeholder(incoherent_bundle_reading, village_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Kirishitan survivors, hidden Pure Land practitioners, and heterodox mountain ascetics suppressed by enforced religious conformity. Face legal penalties for non-compliance with danka registration and state-sanctioned ritual participation. Exit is structurally possible (migration to margins, apostasy, withdrawal to remote mountains) but carries severe social and economic costs. The shinbutsu bundle functions as surveillance mechanism that makes heterodox practice visible and punishable.
narrative_ontology:constraint_stakeholder(incoherent_bundle_reading, heterodox_communities, payer,
    moderate, biographical, constrained, regional).

% Uses the danka system as population census and heterodoxy suppression mechanism. Temple registration provides legibility: tracks household movement, identifies non-conformists, enforces ideological uniformity. The shinbutsu fusion is experienced as pure coordination — solves genuine administrative problems (population surveillance, Christian persecution) with negligible cost to the regime. Could abandon the system (arbitrage exit) but maintains it because it delivers control.
narrative_ontology:constraint_stakeholder(incoherent_bundle_reading, tokugawa_bakufu, agenda_setter,
    institutional, biographical, arbitrage, national).

% Benefits from guaranteed parishioners (danka households legally bound to temples regardless of belief) and from role as administrative organs of the state (collect registration fees, issue travel permits). But bears cost of doctrinal dilution: required to administer a system that erases sectarian distinctiveness and subordinates spiritual function to administrative function. Constrained exit — temples are structurally dependent on danka rents and cannot easily withdraw from the system without losing economic base.
narrative_ontology:constraint_stakeholder(incoherent_bundle_reading, temple_administrative_hierarchy, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(incoherent_bundle_reading, temple_administrative_hierarchy, payer).

% Post-Restoration observers who enacted shinbutsu bunri (separation) edicts to dissolve the Tokugawa fusion. View the shinbutsu bundle as degraded institutional artifact that persists through inertia after its primary function (bakufu surveillance) has dissolved. See the constraint as piton — temples maintain danka ties and village calendars preserve dual observances despite official separation, more from habit and institutional momentum than from functional necessity.
narrative_ontology:constraint_stakeholder(incoherent_bundle_reading, meiji_restorationists, observer,
    powerful, civilizational, mobile, national).

% Abstract good with no institutional advocate. The enforced shinbutsu bundle suppresses doctrinal clarity in favor of administrative convenience. Neither Buddhist sectarian doctrine nor Shinto ritual tradition is preserved in coherent form — both are instrumentalized for state surveillance. Doctrinal coherence cannot organize to defend itself and has no exit from the constraint.
narrative_ontology:constraint_stakeholder(incoherent_bundle_reading, doctrinal_coherence, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(incoherent_bundle_reading, doctrinal_coherence).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The danka system coordinates mortuary rites (assigns households to temples for death rituals and ancestor veneration) and provides population census infrastructure (temple registration tracks household composition and movement).
% TRANSFER_FUNCTION: The constraint moves registration fees, ritual payments, and labor (participation in temple and shrine observances) from village households to religious institutions and the bakufu administrative apparatus. It also moves conformity and legibility from heterodox communities to the state (suppression of alternative religious practice in exchange for civic status).
% ABSENT_VOICES: Heterodox practitioners are partially excluded — their objections (hidden Kirishitan, suppressed Pure Land, mountain ascetics) are documented but not represented in the system's design or justification. Doctrinal coherence as an abstract good has no advocate within the constraint's operation. The constraint's unanimity (village practitioners comply) arises from coercion, not from consensus — dissenting voices exist but are suppressed.
% DISAPPEARANCE_RATIONALE: If the danka system disappeared, village mortuary practice would require reorganization (alternative mechanisms for death ritual, ancestor veneration, and temple affiliation). The bakufu census and heterodoxy suppression infrastructure would require replacement. Temple economic base would require restructuring (danka rents are substantial revenue). The Meiji bunri edicts partially tested this: formal separation attempted but village practice rearranged slowly because institutional dependencies (mortuary monopolies, ritual calendars) persisted.
% FOUNDING_PROBLEM: The danka system was formalized in the 1630s to suppress Christianity after the Shimabara Rebellion (1637-1638). The founding problem was heterodoxy detection: how to identify and monitor potentially subversive religious practice (primarily Kirishitan but also other non-conformist groups). Compulsory Buddhist temple registration solved this by making religious affiliation visible and non-compliance punishable.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (Christian heterodoxy) was effectively eliminated by the 18th century — Kirishitan practice was driven deeply underground and posed no organizational threat to the bakufu. Meiji-era scholarship (Anesaki Masaharu, Takakusu Junjirō) documents that the danka system persisted long after its founding function (Christian suppression) became obsolete. The system's continuation served different functions (population census, temple economic base, social control) but the original problem was resolved. Corroboration from outside the beneficiary set: village records from late Edo period show danka compliance as routine administrative burden, not as active heterodoxy suppression. Heterodox communities themselves (hidden Kirishitan scholarship, kakure kirishitan oral histories) attest that the threat the system was built to counter had been successfully suppressed by mid-Edo, yet the system intensified rather than dissolved.
narrative_ontology:disappearance_verdict(incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(incoherent_bundle_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VILLAGE PRACTITIONER (SNARE) — Identity-locked in contradictory ritual obligations. Cannot exit the danka system without losing civic status. Experiences the shinbutsu fusion as incoherent extraction: required to maintain both Buddhist mortuary ties (temple registration) and Shinto agricultural observances under penalty, but given no interpretive framework for resolving the contradictions. The fusion is not experienced as synthesis but as accumulated obligation. Maximum effective extraction — the practitioner bears doctrinal incoherence as a personal cognitive burden with no resolution pathway.
constraint_indexing:constraint_classification(incoherent_bundle_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: HETERODOX COMMUNITY (SNARE) — Constrained by enforceable penalties for non-compliance with state-mandated religious registration. Kirishitan survivors, hidden Pure Land practitioners, and mountain ascetics experience the shinbutsu bundle as a surveillance mechanism that suppresses alternative commitments. Exit is structurally possible (migration, apostasy, withdrawal to margins) but carries severe costs. High effective extraction — the constraint extracts conformity without providing coherent doctrine.
constraint_indexing:constraint_classification(incoherent_bundle_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: TOKUGAWA BAKUFU (ROPE) — Benefits from enforced syncretism as a population control mechanism. The danka system (temple household registration) functions as census and surveillance. From this perspective the shinbutsu bundle is pure coordination: it solves the genuine administrative problem of tracking population movement and suppressing heterodoxy. The bakufu experiences negligible extraction — the constraint delivers legibility and control. Arbitrage exit (the regime could abandon the system) but chooses not to because it works.
constraint_indexing:constraint_classification(incoherent_bundle_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TEMPLE HIERARCHY (TANGLED ROPE) — Constrained by dual role as spiritual institutions and administrative organs of the state. Benefits from guaranteed parish membership (danka households legally bound to temples) but also bears the cost of doctrinal incoherence: required to administer a system that dilutes sectarian distinctiveness. Mixed extraction: temples collect rents from the danka system but sacrifice doctrinal autonomy. Some coordination (mortuary rites genuinely organize social reproduction) alongside extraction (compulsory registration regardless of belief).
constraint_indexing:constraint_classification(incoherent_bundle_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MEIJI RESTORATIONIST (PITON) — Views the Tokugawa shinbutsu bundle as degraded and inertial from the civilizational time horizon. The shinbutsu bunri (separation) edicts (1868-1874) attempted to dissolve the fusion, but the bundle persisted through institutional inertia: danka ties, mortuary monopolies, and village ritual calendars continued despite official separation. The Meiji observer sees a constraint whose primary function (bakufu population control) has atrophied but whose remnants remain as performance. High theater ratio — the post-bunri temples maintain rituals whose administrative function is gone.
constraint_indexing:constraint_classification(incoherent_bundle_reading, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From the analytical position this reading classifies the shinbutsu substrate as a snare: state-enforced fusion that extracts doctrinal conformity from practitioners while providing no coherent interpretive kernel. The 'syncretism' is not a synthesis (which would be rope or scaffold) but an accumulated bundle of contradictory obligations held together by coercion. The constraint suppresses alternatives (hidden Pure Land, Kirishitan survival, heterodox mountain practices) and benefits identifiable agents (bakufu legibility, temple rents). Doctrinal coherence is the victim — an abstract good with no advocate.
constraint_indexing:constraint_classification(incoherent_bundle_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(incoherent_bundle_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(incoherent_bundle_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(incoherent_bundle_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(incoherent_bundle_reading, TR),
    TR >= 0.70.

:- end_tests(incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial. The danka system extracts compulsory registration and ritual participation regardless of belief. Village practitioners bear contradictory obligations (Buddhist mortuary ties + Shinto agricultural rites) without resolution framework. Temples collect guaranteed rents but sacrifice doctrinal autonomy. The extraction is not maximal (0.58 not 0.85) because some coordination function exists: mortuary rites do organize social reproduction, and agricultural observances do coordinate planting/harvest cycles. But the coordination is layered with extraction — the compulsory nature and doctrinal incoherence are extractive overhead beyond coordination need. Suppression (0.68): High. The danka system suppresses exit via legal penalties (loss of temple registration = loss of civic status). Heterodox communities (Kirishitan, Pure Land, mountain practices) face active persecution. The suppression requirement peaks in mid-Edo (0.75) as bakufu enforcement hardens, then declines through bakumatsu (0.62) as enforcement capacity erodes, dropping sharply post-bunri (0.45) when legal penalties are removed but institutional inertia persists. Theater ratio (0.52): Moderate-high. The shinbutsu fusion is partly performative: elite honji suijaku theory (本地垂迹論) rationalizes kami-buddha equivalence, but village practice often maintains contradictory rites without synthesis. The theater ratio rises over the interval (0.32 → 0.68) as the constraint's administrative function (bakufu surveillance) becomes increasingly routinized and its doctrinal function (if any) atrophies. By Meiji the constraint is substantially theatrical — temples maintain danka ties after the administrative function dissolves.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals how the same structural constraint appears as pure coordination (rope) to the bakufu, mixed coordination-extraction (tangled rope) to temples, pure extraction (snare) to village practitioners and heterodox communities, and degraded performance (piton) to Meiji observers. The bakufu sees the danka system as solving a genuine administrative problem (population tracking, heterodoxy suppression) with negligible cost to themselves. Temples see mixed benefit (guaranteed parishioners) and cost (doctrinal dilution). Village practitioners see contradictory obligations with no resolution and no exit. Heterodox communities see suppression of alternatives. The Meiji observer sees institutional inertia after the primary function (bakufu surveillance) has dissolved. The analytical classification (snare) reflects the structural data: identifiable beneficiaries (bakufu, temples), identifiable victims (practitioners, heterodox communities), substantial suppression, and no coherent interpretive kernel that would justify the coordination claim. The key distinction from the syncretic_fusion_reading is that this reading denies kernel coherence: the fusion is an accumulated bundle, not a synthesis.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective derives from beneficiary/victim status and exit options. The Tokugawa bakufu is a declared beneficiary with arbitrage exit → low d → low effective extraction (experiences rope). Temple hierarchy is both beneficiary (guaranteed parishioners) and victim (doctrinal dilution) with constrained exit → moderate d → moderate effective extraction (experiences tangled rope). Village practitioners are declared victims with identity_locked exit → high d → high effective extraction (experiences snare). Heterodox communities are declared victims with constrained exit → high d (though not maximal due to some exit capacity) → high effective extraction (experiences snare). The analytical observer computes the constraint as snare based on structural data but with d derived from the analytical position (no direct participation in the extraction flow). The Meiji restorationist perspective has powerful/mobile context → low d → but classifies as piton due to the theater gate (high theater_ratio post-bunri), not due to high chi. The directionality derivation distinguishes structural extraction (beneficiary/victim relationships) from experienced extraction (chi as a function of d and context).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through the committer frame. The three readings (syncretic_fusion, domain_partition, incoherent_bundle) are competing framings of the shinbutsu substrate, not observer-dependent perspectives on a single constraint. Each reading instantiates a structurally distinct constraint with different ε values and different beneficiary/victim sets. The syncretic_fusion_reading would classify the substrate as rope or scaffold (genuine synthesis, possibly with sunset as modernization proceeds). The domain_partition_reading would classify as tangled_rope (functional separation with residual extraction at boundaries). This incoherent_bundle_reading classifies as snare (state-enforced extraction with no coherent kernel). The readings coexist as live positions held by different scholarly and practitioner communities: (1) scholars emphasizing honji suijaku theory and elite integration support syncretic_fusion; (2) scholars emphasizing functional domain separation (temples handle death, shrines handle life-cycle transitions) support domain_partition; (3) scholars emphasizing coercion, doctrinal incoherence, and heterodox suppression support incoherent_bundle. The empirical resolution mechanism is whether village practitioners experienced the fusion as resolved or unresolved — testimony in Tokugawa legal records, persistence of heterodox practice, and post-bunri persistence patterns are the key observables. The mandatrophy is not 'which reading is correct' but 'which structural relationships do the observables support.' This reading's omega variables document the irreducible uncertainties: kernel existence, coordination function authenticity, and post-bunri persistence mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_existence,
    'Does a coherent ontological kernel exist beneath the shinbutsu fusion, or is the fusion an incoherent bundle of contradictory commitments held together by state enforcement?',
    'Historical analysis of whether practitioners experienced the fusion as resolved synthesis or as unresolved contradiction. Evidence: (1) presence/absence of synthetic doctrinal texts attempting to reconcile kami and buddha ontologies (e.g., honji suijaku theory as genuine kernel vs. elite rationalization); (2) peasant testimony in Tokugawa legal records showing doctrinal confusion vs. doctrinal integration; (3) rate of heterodox persistence (hidden Kirishitan, suppressed Pure Land) as signal of coerced incoherence.',
    'If coherent kernel exists: the constraint reclassifies toward syncretic_fusion_reading (rope/scaffold from more perspectives). If incoherent bundle: this reading is structurally correct (snare from most perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_existence, empirical, 'Whether shinbutsu fusion has a coherent ontological kernel or is an enforced incoherent bundle').

omega_variable(
    committer_frame_alternative,
    'Are the three readings (syncretic_fusion, domain_partition, incoherent_bundle) three structural states of the same kernel, or three different kernels that share a label?',
    'Cross-reading comparison of whether ε and beneficiary/victim sets are stable across readings. If readings produce widely divergent ε values, they are different constraints (different kernels). If they share ε but differ only in framing, they are readings of the same kernel.',
    'If different kernels: the shinbutsu_ontological_substrate label is ambiguous and should be decomposed further. If same kernel: the committer frame correctly models three readings of one contested substrate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_alternative, conceptual, 'Whether the three readings are framings of one kernel or separate kernels sharing a label').

omega_variable(
    danka_system_coordination_function,
    'Does the danka system (temple household registration) provide genuine mortuary coordination, or is its mortuary function a cover for administrative extraction?',
    'Comparison of danka system to pre-Tokugawa mortuary arrangements. If danka solves a coordination problem that existed before enforcement (e.g., disputes over mortuary rites, unclear ritual obligations), it has genuine coordination function. If pre-Tokugawa communities handled mortuary rites without danka-like compulsion, the coordination story is cover.',
    'If genuine coordination: tangled_rope classification gains support (coordination + extraction). If cover: snare classification gains support (pure extraction with coordination theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(danka_system_coordination_function, empirical, 'Whether danka system mortuary function is genuine coordination or extraction cover').

omega_variable(
    post_bunri_persistence_mechanism,
    'Why did shinbutsu fusion persist in village practice after the Meiji bunri edicts formally separated Buddhism and Shinto?',
    'Analysis of post-1868 village ritual records. Two competing mechanisms: (1) internalized fusion — practitioners had genuinely integrated the bundle and resisted separation (signals genuine kernel); (2) institutional inertia — temples retained danka monopolies and village calendars preserved dual observances because no alternative infrastructure existed (signals piton/inertial persistence).',
    'If internalized fusion: supports syncretic_fusion_reading. If institutional inertia: supports this reading (incoherent bundle held by enforcement, persisting as piton after enforcement weakens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_bunri_persistence_mechanism, empirical, 'Mechanism of post-bunri persistence: internalized synthesis vs institutional inertia').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(incoherent_bundle_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(incoh_theater_early_edo, incoherent_bundle_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(incoh_theater_mid_edo, incoherent_bundle_reading, theater_ratio, 50, 0.44).
narrative_ontology:measurement(incoh_theater_late_edo, incoherent_bundle_reading, theater_ratio, 100, 0.52).
narrative_ontology:measurement(incoh_theater_bakumatsu, incoherent_bundle_reading, theater_ratio, 150, 0.61).
narrative_ontology:measurement(incoh_theater_meiji, incoherent_bundle_reading, theater_ratio, 200, 0.68).

% Extraction over time
narrative_ontology:measurement(incoh_extract_early_edo, incoherent_bundle_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(incoh_extract_mid_edo, incoherent_bundle_reading, base_extractiveness, 50, 0.54).
narrative_ontology:measurement(incoh_extract_late_edo, incoherent_bundle_reading, base_extractiveness, 100, 0.58).
narrative_ontology:measurement(incoh_extract_bakumatsu, incoherent_bundle_reading, base_extractiveness, 150, 0.56).
narrative_ontology:measurement(incoh_extract_meiji, incoherent_bundle_reading, base_extractiveness, 200, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(incoh_suppress_early_edo, incoherent_bundle_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(incoh_suppress_mid_edo, incoherent_bundle_reading, suppression_requirement, 50, 0.75).
narrative_ontology:measurement(incoh_suppress_late_edo, incoherent_bundle_reading, suppression_requirement, 100, 0.68).
narrative_ontology:measurement(incoh_suppress_bakumatsu, incoherent_bundle_reading, suppression_requirement, 150, 0.62).
narrative_ontology:measurement(incoh_suppress_meiji, incoherent_bundle_reading, suppression_requirement, 200, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(incoherent_bundle_reading, identity_coordination).
narrative_ontology:affects_constraint(incoherent_bundle_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(incoherent_bundle_reading, domain_partition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the shinbutsu_ontological_substrate kernel. The three readings (syncretic_fusion, domain_partition, incoherent_bundle) form a constraint family linked by committer-frame network edges. Each reading instantiates a structurally distinct constraint with different ε values, different beneficiary/victim relationships, and different classifications. The readings are not observer-dependent perspectives (which would be handled by the perspectives array within one story) but competing framings of what the substrate IS. Cross-reading analysis measures whether the observables (practitioner testimony, heterodox persistence, post-bunri trajectories) converge on one reading or remain contested.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
