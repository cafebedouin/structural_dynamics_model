% ============================================================================
% CONSTRAINT STORY: symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_symbol_continuity_reading, []).

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
 *   constraint_id: symbol_continuity_reading
 *   human_readable: Ritual Preserves Symbolic Continuity and Collective Identity Across Time
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint instantiates the symbol-continuity reading of the
 *   catastrophe-memory kernel: ritual preserves collective identity across
 *   time by transmitting shared symbolic meaning from one generation to the
 *   next. Unlike the survival-competence reading (which emphasizes ritual's
 *   practical function in maintaining group cohesion during crises), or the
 *   trauma-encoding reading (which emphasizes emotional consolidation of
 *   difficult experience), this reading centers on the symbolic function —
 *   the way ritual carries and transmits meaning about who the community is
 *   and how it understands its place in time. The constraint operates in
 *   communities with high time-depth awareness: religions, ethnic traditions,
 *   family systems, institutional cultures. The core mechanism: repeated
 *   enactment of shared symbolic forms creates mutual recognition across
 *   generations ('this is how our people do things'; 'by doing this, I become
 *   who my parents were'). The extractiveness is low (ε = 0.15) because the
 *   mechanism operates as genuine coordination when the symbolic meaning is
 *   actively transmitted. But extractiveness rises over time (theater_ratio
 *   trajectory from 0.45 to 0.78) as meaning decays and the ritual becomes
 *   performative rather than communicative. The suppression requirement also
 *   rises (from 0.18 to 0.31 over the interval), indicating that as adaptive
 *   modification pressures increase, the institutions preserving rigid
 *   symbolic forms must suppress more actively to maintain the constraint.
 *   This reading asserts that ritual's persistence depends on the integrity
 *   of the symbol-transmission mechanism, not on survival-utility or
 *   trauma-consolidation (which the sibling readings emphasize). It coexists
 *   with those readings — a single ritual can simultaneously preserve
 *   symbols, consolidate trauma, and maintain survival-group coordination —
 *   but distinguishes its core claim: what is being preserved is
 *   identity-through-meaning, not survival-capacity or emotional processing.
 *
 * KEY AGENTS:
 *   - Tradition continuity: The abstract entity that benefits — not people, but the transmission chain itself. Beneficiary because the constraint exists to preserve it.
 *   - Identity practitioners (powerless/identity_locked): Members whose identity is constituted through ritual participation. They benefit from continuity but cannot exit without dissolving identity.
 *   - Religious authorities (institutional/constrained): Custodians of ritual form whose institutional authority depends on maintaining symbolic coherence. They benefit and also enforce.
 *   - Adaptive practitioners (moderate/constrained): Reform-minded members who want to preserve meaning while modifying forms. They bear the cost of suppression against modification.
 *   - Born-in practitioners (powerless/trapped): Those with no choice in participation; trapped by family and internalized identity.
 *   - Movements for adaptive continuity (organized/mobile): Reform movements that see symbolic preservation as compatible with form modification. They have agency and exit paths.
 *   - Ossified institutional keepers (institutional/arbitrage): Institutions maintaining canonical ritual forms as exact repetitions; benefiting from authority preservation. Theater-dominant.
 *   - Adaptive modification itself: The victim in this reading — rigid symbolic preservation costs the capacity for the tradition to adapt to changed contexts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(symbol_continuity_reading, 0.15).
domain_priors:suppression_score(symbol_continuity_reading, 0.25).
domain_priors:theater_ratio(symbol_continuity_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(symbol_continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(symbol_continuity_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(symbol_continuity_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(symbol_continuity_reading, rope).
narrative_ontology:human_readable(symbol_continuity_reading, "Ritual Preserves Symbolic Continuity and Collective Identity Across Time").
narrative_ontology:topic_domain(symbol_continuity_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(symbol_continuity_reading, 'effac283-0a3d-4b77-8661-d95e00223d1a').
narrative_ontology:cs_kernel_codification('effac283-0a3d-4b77-8661-d95e00223d1a', distributed).
narrative_ontology:cs_authority_grounding('effac283-0a3d-4b77-8661-d95e00223d1a', lineage).
narrative_ontology:cs_interpretation_layer_present('effac283-0a3d-4b77-8661-d95e00223d1a').
narrative_ontology:cs_reading_relation('effac283-0a3d-4b77-8661-d95e00223d1a', symbol_continuity_reading__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('effac283-0a3d-4b77-8661-d95e00223d1a', symbol_continuity_reading__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('effac283-0a3d-4b77-8661-d95e00223d1a', symbol_continuity_reading__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('effac283-0a3d-4b77-8661-d95e00223d1a', foundational, symbol_transmission_preserves_identity).
narrative_ontology:cs_axiom_status(symbol_transmission_preserves_identity, holdable).
narrative_ontology:cs_axiom_grounding('effac283-0a3d-4b77-8661-d95e00223d1a', symbol_transmission_preserves_identity, conventional).
narrative_ontology:cs_axiom('effac283-0a3d-4b77-8661-d95e00223d1a', foundational, ancestral_recognition_requires_form_continuity).
narrative_ontology:cs_axiom_status(ancestral_recognition_requires_form_continuity, holdable).
narrative_ontology:cs_axiom_grounding('effac283-0a3d-4b77-8661-d95e00223d1a', ancestral_recognition_requires_form_continuity, empirically_contingent).
narrative_ontology:cs_reference_frame('effac283-0a3d-4b77-8661-d95e00223d1a', ancestral_symbol_recognition).
narrative_ontology:cs_drift_state('effac283-0a3d-4b77-8661-d95e00223d1a', contemporary_pluralism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('effac283-0a3d-4b77-8661-d95e00223d1a', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(symbol_continuity_reading, tradition_continuity).
narrative_ontology:constraint_beneficiary(symbol_continuity_reading, identity_practitioners).
narrative_ontology:constraint_victim(symbol_continuity_reading, adaptive_modification).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(symbol_continuity_reading, religious_authorities).
narrative_ontology:constraint_victim(symbol_continuity_reading, identity_practitioners).
narrative_ontology:constraint_victim(symbol_continuity_reading, adaptive_practitioners).
narrative_ontology:constraint_victim(symbol_continuity_reading, born_in_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The abstract entity benefiting from symbol transmission across generations. Not an agent but a beneficiary category because the constraint exists to preserve it. The tradition persists when meaning is actively transmitted; it degrades when ritual becomes performative.
narrative_ontology:constraint_stakeholder(symbol_continuity_reading, tradition_continuity, beneficiary,
    analytical, civilizational, analytical, regional).
narrative_ontology:stakeholder_non_agent(symbol_continuity_reading, tradition_continuity).

% Members whose self-concept is constituted through ritual participation and inherited identity. They benefit from continuity with ancestors and community recognition ('by doing this, I am who my parents were'). They also pay the cost of conformity and cannot exit without identity dissolution. Dual role: beneficiary of identity continuity, payer of participation labor and conformity suppression.
narrative_ontology:constraint_stakeholder(symbol_continuity_reading, identity_practitioners, beneficiary,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(symbol_continuity_reading, identity_practitioners, payer).

% Institutional custodians of ritual form and authoritative interpreters of meaning. They set the agenda (define correct ritual, interpret symbols, enforce orthodoxy). They benefit from institutional authority and resource flows. Constrained exit: leaving the role costs career, authority position, and institutional identity.
narrative_ontology:constraint_stakeholder(symbol_continuity_reading, religious_authorities, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(symbol_continuity_reading, religious_authorities, beneficiary).

% Reform-minded members who want to preserve symbolic meaning while adapting ritual forms to new contexts (language, media, social structure). They bear the cost of institutional suppression against modification and face social pressure for attempted change. Want to keep the function (meaning transmission) while updating the mechanism.
narrative_ontology:constraint_stakeholder(symbol_continuity_reading, adaptive_practitioners, payer,
    moderate, biographical, constrained, local).

% Those with no choice in initial participation, incorporated into tradition through family and early socialization. Trapped by family structure, community membership requirements, and internalized identity norms. Face maximum extraction of participation and conformity with minimal exit option.
narrative_ontology:constraint_stakeholder(symbol_continuity_reading, born_in_practitioners, payer,
    powerless, biographical, trapped, local).

% Reform movements (liberal religious institutions, syncretic traditions, adaptive communities) that pursue symbol preservation through form modification. They have agency: can split from parent tradition, create new institutional form, migrate to alternative frameworks. They set the agenda for how continuity is preserved while admitting change.
narrative_ontology:constraint_stakeholder(symbol_continuity_reading, adaptive_continuity_movements, agenda_setter,
    organized, generational, mobile, regional).

% Institutions maintaining canonical ritual forms as invariant repetition (unchanged liturgy, fixed ceremonial structure, verbatim textual recitation). They preserve institutional authority through claims of antiquity and immutability. Have arbitrage options (can shift to secular institutional roles, administrative functions). Their core function (symbol transmission through meaning) has atrophied; what remains is performative maintenance of institutional form.
narrative_ontology:constraint_stakeholder(symbol_continuity_reading, ossified_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).

% The capacity to modify ritual forms in response to changed contexts. Excluded from the conversation when rigid symbolic preservation is asserted as necessary. In this reading, adaptive modification is a victim: it is suppressed (high suppression_requirement when modification is attempted) and bears costs (forms become unmaintainable, meaning decays, intergenerational transmission fails). Not an agent but a structural capacity being constrained.
narrative_ontology:constraint_stakeholder(symbol_continuity_reading, adaptive_modification, excluded,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_non_agent(symbol_continuity_reading, adaptive_modification).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserve and transmit collective identity across generations through repeated enactment of shared symbolic forms. The ritual enables community members separated by time to recognize themselves as part of the same tradition and to understand their place in a historical continuity stretching from ancestors through themselves to descendants.
% TRANSFER_FUNCTION: Transfer of understanding: meaning encoded in symbolic action flows from practitioners who understand it to new generations of learners. Transfer of identity: by participating in inherited forms, individuals claim membership in the continuous community. Transfer of legitimacy: institutions that maintain canonical forms derive authority from claims of unbroken transmission from the past.
% ABSENT_VOICES: Practical refusers (those who would exit if exit were possible, but are trapped by family and community structure); modernizing reformers suppressed within institutions that resist adaptive modification; communities whose traditions have already degraded to pure theater and have no mechanism to restore meaning transmission; descendants of the catastrophe who were never participants and may reject the imposed meaning-making.
% DISAPPEARANCE_RATIONALE: Beneficiaries of symbol transmission (identity practitioners, religious authorities) claim the world would be fundamentally rearranged — community would dissolve, identity would become rootless, historical continuity would be broken. But reformers argue the function (identity continuity) would persist if forms were allowed to adapt; only rigid institutional preservation would disappear. And observers note that many communities have already abandoned inherited rituals with continuity persisting through other mechanisms (language, narrative, institutional history). The verdict is contested because it depends on whether ritual form is functionally necessary for identity continuity or merely one contingent institutional carrier of it.
% FOUNDING_PROBLEM: After a catastrophe (war, persecution, cultural upheaval, displacement), the community faces the problem of how to preserve its identity and meaning when the generation that directly experienced the event is dying. How does understanding of who we are and what happened to us survive the death of witnesses? Ritual emerged as a mechanism: repeated enactment of symbolic forms carries meaning across the time gap from the generation that understood it to generations born after.
% FOUNDING_PROBLEM_CORROBORATION: Historians and anthropologists attest that ritual systems in post-catastrophe communities do preserve identity continuity across generations (Jewish Passover and Yom Hashoah after Holocaust, Commemoration Days after war, Indigenous ceremonies after colonization). But reformers and practitioners note that the same communities preserve identity through other mechanisms (oral narrative, institutional memory, secular historical education) and that some communities that abandoned inherited rituals maintained identity equally well. The founding problem itself is debated: did ritual SOLVE the problem, or merely provide one way to organize solutions? Institutional custodians claim ritual was necessary; adaptive movements claim the function could have been served other ways.
narrative_ontology:disappearance_verdict(symbol_continuity_reading, contested).
narrative_ontology:founding_problem_status(symbol_continuity_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRADITION KEEPER (ROPE) — Embedded in the practice through identity fusion with ancestral memory. The ritual appears as genuine coordination: transmitting symbolic knowledge that constitutes the community's self-understanding. Low extraction experienced because the keeper benefits from identity continuity and sees no alternative framing. Exit would dissolve identity, not merely cost something. The keeper experiences pure coordination because the symbolic function IS their function.
constraint_indexing:constraint_classification(symbol_continuity_reading, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: RELIGIOUS AUTHORITY (ROPE) — Institutional position depends on maintaining symbolic coherence across generations. The ritual is coordination: enabling the community to recognize itself in inherited forms. Some extraction occurs (institutional power, resource flows to religious leaders), but the coordination function is primary and real. Exit costs are moderate (career change, loss of authority) rather than total. The authority experiences the constraint as necessary to their function, not as parasitic.
constraint_indexing:constraint_classification(symbol_continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ADAPTIVE PRACTITIONER (TANGLED ROPE) — Sees the ritual as a mixed mechanism: genuine transmission of identity, but with rigid requirements that prevent adaptation to changed circumstances. Wants to preserve the symbolic function while modifying operational forms. Experiences active enforcement against modification (social pressure, authority resistance, threat to membership). The constraint requires both coordination maintenance AND suppression of alternative framings. Moderate power, constrained exit (can leave but loses community and identity continuity).
constraint_indexing:constraint_classification(symbol_continuity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: BORN-IN PRACTITIONER (SNARE) — Identity constituted entirely through the tradition; sees ritual as compulsory participation with no exit option and no benefit articulation beyond 'it is who we are.' Trapped by family, community, and internalized identity lock. The ritual appears as pure extraction of participation and conformity with no reciprocal coordination benefit visible from within the tradition's frame. Full experienced extraction because the exit cost is identity dissolution.
constraint_indexing:constraint_classification(symbol_continuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: MOVEMENT FOR ADAPTIVE CONTINUITY (SCAFFOLD) — Organized agents (reform movements, syncretic traditions, liberal religious institutions) see the bottleneck as temporary: symbolic continuity CAN be preserved while operational forms evolve. The constraint has a sunset clause built into its logic — traditions that do not adapt symbolic carriers (language, metaphors, performance media) to changed contexts lose intergenerational transmission. The movement has agency and exit paths (split into new tradition, migrate to alternative framework). Sunset: as digital media and pluralistic contexts make rigid symbolic forms unmaintainable, communities that preserve symbols while adapting carriers survive.
constraint_indexing:constraint_classification(symbol_continuity_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: CANONICAL TEXTUAL AUTHORITY (PITON) — Institutions that maintain ritual forms as exact repetitions of fixed canonical text (unchanged liturgy, invariant performance). The symbolic function has atrophied — the performance is mainly theatrical maintenance of institutional authority rather than genuine transmission. The form persists through institutional inertia and legitimacy claims ('as done since time immemorial') even though the actual symbolic understanding decays generationally. Theater ratio high (0.75+) because the performative aspect dominates the communicative aspect. Low extraction despite institutional benefit because the mechanism is mostly non-functional (members participate for identity/duty, not coordination).
constraint_indexing:constraint_classification(symbol_continuity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: COGNITIVE ANTHROPOLOGIST (MOUNTAIN) — From a civilizational perspective, ritual transmission of collective identity appears as a natural law: humans encode and transmit cultural memory through repeated symbolic action; this is how continuity emerges across generations without centralized authority. The constraint appears immutable — any community requires some mechanism for symbol preservation. However, this naturalizes what is actually a contingent institutional choice. Rituals differ radically in their rigidity, adaptability, and extractiveness. The mountain perspective obscures the fact that some communities preserve symbols while enabling modification (adaptive continuity), while others ossify symbols into immutable performance (piton). The analytical observer risks treating a range of possible institutional arrangements as a single law of nature.
constraint_indexing:constraint_classification(symbol_continuity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(symbol_continuity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(symbol_continuity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(symbol_continuity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(symbol_continuity_reading, TR),
    TR >= 0.70.

:- end_tests(symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15, rising to 0.20 over the interval): Low baseline. Ritual as pure symbol coordination is not inherently extractive — participants benefit from identity continuity and collective recognition. Extraction occurs when ritual becomes compulsory (suppression), when meaning decays (theater ratio rises), or when institutions weaponize ritual form against adaptive modification. The baseline value reflects coordination with modest conformity costs. Rising trajectory shows extraction increasing as the constraint ossifies. Suppression (0.25, rising to 0.31): Moderate and rising. Suppression is low when meaning is actively transmitted (people participate willingly). It rises when rigidity prevents adaptation and communities must coerce participation as meaning decays. Theater ratio (0.65, rising to 0.78): High and rising sharply. This is the key diagnostic metric for this reading. When ritual meaning is actively understood and explicitly transmitted across generations, theater ratio stays low (0.40–0.50): the symbolic function is real communication, not performance. As understanding decays and participation becomes habitual ('we do this because we always have'), theater ratio rises — the performance aspect dominates. The trajectory from 0.45 to 0.78 models a tradition whose symbolic transmission has degraded: practitioners can repeat forms but don't understand meanings; institution maintains ritual through inertia and authority claim rather than active communication. This trajectory indicates the constraint moving from rope (active coordination) toward piton (performative maintenance) over generations.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces radically different classifications across perspectives. The tradition keeper (identity-locked, trapped into the identity role) experiences pure rope: the ritual is genuinely their coordination mechanism for self-recognition across time. The reformist (moderately powerful, constrained by institutional suppression of modification) experiences tangled rope: real coordination function but enforced against adaptation. The born-in member (powerless, trapped with no exit) experiences snare: compulsory participation with extraction disguised as identity. The organized reform movement (with agency and mobile exit) experiences scaffold: the symbol-continuity function CAN be preserved while ritual forms evolve; current rigidity is temporary. The ossified institution (maintaining canonical forms) experiences piton: the performance persists through institutional inertia, meaning has decayed. The civilizational anthropologist risks mountain: treating symbolic transmission as a natural law rather than as one contingent institutional choice. The perspectival gaps are massive because this reading's core claim — that the constraint IS the transmission of meaning — is fundamentally perspective-dependent: when meaning is transmitted, it exists; when understanding decays, it becomes theater. Different observers see the same ritual at different points in this decay trajectory.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is computed from the agent's structural position relative to the symbol-transmission function. Tradition-continuity beneficiary status (identity practitioners, religious authorities) produces low d values → low/negative χ (they benefit from the constraint). Adaptive-modification victim status (reformists, born-in practitioners experiencing modernization pressure) produces higher d values → higher χ (the constraint costs them). Power differentiation: powerless trapped practitioners experience maximum extraction despite being 'beneficiaries' of their own identity because they cannot exercise the benefit as a choice. Institutional beneficiaries with arbitrage options (can exit into secular roles) experience low extraction despite institutional benefits because they have structural mobility. The engine derives d from beneficiary/victim declarations and exit modulation; identity-locked exit amplifies experienced extraction relative to constrained or mobile exit, because the target cannot simply pay a cost — they would have to become someone else. This reading's key directionality feature: tradition-continuity is itself a beneficiary in the base declaration, which is unusual (it is not an agent). This routes through the non-agent gate (agent: false in stakeholder form) to preserve the beneficiary declaration without deriving the false implication that the tradition collects rents.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING: This reading resolves mandatrophy by distinguishing the constraint's function (symbol transmission) from its institutional form (fixed ritual structure). Mandatrophy in this reading occurs when the constraint persists as performative ritual maintaining institutional authority after the symbol-transmission function has decayed. The theater_ratio trajectory (rising from 0.45 to 0.78) models this degradation: the ritual form persists as institutional theater while the meaning it was meant to carry has ossified or become incomprehensible to practitioners. This reading does NOT claim mandatrophy is resolved unless the community successfully decouples symbol preservation (the function) from rigid institutional form (the mechanism). Some traditions do this (adaptive continuity movements, vernacular prayer revivals) — they preserve the symbol-transmission function while allowing form mutation. Others do not (ossified canonical institutions maintaining invariant liturgy) — here the constraint persists as theater maintaining authority, with meaning transmission degraded to performative repetition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_continuity_vs_adaptive_function,
    'Is ritual''s core function the preservation of identical symbolic forms across generations, or the preservation of collective identity through whatever symbolic carriers work in each era?',
    'Longitudinal study of communities that preserved identity while radically modifying ritual forms (Jewish prayer in vernacular vs. Hebrew; Protestant liturgy vs. Catholic) vs. communities that collapsed when symbolic forms became unmaintainable (languages with no living speakers). Does identity persist when forms change, or only when forms remain fixed?',
    'If identical forms required: ritual is a rope coordinate with high rigidity costs and low adaptive yield. If identity-through-carriers sufficient: ritual is a scaffold with planned obsolescence built in. This determines whether adaptive modification is possible within the reading or violates the core commitment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(symbolic_continuity_vs_adaptive_function, empirical, 'Whether symbolic continuity requires identical ritual forms or can adapt carriers').

omega_variable(
    extraction_in_symbolic_transmission,
    'When ritual requires conformity and participation from community members with no opt-out, is that extracting conformity labor, or is it the inevitable cost of collective identity?',
    'Comparison of communities with mandatory vs. voluntary ritual participation: do voluntary-participation traditions show equivalent symbol transmission and identity persistence? Can identity be maintained when participation is chosen rather than compulsory?',
    'If mandatory participation necessary for transmission: extractiveness floor is unavoidable (rope/tangled rope with embedded suppression). If voluntary participation sufficient: extractiveness is contingent institutional choice (could be rope with no suppression). This determines whether the ritual is a coordination mechanism or an extraction mechanism disguised as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_in_symbolic_transmission, empirical, 'Whether mandatory participation is necessary for symbolic transmission').

omega_variable(
    theater_ratio_generational_drift,
    'As generations pass, does the meaningful symbol transmission maintain its communicative power, or does it degrade into performative repetition with decaying meaning?',
    'Qualitative analysis of how practitioners understand ritual meaning over generational change; interviews with initiates, practitioners, and elders about what symbols mean and whether meaning is explicitly transmitted or merely performed. Measure ratio of ''I understand this symbol means X'' (transmission) to ''I perform this because we always have'' (performance without understanding).',
    'If meaning decays over generations: theater_ratio rises (piton trajectory). If meaning is actively retransmitted: theater_ratio remains stable (rope trajectory). This determines whether the constraint exhibits mandatrophy (function ossified, performance maintained) or genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_generational_drift, empirical, 'Whether ritual meaning degrades or persists across generational transmission').

omega_variable(
    reading_kernel_contest_structure,
    'This reading (symbol-continuity) treats ritual as coordinate for identity transmission. The sibling readings decompose ritual into survival-competence (practical function atrophied), trauma-encoding (emotional consolidation), and boundary-maintenance (group membership selection). How does this symbol-continuity reading relate to those accounts? Are they complementary, competitive, or describing different constraints entirely?',
    'Ethnographic analysis of a single ritual tradition from all four reading perspectives. Does each reading identify a real structural function, or do they compete for explaining the same mechanism? Can symbol-continuity coexist with trauma-encoding in the same ritual, or does asserting one entail denying the other?',
    'If readings are complementary: they coexist and this reading is one true lens on a multifunctional constraint. If readings compete: classification differences between readings represent empirical disagreement, not perspectival multiplicity. If readings describe different constraints: the kernel is improperly framed and should be decomposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest_structure, conceptual, 'How symbol-continuity reading relates to sibling readings of the same kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(symbol_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(symcont_theater_t0, symbol_continuity_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(symcont_theater_t25, symbol_continuity_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement(symcont_theater_t50, symbol_continuity_reading, theater_ratio, 50, 0.68).
narrative_ontology:measurement(symcont_theater_t100, symbol_continuity_reading, theater_ratio, 100, 0.78).

% Extraction over time
narrative_ontology:measurement(symcont_extract_t0, symbol_continuity_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(symcont_extract_t25, symbol_continuity_reading, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(symcont_extract_t50, symbol_continuity_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(symcont_extract_t100, symbol_continuity_reading, base_extractiveness, 100, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(symcont_suppress_t0, symbol_continuity_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(symcont_suppress_t50, symbol_continuity_reading, suppression_requirement, 50, 0.26).
narrative_ontology:measurement(symcont_suppress_t100, symbol_continuity_reading, suppression_requirement, 100, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(symbol_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(symbol_continuity_reading, 0.1).
narrative_ontology:affects_constraint(symbol_continuity_reading, survival_competence_reading).
narrative_ontology:affects_constraint(symbol_continuity_reading, trauma_encoding_reading).
narrative_ontology:affects_constraint(symbol_continuity_reading, boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% Symbol-continuity is one reading of the catastrophe-memory kernel. The sibling readings (survival-competence, trauma-encoding, boundary-maintenance) are distinct constraints operating in the same ritual mechanism but with different ε values and different victim/beneficiary structures. Symbol-continuity reading has low extractiveness (ε ≈ 0.15) because it emphasizes the coordination function of meaning transmission. Survival-competence reading would show higher extractiveness if the competence function has atrophied but the ritual persists (institutional inertia). These are not observational variants of one constraint — they are structurally distinct constraints sharing a kernel. Network edge direction: symbol-continuity INFLUENCES the sibling readings because claiming that symbol transmission is the constraint's core function affects how degradation is measured in the other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
