% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission_flat_control, []).

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
 *   constraint_id: catastrophe_memory_transmission_flat_control
 *   human_readable: Obligation to Transmit Catastrophe-Memory Through Fixed Ritual Practice
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   The obligation to transmit catastrophe-memory through fixed ritual
 *   practice creates a structural binding between institutional identity
 *   preservation and psychological identity formation in younger generation
 *   participants. The constraint operates as a coordination mechanism —
 *   communities separated by geography, time, and acculturation pressure need
 *   a stable reference point to maintain shared identity and historical
 *   consciousness — but embeds substantial extraction: the fixity of the
 *   ritual form prevents historiographic revision, constrains interpretation,
 *   and binds participants through identity fusion rather than voluntary
 *   association. The constraint exhibits characteristics of all six DR types
 *   from different structural positions, revealing how a single institutional
 *   arrangement can appear as coordination, extraction, natural law,
 *   performance, temporary support, or pure coercion depending on the
 *   observer's power, time horizon, exit options, and spatial scope. The
 *   rising theater ratio over the interval (0.35 to 0.64) reflects increasing
 *   organizational effort to maintain ritual form amid generational drift and
 *   diaspora — the piton trajectory. The stable suppression requirement and
 *   modest extractiveness growth suggest the constraint is transitioning from
 *   natural binding (identity internalization in closed communities) to
 *   enforced performance (organizational maintenance in dispersed
 *   communities).
 *
 * KEY AGENTS:
 *   - Younger Generation Participants: Primary victims (powerless/identity_locked) — structurally mobile but psychologically bound through identity fusion; experience maximum extraction because exit requires identity dissolution
 *   - Religious Institution: Primary beneficiary (institutional/arbitrage) — preserves collective identity and institutional authority; experiences constraint as pure coordination and has full capacity to modify but chooses maintenance
 *   - Community Historians/Scholars: Secondary victims (moderate/constrained) — bear epistemic responsibility for accuracy while constrained by ritual fixity; benefits from custodian authority but costs of unresolved questions
 *   - Diaspora Organizing for Maintenance: Organized actors (organized/constrained) — face growing theatrical burden as functional binding capacity atrophies; performing ritual fixity with decreasing organic transmission
 *   - Academic Documentation Projects: Transitional support agents (organized/mobile) — building alternative memory preservation systems (archives, curricula, digital documentation) that could sunset the ritual obligation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choices as inherent properties of trauma response
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission_flat_control, 0.35).
domain_priors:suppression_score(catastrophe_memory_transmission_flat_control, 0.48).
domain_priors:theater_ratio(catastrophe_memory_transmission_flat_control, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission_flat_control, extractiveness, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_transmission_flat_control, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_transmission_flat_control, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission_flat_control, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission_flat_control, "Obligation to Transmit Catastrophe-Memory Through Fixed Ritual Practice").
narrative_ontology:topic_domain(catastrophe_memory_transmission_flat_control, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(catastrophe_memory_transmission_flat_control, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission_flat_control, institutional_religious_authority).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission_flat_control, community_identity_maintenance).
narrative_ontology:constraint_victim(catastrophe_memory_transmission_flat_control, younger_generation_participants).
narrative_ontology:constraint_victim(catastrophe_memory_transmission_flat_control, historical_accuracy_documentation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission_flat_control, community_historians_scholars).
narrative_ontology:constraint_victim(catastrophe_memory_transmission_flat_control, community_historians_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Socialized from infancy into the obligation to transmit catastrophe-memory through ritual practice. Participation is expected from childhood; refusal risks social exclusion and identity dissolution within the community. Structurally capable of leaving (geographic or organizational exit exists) but psychologically unable to leave because identity is constituted through the ritual role. Bears the cost of ritual participation — time, emotional labor, identity constraint — while the institutional benefit (authority, continuity) accrues elsewhere.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission_flat_control, younger_generation_participants, payer,
    powerless, biographical, identity_locked, local).

% Controls the form and mandate of ritual transmission. Benefits from the constraint through: (1) maintenance of institutional identity and continuity, (2) authority as custodian of sacred narrative, (3) mechanism for binding younger generation through identity fusion rather than voluntary association. Sets and enforces the obligation. Could modify ritual form or exit constraint entirely but chooses to maintain it because it serves institutional interests.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission_flat_control, institutional_religious_authority, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission_flat_control, institutional_religious_authority, beneficiary).

% Professionally and socially obligated to participate in ritual transmission while also bearing responsibility for historical accuracy and interpretation. Benefits from status as custodian and from participatory authority within ritual. Pays through constraint: cannot revise unresolved historical questions, cannot propose alternative interpretations, cannot correct factual inaccuracies without institutional resistance. Faces career and social cost for scholarly work that treats catastrophe as historical (resolvable) rather than sacred (unchanging).
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission_flat_control, community_historians_scholars, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission_flat_control, community_historians_scholars, beneficiary).

% Work to maintain ritual practice amid geographic dispersal and acculturation pressure. Bear increasing burden of active organizational maintenance as organic intergenerational transmission decays (children raised in diaspora have weaker identity-lock). Document ritual procedures, prescribe exact performance, discipline deviations, coordinate across scattered communities. Experience the constraint as increasingly theatrical — much organizational energy goes to maintaining form while functional transmission of catastrophe-memory fragments. Constrained by identity commitment and by institutional dependence on their organizational labor.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission_flat_control, diaspora_organizational_leaders, agenda_setter,
    organized, generational, constrained, global).

% Create parallel systems for catastrophe-memory preservation: museum exhibitions, archived testimonies, academic interpretation, digital documentation, educational curricula. See these systems as potential sunset for ritual-based transmission — as archival and academic memory systems mature, identity-locked ritual participation becomes optional rather than essential. Not embedded in the community (hence high mobility) and not directly benefiting from institutional authority. Observe the constraint but do not enforce it; document alternatives.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission_flat_control, academic_documentation_projects, observer,
    organized, biographical, mobile, global).

% Individuals who attempt to refuse ritual participation face community pressure, social exclusion, sometimes legal or economic consequences from family or institutional structures. Rarely visible in consensus narratives because refusal is disciplined into silence or exit. Their absence from the community weakens intergenerational transmission bonds, creating pressure for institutional enforcement of the obligation on remaining participants. Would object if heard, but the constraint includes suppression of alternative voices.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission_flat_control, younger_generation_refusers, excluded,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintain shared identity and historical consciousness across diasporic communities separated by geography, time, and acculturation. Communities scattered across generations and geographic distances need a stable, recognizable reference point to remain bounded as a coherent group and to preserve collective memory of the catastrophe that shaped their identity.
% TRANSFER_FUNCTION: The constraint moves identity burden and temporal obligation from institutional authority (which benefits from stable group identity and has designed the ritual) to younger generation participants (who bear the cost of participation, identity constraint, and psychological obligation). It also moves historical authority from documented scholarship to ritual custodianship: interpretive power flows to the institution rather than to scholars or participants.
% ABSENT_VOICES: Younger generation members who wish to refuse participation but are excluded from decision-making about the obligation. Scholars who recognize historical inaccuracies but are constrained from proposing revision. Diaspora members who experience ritual as disconnected from lived community. Families in extremely dispersed contexts (migration, exile) where ritual participation has become logistically or culturally impossible. These voices are absent because refusal is disciplined as betrayal and suppressed as invalid.
% DISAPPEARANCE_RATIONALE: If the obligation to transmit catastrophe-memory through fixed ritual practice disappeared overnight, the world would substantially rearrange: (1) Community identity would not automatically dissolve, but its form would shift away from ritual-centered cohesion; (2) Institutional religious authority would lose a primary mechanism for binding younger members and transmitting sacred narrative; (3) Younger generation members would face an identity gap (who am I without this obligation?) but would gain exit capacity and interpretive freedom; (4) Alternative memory preservation systems (archives, academic study, digital documentation, secular commemoration) would become more prominent; (5) Historical scholarship would likely revise narratives that ritual fixity has protected from critical examination. The constraint's disappearance would not return the world to pre-catastrophe innocence, but it would allow reorganization around voluntary association and empirically grounded interpretation rather than identity-locked obligation.
% FOUNDING_PROBLEM: In the aftermath of collective catastrophe (genocidal violence, ethnic cleansing, religious persecution, natural disaster with massive loss of life), survivor communities need mechanisms to ensure that the trauma-memory is transmitted to generations born after the catastrophe, so that historical consciousness is maintained and the catastrophe's meaning is not erased. The founding problem is preserving collective historical consciousness when direct memory-bearers (survivors) are aging or dying, and younger generations have no lived experience of the catastrophe.
% FOUNDING_PROBLEM_CORROBORATION: Institutional religious authorities and community elders attest that the founding problem remains live — catastrophe-memory is being lost as survivors age and younger generations assimilate into surrounding cultures. Academic historians and diaspora scholars attesting the same erosion of memory transmission. However, alternative attestation comes from academic documentation projects and museum professionals: they argue that scholarly preservation, digital archiving, and educational integration into standard curricula can transmit historical consciousness as effectively as ritual obligation. The founding problem's status depends on whether 'transmission of catastrophe-memory' means 'identity-locked lived experience of the ritual' (live, per institutional view) or 'accurate historical knowledge and cultural awareness' (being addressed by alternative systems, per academic view). This is the contested axis.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission_flat_control, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YOUNGER GENERATION PARTICIPANT (SNARE) — Structurally mobile (could refuse participation) but identity-fused with community membership through socialization from infancy. The obligation binds through internalized identity rather than material barriers. Exit would require abandoning the identity framework that constitutes personhood within the community. Maximum experienced extraction because the binding mechanism is psychological and identity-constitutive rather than material. The participant experiences the fixed ritual as immutable and non-negotiable.
constraint_indexing:constraint_classification(catastrophe_memory_transmission_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: COMMUNITY HISTORIAN/SCHOLAR (TANGLED ROPE) — Constrained by professional and social obligation to participate in ritual transmission while also bearing epistemic responsibility for historical accuracy. Faces genuine coordination problem (transmitting collective memory across generations) but also experiences extraction (ritual fixity prevents historiographic correction or evolution of interpretation). Benefits from the authority granted by custodian status but bears costs of carrying unresolved historical questions that cannot be addressed within the rigid framework. Moderate extraction — some agency and some benefit alongside genuine constraint.
constraint_indexing:constraint_classification(catastrophe_memory_transmission_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RELIGIOUS INSTITUTION (ROPE) — Experiences the obligation as pure coordination: the fixed ritual is the mechanism that preserves collective identity, transmits sacred history, and maintains institutional continuity. Extraction is minimal because the institution is the primary beneficiary and designer of the constraint. The ritual coordinativeness is genuine — without it, diaspora or generations separated by trauma would lose shared reference point. The institution has full arbitrage capacity (can modify the ritual, can exit if it chose to) and uses that power to maintain the constraint. Net beneficiary.
constraint_indexing:constraint_classification(catastrophe_memory_transmission_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the obligation to transmit catastrophe-memory through ritual practice may appear as an immutable consequence of human psychology and collective trauma response: humans transmit traumatic experience through repeated narratives and ritualized commemoration, and this appears as a structural fact of consciousness and social binding rather than as a constructed constraint. However, this classification risks false summitry: the fixity of the ritual is not inherent to trauma transmission generally, but rather a specific institutional choice that naturalizes itself as inevitable or sacred.
constraint_indexing:constraint_classification(catastrophe_memory_transmission_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: DIASPORA ORGANIZING FOR MAINTENANCE (PITON) — Organized agents working to preserve the ritual amid geographic dispersal and acculturation pressures experience the constraint as increasingly theatrical. The performative effort to maintain ritual fixity (recording ceremonies, prescribing exact procedures, disciplining deviations) has grown as the functional binding capacity (actual transmission of lived trauma-memory) has atrophied. The ritual persists through active organizational maintenance and identity enforcement rather than through organic intergenerational teaching. Theater ratio is high: much energy goes to preserving the form while the original function (actualizing catastrophe-memory in lived experience) fragmentizes.
constraint_indexing:constraint_classification(catastrophe_memory_transmission_flat_control, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ACADEMIC/MUSEUM DOCUMENTATION PROJECT (SCAFFOLD) — Contemporary agents creating parallel archives, museum exhibitions, and academic documentations of catastrophe-memory experience the obligation as a transitional coordination problem. These agents see the fixed ritual as temporary scaffolding while more resilient (and less identity-locked) mechanisms of memory transmission — archives, education curricula, digital preservation, scholarly interpretation — are being built. The sunset logic is structural: as documentation and academic interpretation mature, the burden of ritual transmission shifts to institutional memory systems that do not require identity-locked participation. Temporary support with a declared exit path.
constraint_indexing:constraint_classification(catastrophe_memory_transmission_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_memory_transmission_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_memory_transmission_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_memory_transmission_flat_control, TR),
    TR >= 0.70.

:- end_tests(catastrophe_memory_transmission_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint extracts from younger generation participants through identity lock and from historians through preventable inaccuracy, but extraction is not severe because the institution genuinely solves a coordination problem (transmitting collective memory across generations) and many participants experience identity benefits alongside costs. The moderate value reflects the hybrid nature of the constraint: real coordination function plus real extraction through fixity enforcement. Suppression (0.48): Moderate. Significant barriers to exit include identity internalization, social exclusion risk for those who refuse, and limited alternative frameworks for maintaining community identity. But suppression is not maximal because exit is physically possible (some individuals do leave) and because alternative documentation systems are developing that could reduce suppression. Theater ratio (0.58): Moderate-high. Increasing organizational effort to maintain ritual form amid generational drift and diaspora dispersion creates theatrical burden. The ritual's original function (transmitting lived trauma-memory through intergenerational teaching in closed communities) has atrophied as communities disperse; contemporary effort increasingly goes to preserving the form rather than enabling organic transmission. The interval measurement shows this trajectory clearly: theater rises as suppression requirement falls, indicating the shift from natural internalization to enforced performance.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap lies between the institutional perspective (Rope: pure coordination) and the younger participant perspective (Snare: pure extraction with identity-lock binding). The institution genuinely experiences the constraint as solving the coordination problem of dispersed communities maintaining shared identity. The younger participant experiences the obligation as identity-constituting and non-negotiable, with costs borne through psychological obligation rather than material force. The historian perspective (Tangled Rope) bridges this gap, naming both the real coordination function and the real extraction through fixity enforcement. The piton perspective (diaspora organizing) reveals that institutional maintenance of the constraint requires increasing organizational effort as organic binding capacity decays. The scaffold perspective (academic documentation) is currently aspirational rather than structural but points to a potential sunset logic where alternative memory preservation systems become sufficient. The analytical mountain perspective risks false summitry by naturalizing the institutional choice (ritual fixity) as an inevitable consequence of human trauma psychology.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from beneficiary/victim status and exit options. Younger generation participants classified as identity-locked have low structural mobility but high psychological entrenchment — they could theoretically exit (no material barriers) but cannot exit (identity-constitutive role). The engine derives d from victim status + identity-locked exit → d toward 0.75-0.85 (high target directionality). The religious institution classified as beneficiary with arbitrage options has d toward 0.05-0.15 (beneficiary directionality). Community historians with constrained exit and mixed beneficiary/victim status have d toward 0.45-0.55 (symmetric directionality). The dramatic difference between the powerless participant's high d and the institutional beneficiary's low d produces the perspectival gap: the same constraint feels extractive from the trapped position and coordinative from the beneficiary position. Effective extraction (χ) is amplified for identity-locked targets and damped for arbitrage-option beneficiaries, creating the measured differential experience.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The founding problem is clear (transmitting catastrophe-memory across generations after collective trauma) but its status is contested. For diaspora and academically connected participants, the founding problem is live and being addressed through alternative mechanisms (documentation, curricula, digital preservation). For institutionally embedded participants, the founding problem is reframed as ongoing identity maintenance rather than trauma-response: the catastrophe is historical but the obligation is eternal. This reframing is the mandatrophy risk — when the founding problem's status shifts from 'live trauma requiring transmission' to 'eternal identity obligation,' the constraint transitions from scaffold (temporary support during grief and diaspora) to permanent extraction mechanism. The measured rise in theater ratio (0.35 to 0.64) supports the mandatrophy hypothesis: as the founding problem becomes historical rather than lived, the constraint's performative burden increases to compensate for declining organic internalization. Resolution would require explicit institutional articulation of the founding problem's current status and a decision whether the constraint continues to serve that problem or has become institution-sustaining theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_fixity_necessity,
    'Is the fixity of the ritual form necessary to transmit catastrophe-memory across generations, or does fixity primarily serve institutional identity preservation?',
    'Comparative analysis of trauma-memory transmission across communities with rigid vs. flexible ritual structures; longitudinal measurement of memory retention and emotional impact as function of ritual flexibility; diaspora communities where ritual form has necessarily adapted',
    'If fixity is necessary: constraint is primarily Rope (genuine coordination). If fixity serves identity preservation: constraint is primarily Tangled Rope or Snare (extraction with coordination cover). This resolves the perspectival gap between the institution''s experience and the younger generation''s.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_fixity_necessity, empirical, 'Whether ritual fixity is necessary or optional for memory transmission').

omega_variable(
    identity_lock_mechanism,
    'Is the binding of younger generation participants primarily through identity fusion (cognitive/psychological) or through material barriers (social exclusion, economic dependency, legal restriction)?',
    'Study of participants who exit the ritual: do they retain community identity if they leave (suggesting identity-lock is not total), or does exit cascade to identity dissolution? Study of coercion mechanisms (what specifically enforces participation for those who wish to refuse)?',
    'If primarily identity-locked: classification is Snare with psychological binding. If primarily material: classification shifts to Snare with structural coercion. This affects both directionality and therapeutic/reform pathways.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Mechanism binding younger participants to ritual obligation').

omega_variable(
    catastrophe_specificity,
    'Does the obligation to transmit catastrophe-memory bind specifically to the historical catastrophe, or has the ritual become a mechanism for maintaining institutional authority independent of the original trauma''s relevance?',
    'Textual and interview analysis tracking what is actually transmitted (factual history vs. sacred narrative vs. obligation to perform); comparison of participants'' understanding of the catastrophe across generations; analysis of how reinterpretation attempts are disciplined',
    'If trauma-specific: constraint can sunset as historical distance grows (Scaffold logic). If institution-sustaining: constraint persists as Piton (atrophied function maintained theatrically). This resolves mandatrophy: is the founding problem (transmitting collective trauma) still live?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_specificity, empirical, 'Whether obligation remains tied to historical trauma or has become institutional maintenance').

omega_variable(
    generational_divergence,
    'Do successive generations experience the obligation as increasingly extractive (theater ratio rising), or does each generation internalize the identity-lock equally?',
    'Longitudinal measurement of theater_ratio and extractiveness across 3+ generations; tracking of ritual modification attempts and institutional discipline of deviations; survey of subjective experience of constraint burden across age cohorts',
    'Rising theater ratio suggests piton trajectory (atrophied function). Stable identity-lock suggests stable Snare or Tangled Rope. This determines whether scaffold (transitional support) is structural or aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_divergence, empirical, 'Whether generations experience increasing theatrical burden or stable internalization').

omega_variable(
    false_summit_natural_trauma,
    'Is the obligation to transmit catastrophe-memory through ritual a natural consequence of human trauma response (Mountain), or is ritual fixity a constructed institutional choice that naturalizes itself as inevitable?',
    'Comparative study of trauma transmission across cultures with and without fixed ritual obligation; analysis of how ritual form became codified and what institutional interests that codification served; evidence of deliberate suppression of alternative transmission mechanisms',
    'If genuinely natural: Mountain classification confirmed. If constructed: false summit detected — constraint reclassifies to Tangled Rope or Snare depending on beneficiary concentration. This determines whether the constraint is negotiable or requires external reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_trauma, conceptual, 'Whether ritual obligation is natural law or constructed constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission_flat_control, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catmem_tr_t0, catastrophe_memory_transmission_flat_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(catmem_tr_t25, catastrophe_memory_transmission_flat_control, theater_ratio, 25, 0.48).
narrative_ontology:measurement(catmem_tr_t50, catastrophe_memory_transmission_flat_control, theater_ratio, 50, 0.58).
narrative_ontology:measurement(catmem_tr_t75, catastrophe_memory_transmission_flat_control, theater_ratio, 75, 0.64).

% Extraction over time
narrative_ontology:measurement(catmem_be_t0, catastrophe_memory_transmission_flat_control, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(catmem_be_t25, catastrophe_memory_transmission_flat_control, base_extractiveness, 25, 0.32).
narrative_ontology:measurement(catmem_be_t50, catastrophe_memory_transmission_flat_control, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(catmem_be_t75, catastrophe_memory_transmission_flat_control, base_extractiveness, 75, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(catmem_su_t0, catastrophe_memory_transmission_flat_control, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(catmem_su_t25, catastrophe_memory_transmission_flat_control, suppression_requirement, 25, 0.5).
narrative_ontology:measurement(catmem_su_t50, catastrophe_memory_transmission_flat_control, suppression_requirement, 50, 0.48).
narrative_ontology:measurement(catmem_su_t75, catastrophe_memory_transmission_flat_control, suppression_requirement, 75, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission_flat_control, attachment_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_transmission_flat_control, diaspora_identity_maintenance).
narrative_ontology:affects_constraint(catastrophe_memory_transmission_flat_control, intergenerational_trauma_transmission).
narrative_ontology:affects_constraint(catastrophe_memory_transmission_flat_control, institutional_authority_legitimation).

% DUAL FORMULATION NOTE:
% This constraint occupies a critical junction between three distinct structural problems: (1) diaspora communities maintaining collective identity across geographic dispersal (coordination problem), (2) trauma survivors transmitting historical memory to those without lived experience (transmission problem), (3) religious institutions maintaining authority through custody of sacred narrative (institutional persistence problem). These are related but structurally distinct — different ε values depending on which problem is foregrounded. The flat construction treats the constraint as a hybrid (Tangled Rope) that attempts to solve all three simultaneously; a complete network analysis would decompose into separate stories with different beneficiary structures and different sunset logics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
