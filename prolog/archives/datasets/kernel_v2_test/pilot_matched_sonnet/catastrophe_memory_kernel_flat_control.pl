% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel_flat_control, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_kernel_flat_control
 *   human_readable: Ritual Catastrophe-Commemoration as Collective Memory Transmission
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Ritual catastrophe-commemoration — the practice of annually or cyclically
 *   re-enacting, reciting, or symbolically representing historical disasters,
 *   persecutions, or collective traumas — serves as a primary mechanism for
 *   transmitting collective memory across generations in diaspora
 *   communities. Examples include Passover seders commemorating Egyptian
 *   slavery and exodus, Tisha B'Av mourning the destruction of the Jerusalem
 *   temples, Ashura commemorating the martyrdom of Hussein, and Holocaust
 *   remembrance rituals. The constraint operates at civilizational
 *   timescales: some commemorative practices have persisted for over two
 *   millennia despite geographic dispersion, language shifts, and the absence
 *   of centralized enforcement. The ritual solves a genuine coordination
 *   problem — how to maintain shared historical consciousness when
 *   communities are scattered across continents and generations — but the
 *   solution's structure raises questions about memory fidelity, voluntary
 *   participation, and the naturalization of culturally contingent practices
 *   as cognitive universals. The constraint's low extractiveness (0.18)
 *   reflects that most participants experience net benefit: cultural
 *   continuity, community belonging, and connection to ancestral experience.
 *   The moderate theater ratio (0.35) reflects that some performative
 *   elements have accumulated over time (liturgical elaboration, symbolic
 *   objects whose original function is forgotten) but the core
 *   memory-transmission function remains intact. The measurements show
 *   gradual drift: theater and extraction both increase slightly over the
 *   two-millennium interval as institutional structures elaborate and ritual
 *   practice becomes more formalized, but the increases are modest — the
 *   constraint has remained substantially functional across its entire
 *   lifespan.
 *
 * KEY AGENTS:
 *   - Individual Participants: Primary beneficiaries (powerless/identity_locked) — receive cultural continuity and community belonging; identity-locked but not extracted from
 *   - Community Leadership: Coordinating agents (moderate/constrained) — bear custodial responsibility but benefit from community cohesion the ritual enables
 *   - Diaspora Network: Institutional beneficiaries (institutional/mobile) — synagogues, churches, mosques, temples, cultural organizations that coordinate memory transmission globally
 *   - Future Generations: Implicit beneficiaries (powerless/trapped in time) — receive transmitted memory but cannot participate in its construction
 *   - Assimilationist Reformers: Organized agents (organized/constrained) — see the ritual as temporary scaffolding meant to fade with integration
 *   - State Integration Apparatus: Institutional actors (institutional/constrained) — experience mixed coordination and extraction from minority commemorative practice
 *   - Cognitive Anthropologist: Analytical observer (analytical/analytical) — risks naturalizing culturally contingent practice as cognitive universal
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel_flat_control, 0.18).
domain_priors:suppression_score(catastrophe_memory_kernel_flat_control, 0.25).
domain_priors:theater_ratio(catastrophe_memory_kernel_flat_control, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel_flat_control, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_kernel_flat_control, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_kernel_flat_control, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel_flat_control, rope).
narrative_ontology:human_readable(catastrophe_memory_kernel_flat_control, "Ritual Catastrophe-Commemoration as Collective Memory Transmission").
narrative_ontology:topic_domain(catastrophe_memory_kernel_flat_control, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel_flat_control, 'bed37908-1ca3-47be-95a7-2b5f0abf32df').
narrative_ontology:cs_kernel_codification('bed37908-1ca3-47be-95a7-2b5f0abf32df', formalized).
narrative_ontology:cs_authority_grounding('bed37908-1ca3-47be-95a7-2b5f0abf32df', lineage).
narrative_ontology:cs_interpretation_layer_present('bed37908-1ca3-47be-95a7-2b5f0abf32df').
narrative_ontology:cs_created_at('bed37908-1ca3-47be-95a7-2b5f0abf32df', '').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(catastrophe_memory_kernel_flat_control, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel_flat_control, diaspora_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel_flat_control, future_generations).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel_flat_control, cultural_continuity_itself).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel_flat_control, individual_participant).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel_flat_control, community_leadership).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel_flat_control, diaspora_network).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel_flat_control, state_integration_apparatus).
narrative_ontology:constraint_victim(catastrophe_memory_kernel_flat_control, assimilationist_reformers).
narrative_ontology:constraint_victim(catastrophe_memory_kernel_flat_control, state_integration_apparatus).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel_flat_control, ritual_efficacy_for_memory_transmission).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel_flat_control, embodied_memory_persistence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participates in annual or cyclical ritual commemoration of ancestral catastrophe. Receives cultural continuity, community belonging, and connection to historical experience. Identity is partly constituted through the ritual practice — exit would require abandoning not just the practice but the identity framework it sustains. Experiences the ritual as coordination: it solves the problem of maintaining connection across displacement and time.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel_flat_control, individual_participant, beneficiary,
    powerless, biographical, identity_locked, local).

% Bears responsibility for accurate transmission of commemorative practice across generations. Sets ritual calendar, maintains liturgical texts, trains next generation in proper performance. Constrained by custodial duty but benefits from community cohesion and cultural authority the ritual enables. Exit would damage community continuity and personal standing.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel_flat_control, community_leadership, agenda_setter,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel_flat_control, community_leadership, beneficiary).

% Institutional infrastructure (synagogues, churches, mosques, temples, cultural organizations) that coordinates memory transmission across vast geographic and temporal scales. The ritual serves as coordination standard: shared practice enables mutual recognition and intelligibility across dispersed communities. Benefits from the coordination function itself — the ritual is what makes the network possible.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel_flat_control, diaspora_network, beneficiary,
    institutional, civilizational, mobile, global).

% Receive transmitted historical memory through ritual participation. Cannot participate in construction of the commemorative practice — inherit it as given. Trapped by temporal position: no exit from the memory they receive, no voice in how it is shaped.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel_flat_control, future_generations, beneficiary,
    powerless, generational, trapped, global).

% Organized agents seeking to modernize or streamline ritual practice. Experience catastrophe-commemoration as obstacle to integration: the ritual's emphasis on historical persecution reinforces distinct identity boundaries that complicate assimilation. See the practice as temporary scaffolding meant to fade as communities achieve security. Constrained by community ties and cultural loyalty from full exit.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel_flat_control, assimilationist_reformers, payer,
    organized, generational, constrained, national).

% State institutions managing minority integration. Benefit from community self-organization (the ritual maintains cohesion without state resources) but bear costs when commemorative practice reinforces parallel loyalty structures and complicates assimilation narratives. Must actively manage tension between pluralism and integration. Constrained by liberal democratic commitments from suppressing minority practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel_flat_control, state_integration_apparatus, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel_flat_control, state_integration_apparatus, beneficiary).

% Studies ritual memory transmission as cognitive and cultural phenomenon. Observes that embodied practice encodes information more durably than propositional knowledge and that synchronized group performance creates shared attentional frames resistant to forgetting. Risks naturalizing culturally contingent practice as cognitive universal — treating the specific ritual forms as inevitable rather than constructed.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel_flat_control, cognitive_anthropologist, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintaining shared historical consciousness across generations when geographic dispersion, language shifts, and absence of centralized authority would otherwise fragment collective memory within 3-4 generations.
% TRANSFER_FUNCTION: The ritual transfers cultural continuity, community belonging, and historical consciousness from older to younger generations. It also transfers social capital and cultural authority to those who maintain and transmit the practice (community leadership, institutional infrastructure).
% ABSENT_VOICES: Those who have exited the community or been expelled for non-participation. Individuals who experience the commemorated catastrophe as irrelevant to their present identity but face social costs for non-participation. Minority factions within the community whose interpretation of the catastrophe was not selected for ritual commemoration. These voices are absent from the ritual's construction but would contest its content or obligatory status if present.
% DISAPPEARANCE_RATIONALE: If the ritual disappeared, diaspora communities would lose their primary mechanism for transmitting historical memory across generations. Within 3-4 generations, shared historical consciousness would fragment into family-level oral traditions with high variance and low fidelity. Community cohesion would weaken as the shared commemorative practice that creates synchronous collective experience disappears. The diaspora network would lose its coordination standard — communities would drift apart in practice and interpretation. However, alternative memory technologies (historical education, museums, digital archives) could partially replace the ritual's function, so the rearrangement would not be total collapse but significant structural change.
% FOUNDING_PROBLEM: How to maintain collective memory of formative historical catastrophes across millennia of geographic dispersion, language shifts, and political fragmentation when written records alone are insufficient (most community members are illiterate or semi-literate for most of history) and when centralized institutional authority is absent or weak due to diaspora conditions.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive anthropologists and memory researchers corroborate that embodied ritual practice transmits information more durably than text alone, particularly across populations with variable literacy. Historians of diaspora communities corroborate that communities without ritual commemoration lose detailed historical memory within 3-4 generations despite written records. However, the founding problem's status as 'live' is contested by assimilationist reformers who argue that modern literacy, digital archives, and historical education have made ritual commemoration obsolete as a memory technology. The contestation is documented in denominational debates over ritual simplification and in generational surveys showing declining participation rates among younger cohorts with high educational attainment.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel_flat_control, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL PARTICIPANT (ROPE) — Identity-locked but experiences the ritual as coordination: the practice solves the genuine problem of maintaining connection to ancestral experience and community identity across displacement. Low extraction — the participant receives cultural continuity, shared meaning, and community belonging. The identity lock is not extractive capture but constitutive membership: exit would require abandoning the identity the ritual helps constitute.
constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: COMMUNITY LEADERSHIP (ROPE) — Constrained by responsibility to transmit accurately but experiences the ritual as coordination mechanism. The constraint solves the collective action problem of maintaining shared memory across generations when geographic dispersion would otherwise fragment the narrative. Moderate power and constrained exit reflect the weight of custodianship, but extraction is low — the leadership benefits from community cohesion and cultural authority that the ritual enables.
constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DIASPORA NETWORK (ROPE) — Mobile institutional actors (synagogues, churches, mosques, temples, cultural organizations) coordinate memory transmission across vast geographic and temporal scales. The ritual is a coordination standard: shared practice enables recognition and mutual intelligibility across dispersed communities. Low extraction — the network benefits from the coordination function itself. The ritual's persistence across millennia demonstrates genuine coordination value: if it were primarily extractive, exit would have occurred during periods of reduced enforcement capacity.
constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ASSIMILATIONIST REFORMERS (SCAFFOLD) — Organized agents seeking to modernize or streamline ritual practice see catastrophe-commemoration as temporary scaffolding: necessary during active persecution or displacement, but meant to fade as communities achieve security and integration. This perspective sees the ritual's function as transitional — maintaining identity during crisis, with an implicit sunset when the crisis ends. Constrained exit reflects the tension between reform impulse and community continuity.
constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE INTEGRATION APPARATUS (TANGLED ROPE) — State institutions managing minority integration experience the ritual as mixed coordination and extraction. Coordination: shared commemorative practice enables minority communities to maintain cohesion without state resources. Extraction: the ritual's emphasis on historical persecution can complicate assimilation narratives and create parallel loyalty structures. The state benefits from community self-organization but bears costs when commemorative practice reinforces distinct identity boundaries. Requires active management of the tension between pluralism and integration.
constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: COGNITIVE ANTHROPOLOGIST (MOUNTAIN) — From a civilizational analytical perspective, ritual repetition as memory transmission mechanism appears as a cognitive universal: embodied practice encodes information more durably than propositional knowledge alone, and synchronized group performance creates shared attentional frames that resist individual forgetting. The constraint looks like a natural law of human memory architecture — communities without ritual commemoration lose historical memory within 3-4 generations regardless of written records. However, this risks naturalizing what is actually a constructed coordination solution: the specific ritual forms, the selection of which catastrophes to commemorate, and the interpretive frames applied are all contingent choices, not cognitive inevitabilities.
constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_memory_kernel_flat_control, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(catastrophe_memory_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The ritual solves a genuine coordination problem — maintaining shared memory across diaspora — and most participants are net beneficiaries. The extraction that exists comes from: (1) opportunity cost of time spent in ritual rather than other activities, (2) social pressure to participate even when individual connection to the commemorated catastrophe is weak, (3) potential for ritual content to be shaped by factional interests rather than consensus. But these costs are modest compared to the coordination benefit. The value is low enough to support rope classification from most perspectives. Suppression (0.25): Low-moderate. Participation is largely voluntary in contemporary contexts, but community sanctions for non-participation can be significant (social exclusion, questioning of identity authenticity, family conflict). The suppression is higher for identity_locked participants who cannot exit without abandoning their self-concept, but even for them the binding mechanism is internal (identity fusion) rather than external coercion. The value reflects real but not severe barriers to exit. Theater ratio (0.35): Moderate. Some ritual elements are performative — liturgical elaborations whose original meaning is lost, symbolic objects maintained through tradition rather than function, commemorative narratives that have drifted from historical accuracy. But the core memory-transmission function remains: participants do acquire and transmit historical consciousness through the ritual, and the practice demonstrably works across millennia. The theater has increased gradually over time (from 0.20 to 0.35 across the interval) as institutional structures have elaborated, but the constraint has not crossed into piton territory — it is still substantially functional, not primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same ritual practice can be experienced as pure coordination (rope) by participants and diaspora institutions, as temporary scaffolding (scaffold) by reformers seeking integration, as mixed coordination-extraction (tangled rope) by state institutions managing pluralism, and as natural law (mountain) by analysts who mistake cultural construction for cognitive universal. The perspectival gap is not about disagreement over facts but about structural position: participants are inside the identity the ritual constitutes and experience it as coordination; reformers are at the boundary between tradition and modernity and experience it as transitional; state institutions are outside the community and experience the ritual's identity-boundary-reinforcing function as costly; analysts are at civilizational distance and risk abstracting away the contingent choices embedded in the practice. The gap reveals that 'coordination vs extraction' is not an intrinsic property of the ritual but a function of where you stand relative to it.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual participants are identity_locked but are primary beneficiaries — they receive cultural continuity, community belonging, and connection to ancestral experience. The identity lock is constitutive (the ritual helps form the identity) rather than extractive (the ritual exploits a pre-existing identity). This produces low directionality toward the constraint and low or negative effective extraction. Community leadership is constrained by custodial responsibility but also benefits from the community cohesion and cultural authority the ritual enables — moderate directionality, low effective extraction. The diaspora network is mobile and benefits from the coordination standard the ritual provides — low directionality, negative effective extraction (subsidy). Assimilationist reformers are constrained and see themselves as bearing costs (the ritual impedes integration) — higher directionality, moderate effective extraction. The state integration apparatus is institutional and constrained, experiencing mixed benefits (community self-organization) and costs (parallel loyalty structures) — moderate directionality, moderate effective extraction. The analytical observer is analytical and risks seeing a false summit — naturalizing the practice as a cognitive universal when it is actually a culturally contingent coordination solution.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that low extraction does not imply mountain classification. The ritual is genuinely coordinative (low extraction, substantial benefit to participants, solves real collective action problem) but is not a natural law — it is a constructed solution that could be replaced by alternative memory technologies (digital archives, historical education, museum practice). The analytical mountain perspective is a false summit: it naturalizes 'ritual repetition as memory mechanism' as a cognitive universal when the specific ritual forms, catastrophe selection, and interpretive frames are all contingent. The rope classification from participant perspectives is correct: they experience genuine coordination. The scaffold classification from reformers is also correct from their structural position: they see the ritual as transitional. The tangled rope from state institutions reflects real mixed coordination-extraction. All classifications are legitimate perspectival readings. The mandatrophy is resolved by recognizing that the constraint is coordinative (not extractive) but constructed (not natural) — a rope, not a mountain, despite its civilizational persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    memory_fidelity_vs_adaptation,
    'Does the ritual transmit accurate historical memory or does it adaptively reconstruct the past to serve present community needs?',
    'Comparison of ritual narratives across time periods and geographic locations with contemporaneous historical records; analysis of how commemorative content shifts in response to present circumstances',
    'If high fidelity: the ritual is primarily a coordination mechanism for preserving truth. If adaptive reconstruction: the ritual is partly an identity-maintenance mechanism that rewrites history, raising extraction concerns for historical accuracy as a collective good.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memory_fidelity_vs_adaptation, empirical, 'Whether ritual memory transmission preserves or reconstructs historical content').

omega_variable(
    voluntary_vs_coerced_participation,
    'Is participation in catastrophe-commemoration rituals genuinely voluntary or is it enforced through community sanctions and identity policing?',
    'Ethnographic observation of exit costs; interviews with non-participants about social consequences; analysis of community discourse about ritual obligation vs. choice',
    'If voluntary: rope classification holds across perspectives. If coerced: identity_locked exit options mask suppression, and the constraint reclassifies toward snare for those who would exit but cannot without community expulsion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_vs_coerced_participation, empirical, 'Whether participation is voluntary or community-enforced').

omega_variable(
    cognitive_universal_vs_cultural_construction,
    'Is ritual repetition as memory mechanism a cognitive universal (mountain) or a culturally constructed solution that could be replaced by alternative memory technologies?',
    'Cross-cultural comparison of memory transmission success rates: ritual-based vs. text-based vs. digital-archive-based systems; cognitive neuroscience of embodied vs. propositional memory encoding',
    'If cognitive universal: mountain classification from analytical perspective is correct. If cultural construction: the analytical mountain is a false summit — naturalizing a contingent practice as inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_universal_vs_cultural_construction, empirical, 'Whether ritual memory transmission is cognitively necessary or culturally contingent').

omega_variable(
    which_catastrophes_get_commemorated,
    'What determines which historical catastrophes receive ritual commemoration and which are forgotten? Is the selection process itself extractive?',
    'Historical analysis of catastrophe selection: which events entered the ritual calendar and which did not; correlation with power structures within the community at the time of selection',
    'If selection is consensus-based and reflects genuine collective trauma: coordination function is primary. If selection reflects factional power or serves to legitimate current authority structures: extraction is embedded in the ritual''s content, not just its practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(which_catastrophes_get_commemorated, conceptual, 'Whether catastrophe selection for commemoration is consensus or power-driven').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel_flat_control, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catmem_theater_ancient, catastrophe_memory_kernel_flat_control, theater_ratio, 0, 0.2).
narrative_ontology:measurement(catmem_theater_medieval, catastrophe_memory_kernel_flat_control, theater_ratio, 500, 0.25).
narrative_ontology:measurement(catmem_theater_early_modern, catastrophe_memory_kernel_flat_control, theater_ratio, 1000, 0.3).
narrative_ontology:measurement(catmem_theater_modern, catastrophe_memory_kernel_flat_control, theater_ratio, 1500, 0.32).
narrative_ontology:measurement(catmem_theater_contemporary, catastrophe_memory_kernel_flat_control, theater_ratio, 1800, 0.35).

% Extraction over time
narrative_ontology:measurement(catmem_extract_ancient, catastrophe_memory_kernel_flat_control, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(catmem_extract_medieval, catastrophe_memory_kernel_flat_control, base_extractiveness, 500, 0.14).
narrative_ontology:measurement(catmem_extract_early_modern, catastrophe_memory_kernel_flat_control, base_extractiveness, 1000, 0.16).
narrative_ontology:measurement(catmem_extract_modern, catastrophe_memory_kernel_flat_control, base_extractiveness, 1500, 0.17).
narrative_ontology:measurement(catmem_extract_contemporary, catastrophe_memory_kernel_flat_control, base_extractiveness, 1800, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel_flat_control, identity_coordination).

% DUAL FORMULATION NOTE:
% This is a flat construction of the catastrophe-commemoration substrate. Specific instantiations (Passover, Tisha B'Av, Ashura, Holocaust remembrance) would be separate constraint stories with their own extractiveness values reflecting the specific historical and institutional contexts. This story models the general mechanism at the level of 'ritual catastrophe-commemoration as memory transmission technology' rather than any particular tradition's implementation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
