% ============================================================================
% CONSTRAINT STORY: boundary_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_boundary_maintenance_reading, []).

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
 *   constraint_id: boundary_maintenance_reading
 *   human_readable: Ritual Boundary Maintenance Through Shared Mourning Practice
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   In this reading, mourning ritual enforces group boundaries by making
 *   conformity to prescribed grief-expression a marker of in-group
 *   membership. The constraint operates by linking emotional authenticity
 *   (genuine grief) to performative conformity (specific ritual forms), then
 *   using deviation from prescribed forms as grounds for status penalty and
 *   exclusion. The reading frames the extractive mechanism as
 *   identity-fusion: mourners internalize the ritual as the only legitimate
 *   way to process their loss, so deviation becomes unthinkable from within
 *   the identity frame rather than merely costly. The structural beneficiary
 *   is the in-group cohesion sustained through boundary enforcement; the
 *   victims are individual autonomy (suppressed grief-expression styles) and
 *   out-group relations (those outside the ritual community are positioned as
 *   insufficiently grieving or inauthentic). Over the measured interval
 *   (generational horizon), suppression has intensified as diaspora
 *   communities formalize and politicize mourning practice, raising stakes
 *   for deviation. Theater ratio has risen as mourning becomes a public
 *   performance of group identity (especially in diaspora contexts) rather
 *   than a private grief-processing act. This reading is distinct from
 *   alternative readings of the same kernel: the symbol-continuity reading
 *   would emphasize what cultural meanings the ritual preserves through
 *   repetition (not boundary enforcement); the trauma-encoding reading would
 *   emphasize how the ritual transmits collective memory across generations
 *   (encoding vs. boundary); the survival-competence reading would emphasize
 *   what coping skills mourners develop (function vs. boundary). This reading
 *   isolates the extractive boundary-maintenance frame.
 *
 * KEY AGENTS:
 *   - Bound Mourner (powerless/identity_locked): Individual whose identity is constituted through ritual participation; experiences maximum extraction through suppression but perceives the constraint as natural/necessary
 *   - Heterodox Mourner (moderate/constrained): Member whose grief-expression deviates from prescribed forms; bears sanctions and reputational cost; partially benefits from in-group coordination but constrained by conformity pressure
 *   - Religious Authority (institutional/arbitrage): Ritual administrators (clergy, elders, tradition-keepers); benefit from boundary definition and legitimacy to enforce mourning norms; see constraint as pure coordination
 *   - Diaspora Member (powerful/mobile): Geographically or culturally distant member; mourning ritual offers connection to ancestral community but with reduced local enforcement cost
 *   - Secular Ritualizer (institutional/arbitrage): Third-party observer or participant without identity attachment; performs ritual without being bound by it; theater highest for this agent
 *   - In-Group Cohesion (abstract/beneficiary): The abstract good of group continuity and solidarity that the boundary-enforcement mechanism sustains
 *   - Individual Autonomy (abstract/victim): The abstract cost of suppressed emotional expression and constrained grief-processing styles
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(boundary_maintenance_reading, 0.48).
domain_priors:suppression_score(boundary_maintenance_reading, 0.62).
domain_priors:theater_ratio(boundary_maintenance_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(boundary_maintenance_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(boundary_maintenance_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(boundary_maintenance_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(boundary_maintenance_reading, "Ritual Boundary Maintenance Through Shared Mourning Practice").
narrative_ontology:topic_domain(boundary_maintenance_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(boundary_maintenance_reading, '02d9dd96-1f87-415d-9d03-6edfdb95c22f').
narrative_ontology:cs_kernel_codification('02d9dd96-1f87-415d-9d03-6edfdb95c22f', distributed).
narrative_ontology:cs_authority_grounding('02d9dd96-1f87-415d-9d03-6edfdb95c22f', lineage).
narrative_ontology:cs_interpretation_layer_present('02d9dd96-1f87-415d-9d03-6edfdb95c22f').
narrative_ontology:cs_reading_relation('02d9dd96-1f87-415d-9d03-6edfdb95c22f', boundary_maintenance_reading__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('02d9dd96-1f87-415d-9d03-6edfdb95c22f', boundary_maintenance_reading__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('02d9dd96-1f87-415d-9d03-6edfdb95c22f', boundary_maintenance_reading__survival_competence_reading, coexists_with).
narrative_ontology:cs_axiom('02d9dd96-1f87-415d-9d03-6edfdb95c22f', foundational, boundary_maintenance_is_primary_function).
narrative_ontology:cs_axiom_status(boundary_maintenance_is_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('02d9dd96-1f87-415d-9d03-6edfdb95c22f', boundary_maintenance_is_primary_function, deontological).
narrative_ontology:cs_axiom('02d9dd96-1f87-415d-9d03-6edfdb95c22f', foundational, conformity_produces_membership).
narrative_ontology:cs_axiom_status(conformity_produces_membership, holdable).
narrative_ontology:cs_axiom_grounding('02d9dd96-1f87-415d-9d03-6edfdb95c22f', conformity_produces_membership, conventional).
narrative_ontology:cs_reference_frame('02d9dd96-1f87-415d-9d03-6edfdb95c22f', synchronized_collective_grief_processing).
narrative_ontology:cs_drift_state('02d9dd96-1f87-415d-9d03-6edfdb95c22f', contemporary, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('02d9dd96-1f87-415d-9d03-6edfdb95c22f', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(boundary_maintenance_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(boundary_maintenance_reading, in_group_cohesion).
narrative_ontology:constraint_beneficiary(boundary_maintenance_reading, religious_authority_structure).
narrative_ontology:constraint_victim(boundary_maintenance_reading, individual_autonomy).
narrative_ontology:constraint_victim(boundary_maintenance_reading, out_group_relations).
narrative_ontology:constraint_victim(boundary_maintenance_reading, heterodox_mourners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(boundary_maintenance_reading, heterodox_mourner).
narrative_ontology:constraint_beneficiary(boundary_maintenance_reading, religious_authority).
narrative_ontology:constraint_beneficiary(boundary_maintenance_reading, diaspora_member).
narrative_ontology:constraint_victim(boundary_maintenance_reading, bound_mourner).
narrative_ontology:constraint_victim(boundary_maintenance_reading, heterodox_mourner).
narrative_ontology:constraint_victim(boundary_maintenance_reading, diaspora_member).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Member of the in-group who experiences the loss directly (death, displacement, catastrophe) and participates in mourning ritual. Identity is constituted through ritual participation — they cannot imagine themselves outside the prescribed grief-processing form. Exit from the ritual would require abandoning their self-concept as a grieving member of the community. Grief conformity is not experienced as conformity but as the only authentic way to mourn.
narrative_ontology:constraint_stakeholder(boundary_maintenance_reading, bound_mourner, payer,
    powerless, biographical, identity_locked, local).

% Community member whose grief-expression style or mourning practice deviates from prescribed forms. May grieve more privately, express emotion differently, or engage alternative commemorative practices. Bears informal sanctions (status loss, exclusion from organizing roles, reputation damage) for deviation. Simultaneously benefits from the in-group's coordination around shared loss — the community validates their grief and provides social support. Constrained: could deviate at substantial cost but finds the in-group's coordinated response valuable enough to partially conform.
narrative_ontology:constraint_stakeholder(boundary_maintenance_reading, heterodox_mourner, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(boundary_maintenance_reading, heterodox_mourner, beneficiary).

% Clergy, elders, tradition-keepers, and ritual administrators who define, legitimize, and enforce mourning norms. They possess the authority to determine which grief-expressions count as authentic and which count as deviation. Benefits from this authority — their role as arbiter of legitimate grief is a source of social influence and institutional legitimacy. Can exit the constraint through various arbitrage routes: delegating to other administrators, modifying ritual forms, or accepting alternative mourning styles as equally legitimate.
narrative_ontology:constraint_stakeholder(boundary_maintenance_reading, religious_authority, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(boundary_maintenance_reading, religious_authority, beneficiary).

% In-group member living geographically or culturally distant from the primary community. Mourning ritual offers connection to ancestral community and reinforcement of diasporic identity — coordinates grief processing with remote family and affirms group continuity. However, experiences lower enforcement cost than local members because deviance carries reduced reputational penalty (remote community has limited enforcement capacity). Can choose to participate deeply or instrumentally depending on identity goals.
narrative_ontology:constraint_stakeholder(boundary_maintenance_reading, diaspora_member, payer,
    powerful, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(boundary_maintenance_reading, diaspora_member, beneficiary).

% Academic researcher, cultural tourist, interfaith dialogue participant, or other third party who observes or participates in mourning ritual without identity attachment to the in-group. May perform ritual actions and conform to procedural requirements but is not bound by the identity-lock mechanism. Experiences the constraint as theater or performance — goes through prescribed motions to show respect or gather data without the emotional stakes that bind in-group members. Has complete exit options (can stop observing at any time).
narrative_ontology:constraint_stakeholder(boundary_maintenance_reading, secular_ritualizer, observer,
    institutional, civilizational, arbitrage, global).

% The abstract collective good of shared identity, group continuity, and coordinated response to catastrophe. Mourning ritual sustains in-group bonds by creating synchronized grief-processing and affirming membership through shared practice. The constraint's coordination function produces this benefit — it is not a person or organization but a structural outcome.
narrative_ontology:constraint_stakeholder(boundary_maintenance_reading, in_group_cohesion, beneficiary,
    moderate, generational, analytical, regional).
narrative_ontology:stakeholder_non_agent(boundary_maintenance_reading, in_group_cohesion).

% The abstract individual capacity for self-directed grief-processing and non-conformist emotion expression. The constraint suppresses alternative mourning styles through identity-lock and social sanction, reducing the individual's autonomy to grieve in ways that deviate from prescribed forms. Mourners cannot access their own grief-processing preferences if those preferences are outside the ritual frame.
narrative_ontology:constraint_stakeholder(boundary_maintenance_reading, individual_autonomy, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_non_agent(boundary_maintenance_reading, individual_autonomy).

% The capacity for in-group members to form emotional and relational bridges with outsiders and with communities that grieve differently. The boundary-maintenance function suppresses out-group relations by reinforcing in-group/out-group distinctions through mourning-practice conformity. Out-group members are implicitly marked as insufficiently grieving or inauthentic — creating relational distance and reducing cross-community grief-work.
narrative_ontology:constraint_stakeholder(boundary_maintenance_reading, out_group_relations, payer,
    powerless, generational, trapped, continental).
narrative_ontology:stakeholder_non_agent(boundary_maintenance_reading, out_group_relations).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Process collective loss and maintain group identity continuity after catastrophe through synchronized ritual practice that affirms membership and validates grief.
% TRANSFER_FUNCTION: Mourners transfer conformity (emotional expression labor, time, participation compliance) to the in-group collective. In return, the collective transfers validation (grief legitimation, social support, membership affirmation, identity continuity). The boundary-enforcement layer intercepts part of this: deviation from prescribed forms transfers status and relational access to the authority structure that enforces norms.
% ABSENT_VOICES: Out-group members or alternative grief-processing communities are systematically absent from the ritual frame. Those who grieve outside the prescribed forms are not in the room making the case for heterodox mourning. Bound mourners, locked in the identity frame, cannot articulate the cost of autonomy suppression. The primary absent voice is the individual mourner's own suppressed grief-processing preferences.
% DISAPPEARANCE_RATIONALE: From the in-group perspective, if the mourning ritual disappeared, community continuity would be threatened — the shared practice that affirms membership and validates grief would vanish, and members would experience disorientation and isolation. Group identity would fragment. From the individual autonomy perspective, if the constraint disappeared, people would grieve according to their own preferences, potentially with deeper emotional processing and faster return to functioning. From the scholarly perspective, if the constraint disappeared, communities would need to find alternative mechanisms for processing collective loss — rituals might simplify, therapy/counseling might scale up, or grief might become more privatized. The world would rearrange, but the rearrangement might not be harmful and could reduce suppression costs.
% FOUNDING_PROBLEM: After a collective catastrophe (death, displacement, cultural destruction), communities face the coordinating problem of how to process loss together, affirm group continuity, and prevent social fragmentation during vulnerability. Mourning ritual emerged as a solution to synchronize grief-processing and create shared meaning-making.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by ethnographic research documenting that communities without established mourning practices experience greater grief-related mental health impacts and slower collective recovery (Rosenblatt 2000, Klass et al. 1996). Also corroborated by evolutionary psychology arguments that synchronized grief processing enhances group cohesion and resource-sharing during crisis (Wilson 2002). However, this corroboration comes primarily from researchers who benefit from the academic framing of mourning-as-functional. Corroboration from heterodox mourners themselves would require including perspectives of those who grieve outside prescribed forms — such perspectives are systematically rare in the literature because heterodox grievers are often marginalized or silent.
narrative_ontology:disappearance_verdict(boundary_maintenance_reading, contested).
narrative_ontology:founding_problem_status(boundary_maintenance_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individual member locked into mourning ritual by identity fusion with the community. Structurally able to opt out but identity is constituted through ritual participation — exit requires abandoning membership and self-concept simultaneously. Experiences maximum suppression: grief becomes the mechanism for enforcing conformity. Cannot perceive the extraction from within the frame because mourning is naturalized as the only appropriate response.
constraint_indexing:constraint_classification(boundary_maintenance_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% Member whose mourning practices deviate from prescribed forms (different duration, emotional expression, commemoration method). Coordinated WITH the in-group through shared loss but extracted FROM through enforced conformity. Experiences suppression: informal sanctions, reputation damage, exclusion from inner circles. Can see the extraction from biographical horizon but constrained by generational expectations and relational cost of deviation.
constraint_indexing:constraint_classification(boundary_maintenance_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Ritual administrators (clergy, elders, tradition-keepers) benefit from boundary maintenance: mourning practice legitimizes their authority to define legitimate grief, certify proper observance, and adjudicate who belongs. They see the constraint as pure coordination — solving the problem of how communities process collective loss and maintain identity continuity. Net beneficiary with exit options: can modify practice, delegate administration, or exit to other leadership roles.
constraint_indexing:constraint_classification(boundary_maintenance_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% Member living geographically or culturally distant from the in-group. Mourning ritual represents both coordination (connection to ancestral community) and extraction (conformity pressure despite reduced social cost of deviation). Mobile exit options create lower experienced extraction than locally trapped members. Generational horizon: ritual is valuable for intergenerational continuity but optional for local peer status.
constraint_indexing:constraint_classification(boundary_maintenance_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% Third parties (academic anthropologists, cultural tourists, interfaith dialogue participants) who participate in mourning rituals without identity attachment or generational obligation. Theater ratio highest here: ritual becomes performance of cultural authenticity or academic observation. The participant goes through prescribed motions but the binding mechanism (identity fusion + threat of exclusion) does not apply. Institutional power to describe and frame the ritual without being bound by it.
constraint_indexing:constraint_classification(boundary_maintenance_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Civilizational analytical reading: collective grief processing and in-group boundary maintenance are immutable features of human social organization. Mourning rituals are not contingent institutional arrangements but necessary functions of how communities persist and process loss. All societies have them; all humans participate in them; the boundary-enforcement function is inseparable from the grief-processing function. From this perspective, extraction is not extraction — it is the necessary cost of collective memory and social continuity. Engine computes FALSE SUMMIT: natural law claim obscures the contingent institutional design choices (which mourning forms count as legitimate, who enforces compliance, what happens to deviants) that generate the actual extractiveness.
constraint_indexing:constraint_classification(boundary_maintenance_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(boundary_maintenance_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(boundary_maintenance_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(boundary_maintenance_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(boundary_maintenance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(boundary_maintenance_reading, TR),
    TR >= 0.70.

:- end_tests(boundary_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate. The constraint extracts conformity from mourners through identity-fusion rather than pure material coercion, but the extraction is substantial — deviation incurs reputational cost, social exclusion, and identity crisis. The value reflects that the binding mechanism (identity-lock) is internalized rather than externally enforced, yet the suppression of alternative grief-processing styles is real. Over the interval, extractiveness has risen as diaspora communities formalize mourning practice and make it a political/cultural marker — grief becomes inseparable from group identity assertion. Suppression (0.62): Moderate-high and rising. Barriers to deviation include social sanctions (status loss, exclusion from inner circles), emotional pressure (guilt and shame for insufficient conformity), and identity threat (grief deviation risks membership). The identity-lock mechanism means the suppression is partly internalized — mourners enforce the norm on themselves — which may understate the structural coercion but does reflect how the constraint operates. Rising trajectory reflects enforcement intensification in diaspora contexts where mourning practice becomes a highly visible marker of group loyalty. Theater ratio (0.58, rising): Moderate and increasing. Mourning ritual involves genuine grief-processing (authentic function) but increasingly also performs group-identity and boundary-assertion, especially in contexts where the in-group's survival or legitimacy is contested. The theater ratio rise reflects the increasing performative load on mourning ritual as it becomes a political/cultural statement rather than purely a personal grief mechanism. In early/stable community contexts, theater ratio would be lower; in diaspora or post-catastrophe contexts where group identity is under pressure, theater ratio rises as mourning becomes a stage for asserting community continuity.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer risks naturalizing a contingent institutional arrangement (mourning-as-boundary-enforcement) into an immutable law of human social organization. From the civilizational perspective, all societies have mourning rituals and all have in-group/out-group boundaries — so the reading seems to describe a structural necessity. But the bound mourner's identity-lock perspective and the heterodox mourner's constrained perspective both reveal that the specific ways mourning enforces boundaries are choices, not laws. Different communities enforce boundaries through different ritual forms; some communities have mourning without strong boundary-enforcement; some have boundary enforcement without ritualized mourning. The analytical observer's mountain classification is a false summit — it naturalizes what is actually a contingent institutional architecture (which grief-expressions count as legitimate, who enforces compliance, what happens to deviants). The beneficiary's rope perspective (religious authority seeing pure coordination) and the bound mourner's snare perspective (experiencing identity-locked suppression) diverge because the beneficiary collects from the boundary enforcement while the mourner bears the identity-lock cost. The heterodox mourner's tangled-rope perspective splits the difference: partly benefiting from in-group coordination but partly paying the cost of conformity pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural position relative to the extraction flow. Bound mourner: powerless/identity_locked/local — zero material exit options, identity fused with compliance, maximum experienced extraction despite low measured suppression (identity-lock internalizes the constraint). Heterodox mourner: moderate/constrained/regional — has exit options (could grieve differently) but faces reputational costs; moderate experienced extraction. Religious authority: institutional/arbitrage/regional — benefits from boundary-definition authority, low experienced extraction (negative χ), net subsidy. Diaspora member: powerful/mobile/continental — geographic and cultural distance reduce enforcement capacity, lower experienced extraction than local members despite same baseline suppression. Secular ritualizer: institutional/arbitrage/global — performs ritual without identity attachment, theater highest, experienced extraction near zero (performs conformity without being bound by it). Beneficiary/victim declarations feed the derivation: in-group cohesion is the beneficiary (receives coordination function + boundary value); individual autonomy and out-group relations are victims (bear suppression costs). Religious authority is declared as beneficiary because they collect legitimate authority from boundary-definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint has not resolved its mandatrophy because the original mandate (process collective grief; maintain group continuity after loss) is still live and functional. Mourning rituals DO facilitate grief-processing and DO sustain in-group bonds. But a secondary function (boundary enforcement through conformity pressure) has been layered onto the primary mandate, and the secondary function now generates extraction that may outweigh the primary function's coordination value. Mandatrophy would resolve if the primary mandate (grief-processing) became obsolete but the constraint persisted through inertia. The measurement trajectory shows rising suppression_requirement and theater_ratio but stable extractiveness — suggesting the constraint has NOT yet atrophied (it still solves a real coordination problem) but IS increasingly performative and extractive as it becomes a political/cultural tool. This prevents piton classification despite rising theater. The reading sustains tangled_rope (genuine coordination + extraction hybrid) rather than piton (atrophied function maintained theatrically).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_maintenance_versus_grief_processing,
    'Is the extractive suppression (conformity pressure, deviation sanctions) structurally necessary for collective grief processing, or is it an institutional choice layered onto the grief function?',
    'Comparative analysis of mourning practices across communities with identical loss but different boundary-enforcement mechanisms. Identify whether grief-processing effectiveness (measured by reduced depression, PTSD, community cohesion) correlates with suppression level or remains independent. Ethnographic analysis of communities that have decoupled grief ritual from boundary enforcement (secular memorial services, religious communities with heterodox mourning options).',
    'If correlated: grief processing requires boundary suppression (extraction is functional cost, not rent). Constrains classification toward Rope. If independent: boundary enforcement is rent-seeking overlay on grief processing. Sustains Tangled Rope or Snare classification. Resolves the false summit reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_maintenance_versus_grief_processing, empirical, 'Structural necessity of boundary enforcement for grief processing').

omega_variable(
    alternative_reading_scope,
    'This reading instantiates the boundary-maintenance frame. But is the same observational data equally consistent with the symbol-continuity reading (mourning preserves cultural codes) or the trauma-encoding reading (ritual encodes and transmits trauma memory)? What would falsify one reading and favor another?',
    'Historical analysis of ritual drift: when communities change mourning forms (duration, emotional expressions, participant requirements), does the new form preserve in-group boundaries equally well (boundary-maintenance hypothesis) or does it primarily preserve cultural symbols/trauma encoding despite boundary shift? Track community responses to modernized or simplified mourning rituals.',
    'If simplified rituals maintain boundaries equally: boundary maintenance is NOT the primary function (sibling reading forecloses this one). If simplified rituals weaken boundaries: boundary maintenance is primary (this reading remains live). Determines whether this reading coexists with or forecloses the symbol-continuity reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_scope, empirical, 'Whether mourning function is boundary-maintenance or symbol-continuity').

omega_variable(
    identity_lock_mechanism_scope,
    'The bound mourner uses identity_locked exit options, indicating cognitive/identity capture rather than material barriers. But is this identity-lock universal to all mourners or concentrated in specific sub-populations (women, children, first-generation diaspora)? If concentrated, is the constraint still boundary-maintenance or is it a control mechanism targeting specific bodies?',
    'Demographic breakdown of who experiences identity-locked versus constrained versus mobile exit options within the same ritual. Correlate exit options with power/status/generational position. If identity-lock is uniform across all demographics: the mechanism is genuine cognitive identity-fusion. If concentrated on lower-power demographics: the mechanism is selective suppression masquerading as universal identity.',
    'If uniform: identity lock is the binding mechanism for all. Sustains Tangled Rope from all positions. If concentrated: concentrated identity-lock on lower-power groups reveals the constraint as extractive control (shifts toward Snare from those demographics). The reading''s claim about ''shared mourning'' may naturalize power-differentiated suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_scope, empirical, 'Whether identity-lock is universal or power-concentrated in mourning ritual').

omega_variable(
    kernel_reading_alternative_framings,
    'This story instantiates ONE reading of the catastrophe_memory_kernel: the reading that frames mourning-ritual-function as boundary-maintenance. But the same kernel permits the symbol-continuity reading (mourning preserves cultural codes through repetition), the trauma-encoding reading (mourning encodes and transmits trauma narrative), and the survival-competence reading (mourning develops grief-coping skills). These readings have different beneficiaries, different suppression mechanisms, and different victim classes. Which reading is ''correct'' — or do they coexist as live positions held by different parties and different scholarly traditions?',
    'Ethnographic multi-sited analysis documenting what participants and practitioners SAY the mourning ritual does (boundary maintenance, symbol preservation, trauma encoding, skill development). Compare emic frames (insider accounts) to etic analysis (structural function). If different communities/traditions emphasize different functions consistently, the readings coexist. If one function is universally named but the others are post-hoc scholarly additions, hierarchy among readings emerges.',
    'If coexistent: all four readings remain live; kernel admits multiple valid framings (coexists_with relations to siblings). If hierarchical: this reading may foreclose others or be foreclosed. Determines the reading_relations structure in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framings, conceptual, 'Ontological status of boundary-maintenance versus alternative kernel readings').

omega_variable(
    ritual_sunset_versus_boundary_permanence,
    'The reading frames mourning-ritual as boundary-maintenance. But boundaries need not be permanent. Can mourning ritual have a sunset — a defined period after which the in-group boundary relaxes or the ritual''s boundary-enforcing function is suspended — without the ritual ceasing to exist? Or is boundary maintenance inherently perpetual, making the ritual''s function incompatible with sunset logic?',
    'Historical analysis of mourning practices that have defined endpoints (shiva period, 40-day cycle, annual commemorations). Track whether the boundary-maintenance function persists after the prescribed mourning period ends or whether boundary enforcement requires continuous ritual repetition. Examine communities that have transitioned from perpetual mourning to bounded commemorative practices.',
    'If boundaries can persist without continuous ritual: mourning has decomposable functions (grief-processing ≠ boundary-maintenance) and could be Scaffold (boundary-maintenance function with sunset). If boundaries require continuous ritual repetition: this reading sustains Tangled Rope or Snare indefinitely. Determines whether the constraint is temporary support or perpetual extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_sunset_versus_boundary_permanence, empirical, 'Whether boundary-maintenance requires perpetual ritual or admits bounded duration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(boundary_maintenance_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bound_theater_t0, boundary_maintenance_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(bound_theater_t5, boundary_maintenance_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement(bound_theater_t10, boundary_maintenance_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(bound_extr_t0, boundary_maintenance_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bound_extr_t3, boundary_maintenance_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(bound_extr_t6, boundary_maintenance_reading, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(bound_extr_t10, boundary_maintenance_reading, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(bound_supp_t0, boundary_maintenance_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(bound_supp_t3, boundary_maintenance_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(bound_supp_t6, boundary_maintenance_reading, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(bound_supp_t10, boundary_maintenance_reading, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(boundary_maintenance_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(boundary_maintenance_reading, 0.1).
narrative_ontology:affects_constraint(boundary_maintenance_reading, symbol_continuity_reading).
narrative_ontology:affects_constraint(boundary_maintenance_reading, trauma_encoding_reading).
narrative_ontology:affects_constraint(boundary_maintenance_reading, survival_competence_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_kernel admits four structurally distinct constraint readings, each with different ε values and victim structures. boundary_maintenance_reading (this story) isolates the function of mourning-ritual as boundary-enforcement with moderate extractiveness (0.48). Sibling readings decompose the kernel along orthogonal functional axes: symbol_continuity_reading tracks cultural-code preservation with different suppression mechanism; trauma_encoding_reading tracks narrative transmission with different victim set; survival_competence_reading tracks skill-development with low baseline extraction. All four readings link the same underlying phenomenon (mourning ritual) but identify different constraint-structures within it per the ε-invariance principle: if the observable changes (functional focus shifts), ε changes, so the constraints are distinct stories. Family members coexist as live positions held by different scholarly communities and different observer positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(boundary_maintenance_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
