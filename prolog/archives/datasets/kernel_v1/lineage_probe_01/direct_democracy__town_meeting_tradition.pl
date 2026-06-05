% ============================================================================
% CONSTRAINT STORY: direct_democracy__town_meeting_tradition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_direct_democracy__town_meeting_tradition, []).

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
 *   constraint_id: direct_democracy__town_meeting_tradition
 *   human_readable: Direct Democracy as Town Meeting Tradition
 *   domain: political/comparative
 *
 * SUMMARY:
 *   The town meeting tradition instantiates one reading of the direct
 *   democracy kernel: self-government at the scale where all assembled
 *   neighbors can be heard, with no intermediaries between the people and the
 *   legislature. This reading emphasizes the communal, face-to-face,
 *   transparent nature of direct participation. However, it simultaneously
 *   suppresses delegation, excludes non-attendees from formal voice, and
 *   concentrates power in those with temporal and spatial flexibility to be
 *   present. The constraint exhibits Tangled Rope structure: it coordinates
 *   genuine deliberation (any resident can be heard; issues are debated
 *   openly; decisions reflect collective will) while simultaneously
 *   extracting from those unable to attend (systematic exclusion from voice
 *   despite subjection to outcomes; invisible labor of those who make
 *   attendance possible; externalities borne by larger populations outside
 *   the town meeting scope). The extractiveness is moderate (0.38) because
 *   the coordination function is genuine, the suppression is real but not
 *   total (alternatives exist, though imperfect), and the beneficiary/victim
 *   distinction is primarily determined by attendance capacity rather than
 *   state power. This reading differs structurally from plebiscitary capture
 *   (which treats direct democracy as the ruler's tool) and Swiss referendum
 *   practice (which embeds direct democracy in ongoing representative
 *   institutions).
 *
 * KEY AGENTS:
 *   - Attending Residents: Primary beneficiary (organized/mobile) — benefit from direct voice, transparency, collective deliberation. Regular attenders accumulate decisional power over time.
 *   - Non-Attending Residents: Primary victim (powerless/trapped) — excluded from formal voice despite local residency; subject to outcomes; barriers include work schedules, childcare, disability, social exclusion.
 *   - Out-of-Scope Populations: Secondary victim (powerless/trapped) — residents in larger regions affected by town decisions (environmental, economic spillovers) with no town meeting voice.
 *   - Town Clerk / Administrative Facilitator: Institutional enabler (institutional/arbitrage) — maintains the coordination mechanism; sees role as serving the assembly; identity fused with town meeting institution.
 *   - Landowner / Established Merchant: Powerful participant (powerful/mobile) — has time and incentive to attend; experiences mixed coordination and extraction (voice matters but must compete with others).
 *   - Historical Preservationist: Organizational maintainer (organized/constrained) — sustains town meeting as identity symbol; increasingly performative function as state agencies absorb real power.
 *   - Deliberative Democracy Reformer: Structural reformer (moderate/constrained) — sees current form as temporary scaffold; proposes inclusive redesigns (childcare, evening meetings, online participation).
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks treating scale limit as natural law when it is contingent on communication technology and design choices.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(direct_democracy__town_meeting_tradition, 0.38).
domain_priors:suppression_score(direct_democracy__town_meeting_tradition, 0.42).
domain_priors:theater_ratio(direct_democracy__town_meeting_tradition, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(direct_democracy__town_meeting_tradition, extractiveness, 0.38).
narrative_ontology:constraint_metric(direct_democracy__town_meeting_tradition, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(direct_democracy__town_meeting_tradition, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(direct_democracy__town_meeting_tradition, tangled_rope).
narrative_ontology:human_readable(direct_democracy__town_meeting_tradition, "Direct Democracy as Town Meeting Tradition").
narrative_ontology:topic_domain(direct_democracy__town_meeting_tradition, "political/comparative").

domain_priors:requires_active_enforcement(direct_democracy__town_meeting_tradition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(direct_democracy__town_meeting_tradition, 'fb92cd0d-011f-448c-a215-4af486585deb').
narrative_ontology:cs_kernel_codification('fb92cd0d-011f-448c-a215-4af486585deb', fixed_text).
narrative_ontology:cs_authority_grounding('fb92cd0d-011f-448c-a215-4af486585deb', lineage).
narrative_ontology:cs_interpretation_layer_present('fb92cd0d-011f-448c-a215-4af486585deb').
narrative_ontology:cs_reading_relation('fb92cd0d-011f-448c-a215-4af486585deb', direct_democracy__plebiscitary_capture, coexists_with).
narrative_ontology:cs_reading_relation('fb92cd0d-011f-448c-a215-4af486585deb', direct_democracy__swiss_referendum_system, influences).
narrative_ontology:cs_axiom('fb92cd0d-011f-448c-a215-4af486585deb', foundational, direct_democracy_requires_presence).
narrative_ontology:cs_axiom_status(direct_democracy_requires_presence, holdable).
narrative_ontology:cs_axiom_grounding('fb92cd0d-011f-448c-a215-4af486585deb', direct_democracy_requires_presence, empirically_contingent).
narrative_ontology:cs_axiom('fb92cd0d-011f-448c-a215-4af486585deb', secondary, scale_limit_from_geometry).
narrative_ontology:cs_axiom_status(scale_limit_from_geometry, holdable).
narrative_ontology:cs_axiom_grounding('fb92cd0d-011f-448c-a215-4af486585deb', scale_limit_from_geometry, empirically_contingent).
narrative_ontology:cs_reference_frame('fb92cd0d-011f-448c-a215-4af486585deb', new_england_communal_assembly).
narrative_ontology:cs_drift_state('fb92cd0d-011f-448c-a215-4af486585deb', contemporary_neoliberal_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fb92cd0d-011f-448c-a215-4af486585deb', '').
narrative_ontology:cs_kernel_id(direct_democracy__town_meeting_tradition, direct_democracy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(direct_democracy__town_meeting_tradition, attending_residents).
narrative_ontology:constraint_victim(direct_democracy__town_meeting_tradition, non_attending_residents).
narrative_ontology:constraint_victim(direct_democracy__town_meeting_tradition, out_of_scope_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-ATTENDING RESIDENT (SNARE) — Structurally excluded from the commons by work obligations, childcare, disability, or systematic social exclusion. Bears all costs of decisions made in the town meeting without voice. Cannot exit local governance; must live under rules they had no formal power to shape. The suppression is internalized (belief that 'meeting nights are for other people') and structural (the meeting time itself excludes shift workers, caregivers). Maximum extraction experienced — no coordination benefit, only subjection.
constraint_indexing:constraint_classification(direct_democracy__town_meeting_tradition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INTERMITTENT PARTICIPANT (TANGLED ROPE) — Attends occasionally when directly affected. Experiences genuine coordination (issues are debated openly; voice matters proportionally) but also extraction (voice requires presence; absence means exclusion; the timing and venue suppress participation from certain demographics). Benefits from the transparency and collective deliberation; bears costs of the design's exclusionary mechanics. Partial agency within a constraint that requires constant presence.
constraint_indexing:constraint_classification(direct_democracy__town_meeting_tradition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: REGULAR ATTENDEE (ROPE) — Flexible schedule, proximity to meeting venue, invested in local issues. Experiences the town meeting as pure coordination: open debate, collective decision-making, voice directly affecting outcomes. The constraint is their empowerment mechanism. They benefit from the genuine coordination function with minimal experienced extraction. Mobility is real — they *could* move to an area with representative government, but choose the town meeting form because it serves their interests.
constraint_indexing:constraint_classification(direct_democracy__town_meeting_tradition, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: TOWN CLERK / ADMINISTRATIVE FACILITATOR (ROPE) — Institutional actor embedded in the town meeting. Sees their role as enabling genuine coordination: recording decisions, managing agenda, ensuring procedures are followed. Experiences low extraction because the administrative function genuinely serves the assembly. Arbitrage exists (could migrate to state/federal administration) but the clerk's identity is fused with the town meeting institution — high mobility structurally, but identity_locked functionally.
constraint_indexing:constraint_classification(direct_democracy__town_meeting_tradition, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 5: LANDOWNER / ESTABLISHED MERCHANT (TANGLED ROPE) — Has time flexibility (owns their schedule), strong local social position, economic incentives to attend. Experiences town meeting as coordination mechanism that protects their property interests and market position through transparent local deliberation. Also experiences extraction: needs must compete for scarce deliberative time; major decisions (zoning, taxation affecting property) are subject to majority voice. The constraint both enables and constrains them — coordination function is genuine, asymmetry is real but not total (powerful agent can still navigate the system).
constraint_indexing:constraint_classification(direct_democracy__town_meeting_tradition, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 6: SYSTEMATIC OUTSIDER (GENERATIONAL) — A demographic systematically excluded across generations: tenant families with volatile schedules, undocumented residents, recent migrants without established networks, people with disabilities that make meeting attendance impossible. For these agents, the town meeting is not a coordination mechanism but an extractive mechanism that legitimizes their exclusion as 'voluntary non-participation.' They are trapped in the locality, excluded from the assembly, and subject to majority decisions made without their input. The suppression compounds across generations.
constraint_indexing:constraint_classification(direct_democracy__town_meeting_tradition, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 7: HISTORICAL PRESERVATIONIST (PITON) — Views the town meeting as a cultural artifact of New England governance identity. Maintains the form through civic ritual and historical narrative even as functional power has migrated to state agencies, school boards, and regional authorities. Town meetings persist in theater: the assembly votes on warrant articles that are mostly routine budget approvals and administrative confirmations. Real power over education, infrastructure, and environmental regulation has been transferred to state agencies; the town meeting maintains its ceremonial function as a form of identity assertion. Theater ratio has risen as actual deliberative power has declined.
constraint_indexing:constraint_classification(direct_democracy__town_meeting_tradition, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: DELIBERATIVE DEMOCRACY REFORMER (SCAFFOLD) — Sees town meetings as a temporary staging ground for broader deliberative democracy experiments: citizen assemblies, participatory budgeting, random sortition to increase diversity of participation, evening/weekend/online deliberations to reduce temporal exclusion. Views the current form as a scaffold that could be transformed into more inclusive participation mechanisms. Sees a sunset: as digital tools and dispersed participation norms mature, the physical gathered-in-one-room constraint becomes less necessary. This perspective treats the exclusionary mechanics of the traditional form as solvable design problems, not immutable features.
constraint_indexing:constraint_classification(direct_democracy__town_meeting_tradition, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 9: NATURAL LAW THEORIST (MOUNTAIN) — Views the town meeting scale as reflecting a natural limit: direct democracy only works at the scale where everyone can physically gather and be heard — roughly 1,000-5,000 people. Above this threshold, the constraint becomes mathematically impossible (time to speak → zero). Below it, governance can be direct. This is treated as an immutable law of political organization. However, the structural data contradicts the mountain classification — the beneficiary (attending residents) and victims (non-attending) are clearly identifiable, and extractiveness is governed by who shows up, not by physics. The analytical observer risks naturalizing a contingent design choice (in-person meeting) as inherent to democracy itself.
constraint_indexing:constraint_classification(direct_democracy__town_meeting_tradition, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(direct_democracy__town_meeting_tradition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(direct_democracy__town_meeting_tradition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(direct_democracy__town_meeting_tradition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(direct_democracy__town_meeting_tradition, TR),
    TR >= 0.70.

:- end_tests(direct_democracy__town_meeting_tradition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The town meeting coordinates genuine deliberation (beneficiary function is real), but concentrates power in those who attend, creating systematic extraction from non-attendees. The measurement shows rise from 0.25 to 0.38 over the interval (time points 0, 50, 100), reflecting historical demographic shift: as work becomes less place-based and childcare burdens have fallen more heavily on certain populations, the suppression of non-attendance has increased, raising extractiveness. This is not a design flaw discovered and corrected, but an accumulation of excluded populations over time. Suppression (0.42): Moderate. Multiple barriers to attendance: temporal (meeting timing); spatial (physical venue accessibility); cognitive (assumption that 'meeting is for people who know each other' — identity_locked non-participation); structural (shift work, caregiving responsibilities). The measurement shows rise from 0.30 to 0.42, reflecting increasing temporal rigidity of work and caregiving norms. Theater ratio (0.35): Low-moderate. The traditional form has genuine deliberative function (decisions are actually debated, not merely performed). However, measurement shows rise from 0.15 to 0.35, indicating increasing performative content as real power has migrated to state agencies (school districts, planning boards with state mandates, environmental regulations from federal law). The town meeting votes on warrant articles that must implement state requirements, reducing actual deliberative scope. The piton perspective (theater_ratio ≥ 0.70) has not yet been reached but is approaching.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between attending regular and non-attending resident is maximal: the same constraint is experienced as empowerment (rope/coordination) from the inside and as subjection (snare/extraction) from the outside. The regular attendee sees 'everyone can be heard if they show up'; the non-attendee sees 'I cannot show up and therefore have no voice.' The extractiveness is the same (0.38 in base properties) but the experienced chi differs because d differs: beneficiaries with mobile exit experience low f(d) → low chi; victims with trapped exit experience high f(d) → high chi. The piton and scaffold perspectives share a generational time horizon but opposite conclusions about sunset: the preservationist sees the form as increasingly performative but worth maintaining; the reformer sees the exclusionary mechanics as solvable design problems (childcare, timing, online participation) that could resurrect the coordination function. The mountain perspective risks naturalizing the scale limit as immutable when it is contingent on the chosen communication technology (in-person gathering vs alternatives). False summit detection: the mountain's core claim is that direct democracy must operate at scales where everyone can be heard in real-time — but this depends on treating 'heard in real-time physical gathering' as the definition of democracy rather than one implementation. If heard through asynchronous online deliberation, or through citizen panels with demographic weighting, the scale constraint shifts or vanishes. The mountain is a false summit if it treats a design choice as a physical law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from their structural position within the constraint. Regular attendees with flexible schedules (organized/mobile) experience low d because they are beneficiaries and can exit — they choose to stay. Non-attendees with work/care obligations (powerless/trapped) experience high d because they are victims with no exit. The attending residents are the extractors by virtue of occupying the deliberative space; non-attendees are the extracted-from by virtue of being excluded. The regular attendees' power derives not from institutional position but from temporal/spatial privilege + identity investment ('I am an engaged citizen'). The mechanism differentiates regular from intermittent attenders through exit options: regular attenders have effectively arbitrage-level mobility (could choose other towns, alternative governance models) but stay because the form serves them; intermittent attenders face constrained mobility (must attend when directly affected but cannot sustain constant presence). The clerk experiences arbitrage-level exit (could work in state administration) but identity-locked constraints keep them in town governance.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING INSTANTIATION: This constraint resolves the mandatrophy by specifying one complete reading of the direct democracy kernel. The town meeting tradition reading claims that direct democracy's authentic form is the assembled neighbors with no mediators. This is logically coherent (the kernel is direct participation; the reading operationalizes it at scale where face-to-face assembly is feasible). The sibling reading (plebiscitary capture) claims direct democracy's shadow is manipulation of the populace by the powerful. These coexist as different practices of 'direct democracy' — one emphasizing genuine deliberation, the other emphasizing mass mobilization by elites. The Swiss referendum reading emphasizes embedding direct democracy in ongoing institutions. All three readings reference the same kernel (what is direct democracy?) but implement structurally different constraints. The town meeting reading's extractiveness (0.38) is moderate because it delivers genuine coordination while simultaneously suppressing participation — this is the reading's core tension. The mandatrophy is resolved not by choosing one reading as 'correct' but by recognizing that each reading instantiates a different constraint with different ε, beneficiary/victim structure, and perspectival profiles.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attendance_suppression_mechanism,
    'Is the suppression of non-attendance structural (barriers to participation that remove choice) or internalized (people believe they don''t belong)?',
    'Experimental intervention: provide childcare, shift the meeting time to evening/weekend, offer online participation. If attendance increases significantly: suppression is structural. If attendance stalls: suppression is internalized (identity-locked non-participation).',
    'If structural: the constraint can be reformed (Scaffold perspective). If internalized: the constraint requires identity reframing, not just design changes (deeper work on belonging). Classification may shift from Snare to Tangled Rope if barriers are truly removable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attendance_suppression_mechanism, empirical, 'Whether non-attendance suppression is structural or internalized').

omega_variable(
    scale_threshold_contingency,
    'Is the 1,000-5,000 person threshold for direct democracy an immutable physical/cognitive law or a contingent design constraint dependent on communication technology?',
    'Historical comparison: how did town meeting scale evolve with communication technology (printing, telegraphy, radio, television, internet)? Can digital deliberation mechanisms enable direct democracy at larger scales without spatial co-presence? Comparative study of deliberative innovations that bypass the gathered-assembly requirement.',
    'If immutable: mountain classification is correct (scale is a natural law). If contingent: the mountain perspective is a false summit (naturalizes technology-dependent design choice). Extractiveness may be lower at larger scales using alternative deliberation mechanisms, suggesting the constraint is solvable rather than essential.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scale_threshold_contingency, empirical, 'Whether the direct democracy scale limit is immutable or technology-contingent').

omega_variable(
    delegation_vs_direct_false_choice,
    'Is the town meeting a reading of ''direct democracy as avoiding delegation'' or an instance of ''regular attendees delegating collective power to those with time to attend''?',
    'Network analysis of attendance patterns: do regular attendees functionally represent non-attendees? Comparison of decisions with demographic composition: do meeting outcomes track attendee preferences or town-wide preferences? Survey of non-attendees: do they see themselves as delegating to regular attendees or as excluded from delegation?',
    'If regular attendees are de facto delegates: the town meeting is a form of representative democracy dressed in participatory language — extractiveness may be higher (false coordination claim). If decisions systematically favor non-attendee preferences: genuine direct democracy. If decisions favor attendee preferences: embedded extraction from non-attendees.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(delegation_vs_direct_false_choice, empirical, 'Whether town meetings instantiate direct democracy or hidden delegation').

omega_variable(
    reading_contest_foreclosure,
    'Does the town meeting reading foreclose the plebiscitary capture reading (shared kernel, different conclusions about what direct democracy IS)?',
    'Logical analysis: can a town meeting coexist with plebiscitary dynamics? Historical cases where town meetings have been used for plebiscitary mobilization. Examination of whether the two readings describe different systems or different aspects of the same system.',
    'If foreclosing: one reading of the kernel is logically incompatible with the other within a single framework — the kernel contest is a logical/philosophical dispute, not a sociological one. If coexisting: both readings are live positions (different communities practice different readings). Affects cs_structure.reading_relations assignment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Whether town meeting and plebiscitary readings are mutually exclusive or coexistent').

omega_variable(
    attending_resident_identity,
    'Is attending the town meeting a chosen political identity or a de facto privilege signal (marking those with flexible time, proximity, and comfort in deliberative forums)?',
    'Demographic analysis of regular attendees vs town population. Exit interviews with non-attendees: what barriers they cite vs what patterns analysis reveals. Identity/motivation research: do attendees self-identify as ''active citizens'' and non-attendees as ''less engaged'' (signaling) or do they cite material barriers (structural)?',
    'If identity: the beneficiary is self-selected (those who identify as ''engaged citizens''); the constraint reinforces identity hierarchies. If privilege: the beneficiary is those with structural advantage; the constraint naturalizes class/time wealth as political virtue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attending_resident_identity, empirical, 'Whether attending town meetings signals chosen identity or structural privilege').

omega_variable(
    scale_bifurcation_by_complexity,
    'Does the constraint enforce different scale limits depending on issue complexity? Simple procedural votes might scale to thousands; complex policy deliberation might scale to hundreds. Is there a complexity-dependent threshold the reading should account for?',
    'Analysis of meeting agendas and decision quality: do high-complexity issues (zoning disputes, tax policy) get less attention/quality deliberation as attendance increases? Do simpler issues (confirmations, routine approvals) show no scale effects?',
    'If true: the reading conflates multiple constraints (simple vs complex decision scale) that have different ε values. Should decompose into separate stories per issue complexity. If false: the constraint''s scale ceiling applies uniformly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scale_bifurcation_by_complexity, empirical, 'Whether scale constraints vary by issue complexity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(direct_democracy__town_meeting_tradition, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(townmtg_tr_t0, direct_democracy__town_meeting_tradition, theater_ratio, 0, 0.15).
narrative_ontology:measurement(townmtg_tr_t50, direct_democracy__town_meeting_tradition, theater_ratio, 50, 0.25).
narrative_ontology:measurement(townmtg_tr_t100, direct_democracy__town_meeting_tradition, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(townmtg_be_t0, direct_democracy__town_meeting_tradition, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(townmtg_be_t50, direct_democracy__town_meeting_tradition, base_extractiveness, 50, 0.32).
narrative_ontology:measurement(townmtg_be_t100, direct_democracy__town_meeting_tradition, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(townmtg_su_t0, direct_democracy__town_meeting_tradition, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(townmtg_su_t50, direct_democracy__town_meeting_tradition, suppression_requirement, 50, 0.38).
narrative_ontology:measurement(townmtg_su_t100, direct_democracy__town_meeting_tradition, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(direct_democracy__town_meeting_tradition, identity_coordination).
narrative_ontology:affects_constraint(direct_democracy__town_meeting_tradition, direct_democracy__plebiscitary_capture).
narrative_ontology:affects_constraint(direct_democracy__town_meeting_tradition, direct_democracy__swiss_referendum_system).

% DUAL FORMULATION NOTE:
% The town meeting tradition is one operationalization of the direct democracy kernel. The other readings (plebiscitary capture, Swiss referendum system) are structurally distinct constraints with different extractiveness, suppression, and beneficiary/victim structures, but all three reference the same contested kernel. This story models the town meeting reading as Tangled Rope (0.38 extractiveness); the sibling readings will have different ε values reflecting their different coordination functions and extraction mechanisms. Linked via the kernel_id in the committer frame.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
