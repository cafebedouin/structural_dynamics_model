% ============================================================================
% CONSTRAINT STORY: hybrid_atrophy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_atrophy_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hybrid_atrophy_reading
 *   human_readable: Ritual Atrophy: Mourning Practice Inheriting Survival-Competence Structure
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   The constraint describes a ritual that originated as a
 *   survival-coordination mechanism — marking kin membership, encoding
 *   ecological knowledge, regulating grief-induced behavioral disruption —
 *   but has atrophied under modernity while persisting through institutional
 *   and identity-based enforcement. The ritual structure remains elaborate
 *   and demanding (high suppression, high theater), but the original adaptive
 *   payoff (survival coordination in pre-modern contexts) is no longer
 *   salient to present-generation participants. Instead, the ritual functions
 *   (or is claimed to function) as mourning practice and identity
 *   maintenance. This reading treats the constraint as a piton — a former
 *   functional mechanism (whether primarily survival-coordination or
 *   coordination-with-extraction) that persists through theatrical
 *   maintenance and identity-lock despite atrophied primary function. The
 *   theater_ratio increases over the interval as modernity offers alternative
 *   mourning and identity frameworks, making the ritual's functional
 *   necessity less obvious and its performative burden more visible.
 *   Extractiveness remains moderate because genuine coordination benefits
 *   (mourning processing, social connection) persist alongside the costs, but
 *   the ratio has shifted — modernity has made the extraction mechanism more
 *   salient.
 *
 * KEY AGENTS:
 *   - Present-generation participants: Primary victims (powerless/identity-locked or moderate/constrained) — inherit the ritual without adaptive payoff in modern context; bear time, emotional, and opportunity costs
 *   - Religious institution: Primary beneficiary (institutional/arbitrage) — maintains authority through custodianship of tradition; uses ritual to bind members and legitimize institutional legitimacy
 *   - In-group identity: Secondary beneficiary (abstract) — ritual serves as boundary marker and identity assertion; modern alternative identity frameworks make this function less compelling
 *   - Historical ancestors: Nominal beneficiary (abstract/non-agent) — their memory is preserved, but preservation is enforced on present agents rather than negotiated
 *   - Modernist reformers: Secondary agents (organized/mobile) — see the ritual as a coordination problem with structural sunset; building alternative mourning and identity frameworks
 *   - Closed-community members: Secondary victims (powerless/trapped in some contexts) — face material barriers to exit; experience pure extraction through total institutional control
 *   - Analytical observer: Civilization-scale perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable cultural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_atrophy_reading, 0.38).
domain_priors:suppression_score(hybrid_atrophy_reading, 0.62).
domain_priors:theater_ratio(hybrid_atrophy_reading, 0.76).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_atrophy_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(hybrid_atrophy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hybrid_atrophy_reading, theater_ratio, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(hybrid_atrophy_reading, "Ritual Atrophy: Mourning Practice Inheriting Survival-Competence Structure").
narrative_ontology:topic_domain(hybrid_atrophy_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hybrid_atrophy_reading, 'a7e75233-0e39-47bc-839a-f8d167b0c574').
narrative_ontology:cs_created_at('a7e75233-0e39-47bc-839a-f8d167b0c574', '').
narrative_ontology:cs_kernel_codification('a7e75233-0e39-47bc-839a-f8d167b0c574', distributed).
narrative_ontology:cs_authority_grounding('a7e75233-0e39-47bc-839a-f8d167b0c574', lineage).
narrative_ontology:cs_interpretation_layer_present('a7e75233-0e39-47bc-839a-f8d167b0c574').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_atrophy_reading, in_group_identity_maintenance).
narrative_ontology:constraint_beneficiary(hybrid_atrophy_reading, historical_ancestral_survival).
narrative_ontology:constraint_victim(hybrid_atrophy_reading, present_generation_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RITUAL PARTICIPANT (PITON, IDENTITY-LOCKED) — The present-generation participant is structurally mobile (could decline participation without legal or material barrier) but identity-fused with the ritual as a marker of group belonging. Exit would require abandoning the community identity that constitutes the self. The ritual persists through internalized obligation rather than external coercion. High theater (performative mourning) masks the atrophy of original adaptive function (survival coordination). The constraint is experienced as burden without payoff — the costs of participation are salient, the benefits (ancestral protection, group cohesion through shared memorial) are not.
constraint_indexing:constraint_classification(hybrid_atrophy_reading, piton,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: RELIGIOUS INSTITUTION (ROPE) — The institution perceives the ritual as pure coordination: it coordinates group identity, maintains continuity with ancestral tradition, and solves the collective action problem of shared mourning. The institution benefits from the ritual's role in binding members and legitimating institutional authority through custodianship of tradition. From the institutional perspective, the constraint is a coordination mechanism with minimal extraction — it generates social cohesion at low cost to the institution itself.
constraint_indexing:constraint_classification(hybrid_atrophy_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: DUAL-PRACTICE ADHERENT (TANGLED ROPE) — Adherents embedded in modernity face constrained exit: leaving the ritual risks ostracism, loss of family connection, breach of group identity. They also derive some benefit from ritual participation (mourning processing, social connection) but experience the costs as disproportionate to observable benefits. This perspective sees both coordination (genuine mourning function) and extraction (cost of time, emotional labor, adherence to tradition without adaptive payoff in modern contexts). Experienced extractiveness is moderate because the agent has some agency and some genuine coordination benefit, but also bears asymmetric costs.
constraint_indexing:constraint_classification(hybrid_atrophy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: COERCED PARTICIPANT (SNARE, TRAPPED) — In communities with total institutional control (some fundamentalist or closed communities), participants face material barriers to exit: economic dependency on the community, geographic isolation, restricted information about alternatives. For these agents, the ritual is experienced as pure extraction — costs without perceived benefit, enforced through ostracism and economic coercion. The constraint is a snare because suppression is structural and exit is materially impossible.
constraint_indexing:constraint_classification(hybrid_atrophy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: MODERNIST REFORMER (SCAFFOLD) — Reform movements within religious communities see the atrophied ritual as a temporary coordination problem with a structural sunset. As education increases, communication technology expands, and alternative mourning practices proliferate, the ritual's role as a coordination mechanism decays — participants no longer need it to maintain group identity (social media, diaspora networks, virtual participation enable this). The reformer perspective sees the ritual as gradually being replaced by more efficient coordination mechanisms. Low effective extraction because the agent perceives agency and a clear exit path.
constraint_indexing:constraint_classification(hybrid_atrophy_reading, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN, ANALYTICAL) — From a civilizational/universal perspective, rituals are immutable features of human collective meaning-making. All cultures preserve ritual form even when function atrophies, and this persistence is treated as a natural law of cultural inheritance. The constraint is classified as mountain because it appears as an irreducible feature of how humans mark transitions and maintain identity. However, the structural data contradicts this: the ritual has identifiable beneficiaries (in-group identity, institutional authority) and victims (present-generation bearing costs), indicating this is a false summit — a contingent institutional arrangement naturalized as cultural necessity.
constraint_indexing:constraint_classification(hybrid_atrophy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_atrophy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_atrophy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_atrophy_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hybrid_atrophy_reading, TR),
    TR >= 0.70.

:- end_tests(hybrid_atrophy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint involves genuine coordination benefits (mourning processing, social connection, identity affirmation) alongside asymmetric costs (participants bear more labor than beneficiaries receive direct payoff from). The baseline (0.28) reflects that in pre-modern contexts, the ritual genuinely solved survival-coordination problems with lower theater; the present value (0.38) reflects that modern context has exposed the extraction mechanism while the coordination function persists. Theater ratio (0.76): High and rising. In pre-modern contexts, the ritual's performative and functional elements were integrated — marking transitions and encoding survival knowledge simultaneously. Under modernity, the functional necessity has declined while the performative burden has increased. Participants follow elaborate forms (high theater) without clear understanding of original adaptive purpose (atrophy). The rise from 0.52 to 0.76 over 40 time units indicates that the ritual is becoming increasingly performative relative to functional — the atrophy is measurable through rising theater. Suppression (0.62): Moderate-high. Barriers to exit include social ostracism (structural), economic dependency on community (structural for some contexts), and identity-lock (internalized). The suppression is not total because many modern contexts allow mobility-with-cost rather than absolute confinement. The ambiguity between structural and internalized suppression is routed to an omega variable.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a perspectival chasm between the institution (seeing pure coordination) and participants (seeing burden without payoff). The institution's rope classification reflects its genuine coordination function and net benefit. The participant's piton or snare classification reflects the atrophy of original function combined with persistent cost. The reformer's scaffold perspective sees the constraint as a temporary coordination problem being displaced by more efficient alternatives. The analytical observer's mountain classification risks naturalizing what is actually a contingent institutional arrangement by treating ritual persistence as immutable. The gap reveals that the same constraint structure produces fundamentally different experienced extractiveness depending on whether one's identity or authority is constituted through the ritual versus inherited as burden.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's structural relationship to the ritual's actual (not nominal) benefits. The institution derives clear benefit from ritual maintenance (identity binding, authority legitimation) with arbitrage-level exit freedom — low d. The participant derives mixed benefit (mourning processing, identity affirmation) but bears asymmetric costs (time, emotional labor, adherence to tradition) and faces constrained or identity-locked exit — high d. The ancestor (nominal beneficiary) does not exist as an agent; the preservation benefit accrues to the institution, not to the historical entity. This concentration of benefit in present institutional actors and diffusion of cost across participants is the extraction mechanism. The powerless/trapped or powerless/identity-locked participant experiences maximum d (0.95-1.0) because they bear costs without receiving agent-level benefit and face insurmountable exit barriers. The moderate/constrained participant experiences moderate d (0.60-0.75) because they have some agency and some benefit, but costs are asymmetric. The institutional beneficiary experiences low d (0.05-0.20) because they benefit from the constraint and have easy exit if desired.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing three analytically separate claims: (1) the ritual originated as survival-coordination (possibly true; not evaluated here), (2) the ritual currently functions as mourning practice (partially true; mourning is one function), (3) the ritual's persistence is explained by genuine continued coordination benefits (false for many participants; persistence is explained by identity-lock and institutional enforcement). The piton classification correctly captures the structure: the ritual persists through theatrical maintenance and identity-enforcement despite atrophied primary function. The constraint is NOT a rope (pure coordination) because the costs are asymmetric and the original adaptive rationale is no longer salient. The constraint is NOT a snare (pure extraction) because the mourning and identity-affirmation functions are real. The constraint IS a piton because the performative burden exceeds the functional benefit, and persistence depends on institutional and identity-level enforcement rather than efficient coordination. The mandatrophy is resolved by recognizing that 'ritual survival under modernity' is analytically distinct from 'functional coordination mechanism' — the ritual survives not because it coordinates efficiently but because it binds identities and legitimates institutions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint fundamentally about atrophied survival coordination (original adaptive function), mourning practice (current function), or identity maintenance (meta-function of both)? Which reading is structurally primary?',
    'Historical analysis of ritual origin and transformation; ethnographic documentation of whether participants cite survival, mourning, or identity as the actual reason for continuation; measurement of whether ritual alteration for mourning efficiency would be accepted or rejected by community',
    'If survival-competence reading is primary: ε would be higher (0.55+), type would be snare (beneficiaries are historical, victims are present). If mourning-practice reading is primary: ε would be lower (0.25-0.35), type would be rope or scaffold. If identity-maintenance reading is primary: ε stays moderate (0.38), type is piton — current reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Which of three sibling readings captures the kernel''s structural force: survival adaptation, mourning function, or identity constitution').

omega_variable(
    theater_ratio_calibration,
    'How much of the ritual''s performative content (0.76 theater_ratio) is functional mourning (necessary emotional processing) versus pure theater (performative identity assertion)?',
    'Comparison of ritual participant self-reports (perceived functional benefit vs performance obligation); ethnographic observation of whether ritual modifications that reduce theater but preserve mourning function are accepted or rejected; measurement of psychological outcomes (grief processing, social bonding) with theater-reduced versions',
    'If theater is mostly performative: theater_ratio justified, piton classification confirmed. If theater is mostly functional: theater_ratio overstated, constraint reclassifies toward rope or tangled_rope. This directly determines whether the constraint is atrophied (piton) or still functionally viable (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_calibration, empirical, 'Proportion of ritual performance that is functional mourning versus performative identity assertion').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.62) structural (external social/economic penalties for non-participation) or internalized (identity-lock that creates psychological barriers even without external enforcement)?',
    'Ethnographic comparison: communities with strong external enforcement (ostracism, economic exclusion for non-participation) versus communities with internalized obligation; longitudinal tracking of whether suppression persists after emigration or geographic exit from the community; measurement of participant distress with participation versus willingness to exit if external penalties were removed',
    'If mostly structural: exit_options would be constrained or trapped (not identity_locked), and the constraint would show lower suppression in diaspora populations. If mostly internalized: identity_locked exit is correct, and the constraint would show persistent suppression even in high-exit-option environments. This determines whether the constraint binds through identity or material coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural (external enforcement) or internalized (identity fusion)').

omega_variable(
    beneficiary_historical_vs_present,
    'Who actually benefits from this constraint in the present day? The institution (identity maintenance, authority legitimation) or the historical ancestors (their memory is preserved)?',
    'Analysis of institutional resource allocation (time, money, authority invested in ritual maintenance); measurement of whether ritual beneficiaries are identifiable present-day agents with agency or abstractly distributed across ''tradition''; ethnographic documentation of whether community members see themselves as benefiting or burdened',
    'If present institutional beneficiaries are identifiable: ε justified (0.38), victims are clear (present participants), constraint is tangled rope or snare. If historical ancestors are the nominal beneficiaries: beneficiary is abstract/non-agent, constraint reclassifies toward mountain (immutable duty) or snare (no present beneficiary). This affects whether the constraint is extractive (has present beneficiaries) or mourning (no present extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_historical_vs_present, empirical, 'Whether beneficiary is present institutional actors or abstract historical inheritance').

omega_variable(
    modernity_timeline_for_ritual_sunset,
    'Is the constraint actually undergoing ritual sunset (scaffold logic) or is it stable/even strengthening in the present era despite being atrophied?',
    'Longitudinal measurement: participation rates, ritual adherence strictness, theater_ratio over 50+ years; comparison of old vs young participant cohorts; measurement of ritual modification rates (are communities adapting the ritual or enforcing traditional form?); documentation of whether diaspora communities maintain, modify, or abandon the ritual',
    'If sunset is real: scaffold perspective is correct, chi should decline over interval, measurements should show theater_ratio rising and then declining as alternatives take hold. If constraint is stable or strengthening: it is not a scaffold but a persistent piton or snare; the ''atrophy'' is in function, not force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernity_timeline_for_ritual_sunset, empirical, 'Whether the constraint is undergoing actual sunset or is stable/strengthening despite atrophied function').

omega_variable(
    identity_locked_versus_constrained_exit,
    'For the primary victim (present-generation participant), is exit truly identity-locked (self-concept constituted through ritual participation) or merely constrained by high cost (social ostracism, family rupture)?',
    'In-depth ethnographic interviews: do participants describe exit as unthinkable (identity) or as thinkable-but-costly (constrained)? Do participants who have exited report identity dissolution or role loss? Do participants in high-option-to-exit environments (diaspora, urban) still participate even when external barriers are removed? Measurement of whether alternative identity frameworks (secular identity, different community membership) are available to participants.',
    'If identity-locked: exit_options classification correct, suppression is partly internalized, constraint binds through identity frame. If constrained: exit_options should downgrade to constrained (not trapped), suppression is structural, constraint binds through material cost. This affects whether exit is structurally possible or psychologically impossible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_locked_versus_constrained_exit, empirical, 'Whether victim''s binding is through identity fusion (identity-locked) or high external cost (constrained exit)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_atrophy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybr_tr_t0, hybrid_atrophy_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(hybr_tr_t20, hybrid_atrophy_reading, theater_ratio, 20, 0.68).
narrative_ontology:measurement(hybr_tr_t40, hybrid_atrophy_reading, theater_ratio, 40, 0.76).

% Extraction over time
narrative_ontology:measurement(hybr_be_t0, hybrid_atrophy_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hybr_be_t20, hybrid_atrophy_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(hybr_be_t40, hybrid_atrophy_reading, base_extractiveness, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_atrophy_reading, attachment_coordination).
narrative_ontology:affects_constraint(hybrid_atrophy_reading, survival_competence_reading).
narrative_ontology:affects_constraint(hybrid_atrophy_reading, mourning_practice_reading).
narrative_ontology:affects_constraint(hybrid_atrophy_reading, institutional_authority_through_tradition).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_preservation kernel decomposes into three structurally distinct constraint stories with different ε values and types. This reading (hybrid_atrophy) treats the ritual as a piton — a former functional mechanism now persisting through identity-lock and institutional enforcement. The sibling readings treat the same ritual as a current survival-coordination mechanism (rope/tangled_rope) or as a mourning mechanism (rope). The three stories share the same observational domain but diverge in what they claim the ritual actually does. Each story has its own ε, beneficiary/victim structure, and classification. The network links allow the contamination system to recognize that these are competing readings of the same kernel, not independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hybrid_atrophy_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
