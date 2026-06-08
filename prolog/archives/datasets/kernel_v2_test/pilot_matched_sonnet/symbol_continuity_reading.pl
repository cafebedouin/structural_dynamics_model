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
 *   constraint_id: symbol_continuity_reading
 *   human_readable: Ritual as Symbolic Continuity Mechanism (Symbol Continuity Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint story models ritual practice as a mechanism for
 *   preserving symbolic continuity and collective identity across
 *   generational time. It is ONE READING of the catastrophe_memory_kernel —
 *   the contested question of how communities encode, transmit, and maintain
 *   memory of catastrophic events. The symbol_continuity_reading frames
 *   ritual as primarily a coordination mechanism for identity transmission,
 *   where the beneficiary is tradition-continuity itself and the victim is
 *   adaptive modification capacity (ritual rigidity imposes costs when
 *   contexts change). This reading contrasts with survival_competence_reading
 *   (which would frame ritual as operational training for catastrophe
 *   response, with different beneficiaries and victims) and
 *   trauma_encoding_reading (which would frame ritual as psychological
 *   re-traumatization mechanism). The constraint exhibits low extractiveness
 *   (0.18) because most participants are net beneficiaries of identity
 *   anchoring, but non-zero extraction exists: identity-locked members cannot
 *   exit without self-dissolution, and adaptive modification capacity bears
 *   real costs when ritual forms ossify. Theater ratio (0.42) reflects that
 *   substantial ritual activity is performative maintenance of forms whose
 *   original functional context has been lost, but the performance itself
 *   serves the coordination function (symbolic continuity requires repetition
 *   even when meanings shift). Suppression (0.35) is moderate: social penalty
 *   for non-participation exists but is not severe in most contemporary
 *   contexts; the primary binding mechanism for high-extraction cases is
 *   identity fusion (cognitive lock) rather than structural coercion.
 *
 * KEY AGENTS:
 *   - Individual Practitioner: Mobile participant (moderate/mobile) — voluntary coordination, low extraction, benefits from identity anchoring
 *   - Religious Community: Organized collective (organized/constrained) — coordinates intergenerational transmission, moderate extraction from form-maintenance costs
 *   - Religious Authority: Institutional gatekeeper (institutional/constrained) — mixed beneficiary (legitimacy through lineage) and coordinator (transmission function)
 *   - Reform Movement: Organized adapters (organized/mobile) — see rigid ritual as temporary scaffold, seek to preserve core symbolic function while updating forms
 *   - Identity-Fused Member: Cognitive captive (powerless/identity_locked) — self-concept constituted through ritual participation, cannot exit without identity dissolution, high experienced extraction
 *   - Adaptive Modification Capacity: Abstract victim (powerless/trapped) — the community's ability to update practice in response to changing context, constrained by ritual rigidity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(symbol_continuity_reading, 0.18).
domain_priors:suppression_score(symbol_continuity_reading, 0.35).
domain_priors:theater_ratio(symbol_continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(symbol_continuity_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(symbol_continuity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(symbol_continuity_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(symbol_continuity_reading, rope).
narrative_ontology:human_readable(symbol_continuity_reading, "Ritual as Symbolic Continuity Mechanism (Symbol Continuity Reading)").
narrative_ontology:topic_domain(symbol_continuity_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(symbol_continuity_reading, 'f9f091cb-4965-44f6-9059-47f94591e6b1').
narrative_ontology:cs_kernel_codification('f9f091cb-4965-44f6-9059-47f94591e6b1', distributed).
narrative_ontology:cs_authority_grounding('f9f091cb-4965-44f6-9059-47f94591e6b1', practice).
narrative_ontology:cs_interpretation_layer_present('f9f091cb-4965-44f6-9059-47f94591e6b1').
narrative_ontology:cs_reading_relation('f9f091cb-4965-44f6-9059-47f94591e6b1', symbol_continuity_reading__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9f091cb-4965-44f6-9059-47f94591e6b1', symbol_continuity_reading__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9f091cb-4965-44f6-9059-47f94591e6b1', symbol_continuity_reading__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('f9f091cb-4965-44f6-9059-47f94591e6b1', foundational, symbolic_continuity_primacy).
narrative_ontology:cs_axiom_status(symbolic_continuity_primacy, holdable).
narrative_ontology:cs_axiom_grounding('f9f091cb-4965-44f6-9059-47f94591e6b1', symbolic_continuity_primacy, conventional).
narrative_ontology:cs_axiom('f9f091cb-4965-44f6-9059-47f94591e6b1', secondary, identity_through_repetition).
narrative_ontology:cs_axiom_status(identity_through_repetition, holdable).
narrative_ontology:cs_axiom_grounding('f9f091cb-4965-44f6-9059-47f94591e6b1', identity_through_repetition, conventional).
narrative_ontology:cs_reference_frame('f9f091cb-4965-44f6-9059-47f94591e6b1', founding_catastrophe_memory).
narrative_ontology:cs_drift_state('f9f091cb-4965-44f6-9059-47f94591e6b1', contemporary_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f9f091cb-4965-44f6-9059-47f94591e6b1', '').
narrative_ontology:cs_kernel_id(symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(symbol_continuity_reading, tradition_continuity).
narrative_ontology:constraint_beneficiary(symbol_continuity_reading, collective_identity_maintenance).
narrative_ontology:constraint_beneficiary(symbol_continuity_reading, intergenerational_transmission).
narrative_ontology:constraint_victim(symbol_continuity_reading, adaptive_modification_capacity).
narrative_ontology:constraint_victim(symbol_continuity_reading, contextual_responsiveness).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(symbol_continuity_reading, individual_practitioner).
narrative_ontology:constraint_beneficiary(symbol_continuity_reading, religious_community).
narrative_ontology:constraint_beneficiary(symbol_continuity_reading, religious_authority).
narrative_ontology:constraint_victim(symbol_continuity_reading, identity_fused_member).
narrative_ontology:constraint_vindicates(symbol_continuity_reading, symbolic_transmission_primacy).
narrative_ontology:constraint_vindicates(symbol_continuity_reading, identity_through_repetition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participates in ritual practice voluntarily. Gains identity anchoring, community belonging, and connection to tradition. Can choose non-participation without severe penalty in most contemporary contexts. Benefits outweigh costs.
narrative_ontology:constraint_stakeholder(symbol_continuity_reading, individual_practitioner, beneficiary,
    moderate, biographical, mobile, local).

% Organized collective that coordinates intergenerational transmission of symbolic identity. Benefits from continuity and cohesion. Bears costs of maintaining ritual forms even when functional contexts erode. Constrained exit because community cohesion depends on shared practice.
narrative_ontology:constraint_stakeholder(symbol_continuity_reading, religious_community, beneficiary,
    organized, generational, constrained, regional).

% Institutional structure that sets and enforces ritual correctness. Benefits from legitimacy through lineage and gatekeeping authority. Also coordinates genuine transmission function. Mixed position: both runs the arrangement and collects from it.
narrative_ontology:constraint_stakeholder(symbol_continuity_reading, religious_authority, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(symbol_continuity_reading, religious_authority, beneficiary).

% Organized agents seeking to preserve core symbolic function while adapting forms. Sees rigid ritual as temporary support structure. Mobile exit into alternative practice modes. Neither primarily collecting nor paying — observing and proposing alternatives.
narrative_ontology:constraint_stakeholder(symbol_continuity_reading, reform_movement, observer,
    organized, generational, mobile, regional).

% Individual whose self-concept is constituted through ritual participation. Cannot exit without identity dissolution. Bears high costs when ritual demands conflict with life circumstances but cannot leave. The binding is cognitive rather than structural — has material mobility but psychological lock.
narrative_ontology:constraint_stakeholder(symbol_continuity_reading, identity_fused_member, payer,
    powerless, biographical, identity_locked, local).

% Abstract capacity (not an agent) representing the community's ability to update practice in response to changing contexts. Constrained by ritual rigidity. When ritual forms ossify, the community's operational fitness is reduced. This is a non-agent entry kept for narrative completeness — excluded from beneficiary/victim derivation per agent:false flag.
narrative_ontology:constraint_stakeholder(symbol_continuity_reading, adaptive_modification_capacity, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_non_agent(symbol_continuity_reading, adaptive_modification_capacity).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual coordinates the transmission of complex symbolic identity systems across generational time. Without shared practice, symbolic meanings fragment and collective identity erodes. The coordination problem is real: how to preserve continuity when no individual lives long enough to span the transmission chain.
% TRANSFER_FUNCTION: Ritual moves attention, time, and resources from individual autonomy and adaptive flexibility toward collective identity maintenance and symbolic continuity. From individual practitioners to tradition-continuity (abstract beneficiary). From adaptive modification capacity to form-preservation.
% ABSENT_VOICES: Those who left the tradition (apostates, reformers who fully exited) are not in the room. Their perspective — that rigid ritual imposes unsustainable costs — is excluded from the authority structure's deliberations. Also absent: future generations who will inherit ossified forms without the functional contexts that originally justified them.
% DISAPPEARANCE_RATIONALE: If ritual practice disappeared overnight, the community's symbolic identity transmission would collapse. Intergenerational continuity depends on shared practice. The world rearranges: without ritual, the community either fragments into disconnected individuals or reconstructs alternative transmission mechanisms (which would be different constraints).
% FOUNDING_PROBLEM: The founding problem is generational discontinuity: how to transmit complex symbolic meanings and collective identity when no individual spans the full transmission chain. Ritual solves this by creating repeated, embodied practice that encodes meanings in gesture, language, and calendrical rhythm.
% FOUNDING_PROBLEM_CORROBORATION: The generational transmission problem remains live in all communities that seek to preserve identity across time. Corroborated by: (1) anthropological literature on ritual function across cultures (Rappaport, Turner, Bell), (2) practitioner testimony from diverse religious traditions about the necessity of shared practice for continuity, (3) historical cases of identity fragmentation when ritual practice was disrupted (forced assimilation, diaspora, persecution). The problem is not unique to any single tradition — it is a structural feature of intergenerational identity maintenance.
narrative_ontology:disappearance_verdict(symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(symbol_continuity_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL PRACTITIONER (ROPE) — Experiences ritual as voluntary coordination mechanism for maintaining connection to tradition and community. Mobile exit options (can choose non-participation without severe penalty in most contemporary contexts). Benefits from identity anchoring and community belonging. Low extraction.
constraint_indexing:constraint_classification(symbol_continuity_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: RELIGIOUS COMMUNITY (ROPE) — Organized collective sees ritual as coordination solution to the problem of transmitting identity across generations. Constrained exit (community cohesion depends on shared practice) but genuine coordination function. Moderate extraction from need to maintain forms even when meanings shift.
constraint_indexing:constraint_classification(symbol_continuity_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RELIGIOUS AUTHORITY (TANGLED ROPE) — Institutional actors coordinate symbolic transmission but also extract authority from gatekeeping ritual correctness. Benefits from continuity (legitimacy through lineage) while constraining adaptive modification. Mixed coordination and extraction.
constraint_indexing:constraint_classification(symbol_continuity_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM MOVEMENT (SCAFFOLD) — Organized agents seeking to preserve core symbolic function while adapting forms to contemporary context. Sees rigid ritual as temporary support structure that should sunset as new forms emerge. Mobile exit into alternative practice modes.
constraint_indexing:constraint_classification(symbol_continuity_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: IDENTITY-FUSED MEMBER (SNARE) — Individual whose self-concept is constituted through ritual participation. Identity-locked exit (leaving would require becoming a different person). Experiences high extraction when ritual demands conflict with life circumstances but cannot exit without identity dissolution. The binding is cognitive rather than structural.
constraint_indexing:constraint_classification(symbol_continuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From civilizational perspective, ritual solves genuine coordination problem of transmitting complex symbolic systems across time. Low extraction in aggregate (most participants are net beneficiaries of identity continuity). The constraint coordinates more than it extracts.
constraint_indexing:constraint_classification(symbol_continuity_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(symbol_continuity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(symbol_continuity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(symbol_continuity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low but non-zero. Most ritual participants are net beneficiaries (identity continuity, community belonging, meaning-making). Extraction exists in two forms: (1) identity-locked members who cannot exit when ritual demands conflict with life circumstances, and (2) adaptive modification capacity — the community's operational fitness is constrained when ritual forms ossify and cannot respond to changing contexts. The low value reflects that symbolic transmission is relatively low-cost coordination for most participants. Suppression (0.35): Moderate. Social penalty for non-participation exists (community disapproval, family pressure, status loss) but is not severe in most contemporary contexts. The primary suppression mechanism for high-extraction cases is cognitive (identity fusion) rather than structural (material barriers). Exit is structurally possible but psychologically costly for identity-locked members. Theater ratio (0.42): Moderate. Substantial ritual activity is performative maintenance of forms whose original functional meanings have been lost (liturgical languages no longer understood, gestures whose referents are forgotten, calendrical observances detached from agricultural cycles). But the performance itself serves the coordination function — symbolic continuity requires repetition even when meanings shift. The theater is not pure waste; it is the mechanism of transmission. The ratio has increased over the interval as modernization has eroded functional contexts while preserving ritual forms.
 *
 * PERSPECTIVAL GAP:
 *   The individual practitioner sees pure coordination (Rope) — voluntary participation, clear benefits, mobile exit. The religious community sees coordination with moderate costs (Rope) — form-maintenance is burdensome but necessary for transmission. The religious authority sees mixed coordination and extraction (Tangled Rope) — legitimacy through gatekeeping ritual correctness. The reform movement sees temporary support structure (Scaffold) — rigid forms should sunset as new adaptive forms emerge. The identity-fused member sees extraction (Snare) — cannot exit without self-dissolution, bears costs when ritual conflicts with life. The analytical observer sees aggregate coordination (Rope) — most participants benefit, extraction is localized to identity-locked cases. The gap reveals that the same ritual structure coordinates for some and extracts from others, depending on the agent's identity relationship to the practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (tradition_continuity, collective_identity_maintenance, intergenerational_transmission) are abstract collective goods rather than individual actors, but they represent real coordination functions that most participants benefit from. The individual practitioner and religious community are net beneficiaries (low d, low/negative chi). The religious authority structure is mixed (moderate d, moderate chi) — benefits from legitimacy through lineage but also bears costs of maintaining forms. Victims (adaptive_modification_capacity, contextual_responsiveness) are also abstract but represent real costs: communities with high ritual rigidity show reduced adaptive capacity during environmental or social disruption. The identity-fused member is the clearest individual victim (high d, high chi) — structurally mobile but cognitively locked, bearing extraction through inability to exit when ritual demands conflict with life circumstances. The analytical observer sees low aggregate extraction because most participants are voluntary and net-positive.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that ritual can simultaneously coordinate (preserve identity, transmit meaning, anchor community) and extract (constrain adaptation, lock identities, impose form-maintenance costs). The coordination function is genuine — symbolic continuity across generations is a real collective-action problem that ritual solves. The extraction is also genuine — adaptive modification capacity bears real costs, and identity-locked members cannot exit. The constraint is not misclassified coordination (it really does coordinate) or misclassified extraction (it really does extract from some agents). It is a Tangled Rope from institutional perspectives and a Rope from most individual perspectives, with localized Snare experiences for identity-locked members. The mandatrophy resolution is that both functions coexist in the same structure, experienced differently by different agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is this constraint one reading of the catastrophe_memory_kernel, where different communities read mourning-practice differently?',
    'Cross-reading structural comparison: does survival_competence_reading assign different beneficiaries (operational survival capacity vs symbolic continuity)? Does trauma_encoding_reading locate extraction differently (psychological re-traumatization vs adaptive rigidity)?',
    'If readings are structurally distinct: the kernel decomposes into multiple constraints with different ε values, linked by network.affects_constraints. If readings collapse: the apparent kernel is actually a single constraint with observer-dependent framing (not a true kernel).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Whether this is one reading of a contested kernel or a standalone constraint').

omega_variable(
    symbolic_vs_operational_primacy,
    'Does ritual preserve symbolic continuity at the expense of operational adaptation, or does symbolic preservation enable operational resilience?',
    'Longitudinal community studies: do communities with high ritual rigidity show better or worse adaptive capacity during environmental/social disruption? Correlation between ritual maintenance and community survival rates across multiple catastrophe types.',
    'If symbolic preservation enables resilience: extractiveness is lower (the rigidity cost is offset by adaptive benefit). If symbolic preservation impedes adaptation: extractiveness is higher (the constraint trades operational fitness for identity continuity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_operational_primacy, empirical, 'Whether symbolic continuity supports or impedes operational adaptation').

omega_variable(
    identity_lock_prevalence,
    'What proportion of ritual participants are identity-locked vs mobile? Does the constraint''s aggregate extractiveness depend on this distribution?',
    'Survey data on exit costs: self-reported identity fusion, social penalty estimates, actual exit rates. Cross-cultural comparison of ritual participation patterns in high-choice vs low-choice environments.',
    'If most participants are mobile: aggregate extractiveness is low (voluntary coordination). If substantial fraction is identity-locked: aggregate extractiveness is higher (the constraint binds some agents cognitively even when structural barriers are low).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_prevalence, empirical, 'Distribution of identity-locked vs mobile participants').

omega_variable(
    adaptive_modification_cost,
    'How much operational fitness does ritual rigidity actually cost? Is the victim (adaptive_modification_capacity) bearing significant extraction or negligible cost?',
    'Historical case studies of communities that modified ritual vs those that maintained strict continuity during environmental/social change. Measure survival rates, resource efficiency, social cohesion outcomes.',
    'If modification cost is high: the constraint is more extractive than claimed (tradition continuity comes at real adaptive expense). If modification cost is low: the constraint is closer to pure coordination (symbolic continuity is nearly free).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptive_modification_cost, empirical, 'Magnitude of adaptive cost from ritual rigidity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(symbol_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(symcont_theater_founding, symbol_continuity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(symcont_theater_mid, symbol_continuity_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(symcont_theater_contemporary, symbol_continuity_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(symcont_extract_founding, symbol_continuity_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(symcont_extract_mid, symbol_continuity_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(symcont_extract_contemporary, symbol_continuity_reading, base_extractiveness, 100, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(symcont_suppress_founding, symbol_continuity_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(symcont_suppress_mid, symbol_continuity_reading, suppression_requirement, 50, 0.28).
narrative_ontology:measurement(symcont_suppress_contemporary, symbol_continuity_reading, suppression_requirement, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(symbol_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(symbol_continuity_reading, survival_competence_reading).
narrative_ontology:affects_constraint(symbol_continuity_reading, trauma_encoding_reading).
narrative_ontology:affects_constraint(symbol_continuity_reading, boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_kernel decomposes into four structurally distinct readings, each with its own beneficiary/victim structure and extractiveness value. This story (symbol_continuity_reading) models ritual as symbolic transmission mechanism. The sibling readings model the same ritual practices as operational training (survival_competence_reading), psychological re-traumatization (trauma_encoding_reading), or boundary enforcement (boundary_maintenance_reading). The readings are linked by network.affects_constraints because they describe the same observable ritual practices but assign different structural relationships and different extraction flows.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
