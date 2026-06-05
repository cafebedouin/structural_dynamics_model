% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__principled_intervention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__principled_intervention_reading, []).

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
 *   constraint_id: constitutional_secularism__principled_intervention_reading
 *   human_readable: Constitutional Secularism: Principled State Intervention in Religious Affairs for Social Reform
 *   domain: constitutional_law/religious_governance/political_theory
 *
 * SUMMARY:
 *   The principled intervention reading of constitutional secularism asserts
 *   that the state may legitimately intervene in religious community affairs
 *   when necessary to advance social reform and protect members from internal
 *   oppression. This is ONE reading of a contested constitutional kernel —
 *   the meaning and scope of 'secular governance.' The sibling readings
 *   (strict neutrality: state must abstain from all religious domain
 *   intervention; reformist: state should actively reshape religious
 *   institutions toward modern values) represent different foundational
 *   commitments about the state's proper role and the legitimacy sources for
 *   defining reform. The principled intervention reading occupies a middle
 *   ground: intervention is permitted when grounded in protecting
 *   constitutional rights and supporting internal reform movements, but is
 *   presumptively limited — the state acts as facilitator for community
 *   self-transformation, not as architect of religious practice. The
 *   constraint exhibits classic tangled rope characteristics: genuine
 *   coordination function (the state's intervention can support internal
 *   reformist movements and prevent destabilization from internal conflict),
 *   but also authentic asymmetric extraction (the state's power to define
 *   'protection' and 'reform' can be captured by majoritarian preferences,
 *   and the religious community's autonomy over its own meaning-making is
 *   structurally diminished). The measurements track how this constraint has
 *   evolved over 30 years in constitutional democracies, particularly India:
 *   base extractiveness rising (state intervention scope expanding beyond
 *   initial protections), theater rising (justificatory language increasingly
 *   elaborate as intervention scope widens), and suppression requirement
 *   rising (enforcement mechanisms becoming more coercive as internal
 *   resistance grows).
 *
 * KEY AGENTS:
 *   - Weaker sections within religious communities (women, lower castes, dissidents within faith): powerless/trapped — primary nominal beneficiaries but lack agency in defining protection; structurally vulnerable to both community and state authority
 *   - Religious community elders and institutional leadership (custodians of doctrine, tradition): moderate-to-institutional/constrained — experience extraction of interpretive authority and autonomy; also benefit from state protection against fragmentation
 *   - Constitutional modernization project (reform-minded judges, legislators, activists): powerful/mobile — view intervention as temporary catalytic mechanism; operate with significant agency and ability to shift strategies
 *   - Internal reformist factions (indigenous reform movements within community): organized/mobile — benefit from state support but risk hollowing of legitimacy; experience extraction when state claims credit
 *   - State authority (judiciary, executive, legislative branches): institutional/arbitrage — primary beneficiary of expanded authority; experiences low cost through ability to modulate enforcement based on political conditions
 *   - Majoritarian religious communities: institutional/mobile — present as neutral stakeholders but can capture intervention mechanisms to suppress minority practices under reform framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, 0.58).
domain_priors:suppression_score(constitutional_secularism__principled_intervention_reading, 0.68).
domain_priors:theater_ratio(constitutional_secularism__principled_intervention_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__principled_intervention_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__principled_intervention_reading, "Constitutional Secularism: Principled State Intervention in Religious Affairs for Social Reform").
narrative_ontology:topic_domain(constitutional_secularism__principled_intervention_reading, "constitutional_law/religious_governance/political_theory").

domain_priors:requires_active_enforcement(constitutional_secularism__principled_intervention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__principled_intervention_reading, '277b24cf-cec9-4f12-8f56-268f0ee48c2a').
narrative_ontology:cs_kernel_codification('277b24cf-cec9-4f12-8f56-268f0ee48c2a', formalized).
narrative_ontology:cs_authority_grounding('277b24cf-cec9-4f12-8f56-268f0ee48c2a', extraction).
narrative_ontology:cs_interpretation_layer_present('277b24cf-cec9-4f12-8f56-268f0ee48c2a').
narrative_ontology:cs_reading_relation('277b24cf-cec9-4f12-8f56-268f0ee48c2a', constitutional_secularism__strict_neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('277b24cf-cec9-4f12-8f56-268f0ee48c2a', constitutional_secularism__reformist_reading, influences).
narrative_ontology:cs_axiom('277b24cf-cec9-4f12-8f56-268f0ee48c2a', foundational, state_intervention_legitimacy_requires_internal_reform_support).
narrative_ontology:cs_axiom_status(state_intervention_legitimacy_requires_internal_reform_support, holdable).
narrative_ontology:cs_axiom_grounding('277b24cf-cec9-4f12-8f56-268f0ee48c2a', state_intervention_legitimacy_requires_internal_reform_support, deontological).
narrative_ontology:cs_axiom('277b24cf-cec9-4f12-8f56-268f0ee48c2a', foundational, religious_community_interpretive_autonomy_presumption).
narrative_ontology:cs_axiom_status(religious_community_interpretive_autonomy_presumption, overridden).
narrative_ontology:cs_axiom_grounding('277b24cf-cec9-4f12-8f56-268f0ee48c2a', religious_community_interpretive_autonomy_presumption, deontological).
narrative_ontology:cs_reference_frame('277b24cf-cec9-4f12-8f56-268f0ee48c2a', principled_constitutional_intervention_authority).
narrative_ontology:cs_drift_state('277b24cf-cec9-4f12-8f56-268f0ee48c2a', contemporary_majoritarian_pressure_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('277b24cf-cec9-4f12-8f56-268f0ee48c2a', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__principled_intervention_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, weaker_religious_minorities).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, reformist_state_authority).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, constitutional_modernization_project).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, religious_community_autonomy).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, doctrinal_traditionalists).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, majoritarian_capture_susceptibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WEAKER SECTION (SNARE) — Cannot exit the religious community without severe identity/social cost. State intervention is framed as liberatory but operates as external imposition. Beneficiaries in name (protection, legal standing) but lack meaningful agency in how protection is defined or applied. Trapped by both community authority and state action.
constraint_indexing:constraint_classification(constitutional_secularism__principled_intervention_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RELIGIOUS COMMUNITY LEADERSHIP (TANGLED ROPE) — Genuine coordination function: the state's intervention creates conditions for internal reform discussions and modernization pressures. Also authentic extraction: authority over doctrinal interpretation is transferred to state/courts. Cost of non-compliance is high but not zero — community has limited exit (relocation, diaspora, underground practice) at significant cost.
constraint_indexing:constraint_classification(constitutional_secularism__principled_intervention_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL REFORMERS (SCAFFOLD) — View intervention as temporary catalytic mechanism to align religious practice with constitutional values. Assume this role will sunset as community norms internalize reform (generational time horizon). High agency, mobile exit (can shift to voluntary compliance mechanisms). Theater present but functional: explicit justification for intervention creates accountability.
constraint_indexing:constraint_classification(constitutional_secularism__principled_intervention_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL RELIGIOUS AUTHORITY (TANGLED ROPE) — From the institutional perspective: genuine coordination function (state protection from internal schism, legal enforcement of institutional interests). Also authentic extraction: state intervention in doctrinal matters increases institutional vulnerability to majoritarian pressure and reduces autonomy. Constrained by constitutional commitments and public opinion; cannot simply reject the constraint.
constraint_indexing:constraint_classification(constitutional_secularism__principled_intervention_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE AUTHORITY (ROPE) — From the state's perspective: this is pure coordination. Intervention establishes enforceable standards, prevents community conflict from destabilizing public order, creates opportunity for demonstrating constitutional commitment to reform. Significant benefit (expanded authority, political legitimacy from reform narrative). Low cost through arbitrage — state can shift enforcement intensity based on political conditions.
constraint_indexing:constraint_classification(constitutional_secularism__principled_intervention_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNAL REFORMISTS (TANGLED ROPE) — Benefit from state support for reform agenda (coordination with external authority). Also constrained: reliance on state backing can hollow out indigenous reform legitimacy and create appearance of external imposition. Organized exit available (can redirect reform narrative away from state intervention). Significant extraction from state's ability to claim credit for reform.
constraint_indexing:constraint_classification(constitutional_secularism__principled_intervention_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / LEGAL NATURALISM (MOUNTAIN) — From a civilizational perspective, the constraint appears immutable: constitutional law inherently requires adjudication of religious practice against rights frameworks; state authority necessarily involves some oversight of community norms; there is no escape from the tension between religious autonomy and constitutional standards. This perspective risks false summarization — naturalizes what is actually a contingent institutional choice about when and how to intervene.
constraint_indexing:constraint_classification(constitutional_secularism__principled_intervention_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__principled_intervention_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_secularism__principled_intervention_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_secularism__principled_intervention_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__principled_intervention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The state's expansion into religious domain creates genuine resource and autonomy extraction from communities — interpretive authority over doctrine is transferred to state institutions (courts primarily). However, extractiveness is not maximal because: (1) the framework genuinely supports internal reformist movements, (2) communities retain significant autonomous authority in practice, (3) the intervention mechanism creates accountability (explicit justification required, subject to judicial review), (4) the coordination benefit (preventing internal destabilization, enabling reform) is partially real. The rising trajectory (0.38→0.58 over 30 years) reflects growing scope of intervention and increasing majoritarian capture of the framework. Suppression (0.68): Moderate-high. Significant barriers to exit and contestation: (a) religious identity is typically not chosen (born into), exit costs are severe (family rupture, identity loss, social stigma), (b) state monopoly on enforcement (communities cannot rely on counter-authority to resist), (c) legal systems may prohibit practices that communities view as core to identity. However, suppression is not maximal because communities retain substantial practical autonomy in less-visible domains, enforcement is episodic rather than continuous, and legal channels exist for contestation. Theater ratio (0.62): Moderate-high. The principled intervention framework requires elaborate justification — courts must articulate why intervention is necessary, how it protects constitutional values, why community autonomy is overridden. This justificatory requirement creates functional accountability but also serves theater function: the detailed reasoning can obscure majoritarian capture ('we are protecting weaker sections' when the actual intervention prioritizes majority religious preferences). Rising trajectory (0.48→0.62) reflects increasing detachment between stated justifications and actual patterns of intervention.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival divergence in this constraint runs between the state's experience (Rope: pure coordination, low cost, high benefit through expanded authority) and the weaker section's experience (Snare: no exit, full extraction cost, trapped between community and state). The religious community's experience (Tangled Rope) sits between these extremes — genuine coordination benefit but also authentic extraction. The scaffold perspective (constitutional reformers) views the intervention as temporary, suggesting a sunset mechanism; but the rising measurements undermine this — if the mechanism were truly catalytic and sunset-oriented, suppression requirements should be declining (as norms internalize), not rising. The analytical observer risks naturalizing the constraint as an immutable feature of constitutional law (Mountain), but the structural data reveals this as a false summit: the tension between religious autonomy and state authority is real, but the specific configuration of state intervention is a contingent institutional choice, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is computed from their structural position relative to the constraint. Weaker sections: beneficiary status (nominally protected by state intervention) conflicts with victim status (extraction of autonomy by both state and community); power is powerless; exit is trapped → high d (0.90+) → high f(d) → high experienced χ. Religious community leadership: complex position (beneficiary through state protection against fragmentation; victim through loss of interpretive authority); institutional power; constrained exit → moderate d (0.50-0.60) → moderate f(d) → moderate χ. State authority: primary beneficiary (expanded jurisdiction, political legitimacy); institutional power; arbitrage exit (can shift enforcement) → low d (0.20-0.30) → low/negative f(d) → negative χ (state experiences this as net benefit/coordination). Internal reformists: ambiguous position (benefit from state support; victimized by state claiming credit); organized power; mobile exit → moderate d (0.45-0.55) → moderate f(d) → moderate χ. The perspectival gap emerges because d values diverge significantly: state experiences low d (beneficiary + arbitrage) while community experiences high d (mixed beneficiary/victim + trapped/constrained). This gap is the signature of the tangled rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves potential mandatrophy between tangled_rope and snare by showing that the classification depends on which agent's perspective is centered. From the state's perspective: rope (pure coordination, no extraction cost). From the weaker section's perspective: snare (maximum extraction, no exit). From the community's perspective: tangled rope (mixed coordination benefit and extraction cost). The mandatrophy is not 'which type is correct?' but 'which structural position are you measuring from?' The rising measurements (extractiveness, suppression, theater) signal a drift toward snare as majoritarian capture increases. A genuine resolution would require: (1) explicit sunset criteria with evidence of internalization, (2) empirical tracking showing intervention rates track vulnerability rather than majoritarian preference, (3) dominance of internal (vs. state-imposed) sourcing for reform definition. The constraint currently remains in tangled rope territory but shows drift indicators toward snare if majoritarian capture accelerates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intervention_threshold_specification,
    'What practices constitute ''protection of weaker sections'' requiring intervention vs. internal community matters beyond state authority?',
    'Comparative analysis across constitutional democracies (India, France, secular Turkey, etc.); documentation of explicit threshold criteria in jurisprudence; tracking of case-by-case threshold drift over time',
    'If threshold is narrow (only slavery-equivalent practices): constraint is closer to Rope (narrow coordination gate). If threshold is expansive (all practices deemed ''harmful''): constraint shifts toward Snare (extensive extraction of autonomy). Threshold ambiguity is the primary vector for majoritarian capture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intervention_threshold_specification, conceptual, 'Ambiguity in defining which religious practices require state intervention').

omega_variable(
    majoritarian_capture_risk,
    'Does the principled intervention framework insulate weaker religious minorities from majoritarian religious establishment, or does it enable majoritarian authority to suppress minority practices under the guise of ''reform''?',
    'Empirical tracking: composition of state interpreters (judicial, executive), religious demography, pattern of interventions across majority vs. minority communities; test whether intervention rate is proportional to vulnerability or proportional to majority religious preferences',
    'If interventions track vulnerability: framework protects minorities (tangled rope holding). If interventions track majoritarian preference: framework becomes mechanism for majority domination (snare). This is the critical empirical test of whether ''principled intervention'' is coherent or naturalizes majoritarian capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_capture_risk, empirical, 'Whether principled intervention prevents or enables majoritarian religious capture').

omega_variable(
    reform_legitimacy_sourcing,
    'Who defines what constitutes ''reform'' and ''weaker sections'' — the state, the community''s internal reformist movements, external human rights standards, or some combination? And does this sourcing shift over time?',
    'Genealogical analysis of intervention justifications; tracking which voices are centered in defining ''reform''; comparison with reform movements internal to the community emerging from within vs. imposed from state/external sources; longitudinal study of legitimacy sources',
    'If internal reformists define reform: state role is supporting function (scaffold logic). If state/external sources define reform: state role is imposing function (snare for community, rope for state). Sourcing drift is a key mandatrophy signal — externally-defined reform naturally migrates toward extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_legitimacy_sourcing, empirical, 'Sourcing of ''reform'' definition — internal vs. state-imposed').

omega_variable(
    sibling_reading_empirical_divergence,
    'In practice, does principled intervention reading diverge from strict_neutrality_reading and reformist_reading in measurable ways, or do they collapse into the same institutional outcome?',
    'Comparative empirical analysis across jurisdictions: measure intervention rates, patterns of enforcement, composition of beneficiaries/victims under each reading framework; test whether doctrine predicts behavior or merely provides post-hoc justification',
    'If readings produce empirically distinct outcomes: this reading is a real structural alternative with independent validity. If readings converge on the same extraction pattern: the reading distinction is performative (theater), and all three readings instantiate the same underlying constraint type (likely tangled_rope or snare depending on intensity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_empirical_divergence, empirical, 'Whether principled intervention produces distinct outcomes from rival readings').

omega_variable(
    sunset_vs_permanent_architecture,
    'Is the state intervention mechanism designed as temporary catalytic intervention (with explicit sunset pathways) or as permanent institutional expansion into religious domain?',
    'Doctrinal analysis: does the framework include explicit sunset criteria or transition mechanisms toward greater community autonomy? Empirical tracking: have any interventions been wound down or returned to community authority? Generational timeline: what evidence exists that norms internalization is occurring?',
    'If genuinely sunset-oriented: closer to scaffold logic (temporary support with withdrawal condition). If permanent: tangled_rope or snare, depending on whether community experiences genuine benefit from coordination or only extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_vs_permanent_architecture, empirical, 'Whether intervention is temporary catalytic mechanism or permanent institutional expansion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__principled_intervention_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cspi_tr_t0, constitutional_secularism__principled_intervention_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cspi_tr_t15, constitutional_secularism__principled_intervention_reading, theater_ratio, 15, 0.57).
narrative_ontology:measurement(cspi_tr_t30, constitutional_secularism__principled_intervention_reading, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(cspi_be_t0, constitutional_secularism__principled_intervention_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cspi_be_t15, constitutional_secularism__principled_intervention_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(cspi_be_t30, constitutional_secularism__principled_intervention_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cspi_su_t0, constitutional_secularism__principled_intervention_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(cspi_su_t15, constitutional_secularism__principled_intervention_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(cspi_su_t30, constitutional_secularism__principled_intervention_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__principled_intervention_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, constitutional_secularism__reformist_reading).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, judicial_review_scope_expansion).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, religious_minority_protection_mechanisms).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-way kernel decomposition (constitutional_secularism kernel). Principled intervention reading (this file) influences both strict_neutrality and reformist readings through the mechanism of explicit justification requirements and scope expansion. The sibling readings are separate constraint stories with different ε values, beneficiary/victim structures, and measurement profiles. The network effects run bidirectional: as principled intervention scope expands without sunset evidence, it exerts pressure on strict_neutrality reading (making neutrality appear impossible) and on reformist reading (making reformation appear inevitable).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_secularism__principled_intervention_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
