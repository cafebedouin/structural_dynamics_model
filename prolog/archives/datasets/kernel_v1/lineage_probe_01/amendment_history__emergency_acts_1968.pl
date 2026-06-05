% ============================================================================
% CONSTRAINT STORY: amendment_history__emergency_acts_1968
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_amendment_history__emergency_acts_1968, []).

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
 *   constraint_id: amendment_history__emergency_acts_1968
 *   human_readable: 1968 Emergency Constitution: Sovereignty via Crisis Powers
 *   domain: political/legal/constitutional_amendment
 *
 * SUMMARY:
 *   The 1968 emergency constitution (the 'Notstandsverfassung') amended the
 *   Basic Law to provide explicit emergency powers to the executive and joint
 *   legislative committees during declared states of 'tension' and 'defense.'
 *   This constraint represents one reading of a contested constitutional
 *   kernel — the sovereignty kernel that defines who holds authority to bind
 *   the state and under what conditions that authority can be expanded. The
 *   emergency acts were written into the constitution against fierce protest
 *   from extra-parliamentary movements, intellectuals, and the left, who saw
 *   the emergency provisions as replicating Weimar's death spiral. They also
 *   formally ended the Allied reserved rights over West German security that
 *   had persisted since 1949, replacing external constraint with internal
 *   constitutional authorization. The structural delta is sharp: suppression
 *   of normal legislative procedure in declared crises (high suppression),
 *   genuine governmental coordination benefit (beneficiary is executive
 *   capacity in crisis), victim set is extra-parliamentary opposition and
 *   normal procedure itself, extractiveness bounded by joint-committee
 *   oversight design. The constraint is a tangled_rope: it coordinates rapid
 *   response to border and defense crises (genuine coordination function)
 *   while asymmetrically expanding executive power relative to opposition
 *   (extraction). Theater ratio is moderate (0.48) because the
 *   joint-committee design provides real constraint, not pure performance —
 *   but the constraint's legitimacy discourse (sovereignty, necessity, state
 *   preservation) carries theatrical weight beyond the procedural reality.
 *
 * KEY AGENTS:
 *   - Governmental Executive Capacity: Primary beneficiary (institutional/arbitrage) — gains crisis-response flexibility and sovereign authority over security matters previously reserved to Allies
 *   - Extra-Parliamentary Opposition: Primary victim (powerless/trapped) — faces suppression of normal dissent channels during declared emergencies; cannot exit or contest via conventional procedures
 *   - Parliamentary Majority Coalition: Secondary actor (organized/constrained) — benefits from executive flexibility but constrained by joint-committee requirement; asymmetrically advantaged over minority
 *   - Constitutional Rechtsstaat Tradition: Institutional actor (institutional/identity_locked) — fused with rule-of-law identity; cannot fully exit emergency framework without abandoning legitimacy claims that justify it
 *   - Allied Powers: Structural beneficiary (institutional/arbitrage) — formally replaced by constitutional authority, but informal leverage persists; external constraint replaced with internal authorization that may serve same function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating contingent emergency authorization as immutable sovereignty property
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(amendment_history__emergency_acts_1968, 0.38).
domain_priors:suppression_score(amendment_history__emergency_acts_1968, 0.62).
domain_priors:theater_ratio(amendment_history__emergency_acts_1968, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(amendment_history__emergency_acts_1968, extractiveness, 0.38).
narrative_ontology:constraint_metric(amendment_history__emergency_acts_1968, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(amendment_history__emergency_acts_1968, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(amendment_history__emergency_acts_1968, tangled_rope).
narrative_ontology:human_readable(amendment_history__emergency_acts_1968, "1968 Emergency Constitution: Sovereignty via Crisis Powers").
narrative_ontology:topic_domain(amendment_history__emergency_acts_1968, "political/legal/constitutional_amendment").

domain_priors:requires_active_enforcement(amendment_history__emergency_acts_1968).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(amendment_history__emergency_acts_1968, '22c99c89-49a6-4b8e-bff3-d9473acfdede').
narrative_ontology:cs_kernel_codification('22c99c89-49a6-4b8e-bff3-d9473acfdede', formalized).
narrative_ontology:cs_authority_grounding('22c99c89-49a6-4b8e-bff3-d9473acfdede', extraction).
narrative_ontology:cs_interpretation_layer_present('22c99c89-49a6-4b8e-bff3-d9473acfdede').
narrative_ontology:cs_reading_relation('22c99c89-49a6-4b8e-bff3-d9473acfdede', amendment_history__rearmament_1956, influences).
narrative_ontology:cs_reading_relation('22c99c89-49a6-4b8e-bff3-d9473acfdede', amendment_history__asylum_compromise_1993, coexists_with).
narrative_ontology:cs_reading_relation('22c99c89-49a6-4b8e-bff3-d9473acfdede', amendment_history__debt_brake_2009, coexists_with).
narrative_ontology:cs_reading_relation('22c99c89-49a6-4b8e-bff3-d9473acfdede', amendment_history__reunification_amendments_1990, influences).
narrative_ontology:cs_axiom('22c99c89-49a6-4b8e-bff3-d9473acfdede', foundational, state_emergency_self_preservation_right).
narrative_ontology:cs_axiom_status(state_emergency_self_preservation_right, holdable).
narrative_ontology:cs_axiom_grounding('22c99c89-49a6-4b8e-bff3-d9473acfdede', state_emergency_self_preservation_right, deontological).
narrative_ontology:cs_axiom('22c99c89-49a6-4b8e-bff3-d9473acfdede', foundational, parliamentary_joint_committee_suffices).
narrative_ontology:cs_axiom_status(parliamentary_joint_committee_suffices, holdable).
narrative_ontology:cs_axiom_grounding('22c99c89-49a6-4b8e-bff3-d9473acfdede', parliamentary_joint_committee_suffices, conventional).
narrative_ontology:cs_reference_frame('22c99c89-49a6-4b8e-bff3-d9473acfdede', constitutional_sovereignty_with_allied_constraint).
narrative_ontology:cs_drift_state('22c99c89-49a6-4b8e-bff3-d9473acfdede', contemporary_deferred_reunification, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('22c99c89-49a6-4b8e-bff3-d9473acfdede', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(amendment_history__emergency_acts_1968, amendment_history).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(amendment_history__emergency_acts_1968, governmental_executive_capacity).
narrative_ontology:constraint_victim(amendment_history__emergency_acts_1968, extra_parliamentary_opposition_trust).
narrative_ontology:constraint_victim(amendment_history__emergency_acts_1968, normal_legislative_procedure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXTRA-PARLIAMENTARY OPPOSITION (SNARE) — Faces suspension of normal procedure during declared emergencies; cannot exit or organize effectively under tension declarations; bears the full cost of executive discretion with minimal recourse. The emergency powers suppress their capacity to contest through conventional channels. No exit option; maximum experienced extraction.
constraint_indexing:constraint_classification(amendment_history__emergency_acts_1968, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PARLIAMENTARY MINORITY COALITION (TANGLED_ROPE) — Genuine coordination function: the joint-committee structure (emergency provision) coordinates legislative response to border crises and defense tensions. Asymmetric extraction: the majority coalition benefits more from executive flexibility than the minority, whose legislative leverage atrophies during declared emergencies. Constrained exit — can participate in joint committees but cannot block emergency declaration. Mixed coordination and extraction.
constraint_indexing:constraint_classification(amendment_history__emergency_acts_1968, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE-PARLIAMENTARY LEADERSHIP (ROPE) — Net beneficiary from emergency authority structure. Experiences emergency powers as solving the genuine coordination problem: rapid response to security crises that normal procedure cannot accommodate. The joint-committee requirement is coordination overhead, not extraction. Can arbitrage between emergency and normal procedure as needed. Low effective extraction; sees constraint as legitimate coordination.
constraint_indexing:constraint_classification(amendment_history__emergency_acts_1968, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORMERS (SCAFFOLD) — See the 1968 framework as a temporary structure awaiting sunset via reunification or full constitutional renewal (Article 146 moment). The emergency provisions are scaffolding that should dissolve once sovereignty is complete and the Basic Law is replaced by an all-German constitution ratified in full procedure. Theater low because the constraint is understood as interim. Sunset logic explicit in the Basic Law's own structure.
constraint_indexing:constraint_classification(amendment_history__emergency_acts_1968, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL STATE TRADITION / RECHTSSTAAT IDENTITY (TANGLED_ROPE, identity_locked) — The institutional framework locked into the Rechtsstaat identity, fused with rule-of-law commitments: cannot fully exit the emergency powers framework without abandoning the constitutional legitimacy that the emergency powers were meant to preserve. The Rechtsstaat tradition genuinely coordinates judicial oversight and procedural constraint, but the identity-lock prevents the tradition from fully questioning whether the emergency powers framework serves its stated purpose. Identity-locked exit: structurally mobile (could revise the framework) but identity-fused with the constraint.
constraint_indexing:constraint_classification(amendment_history__emergency_acts_1968, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective grounded in sovereignty theory: every state must retain emergency powers for existence-threatening crises. The constraint appears as an immutable structural property of statehood itself — no state can bind itself absolutely in advance for situations that threaten its continued existence. This perspective risks false summit: naturalizing what are contingent institutional arrangements (how much emergency power, what triggers, what oversight) as inherent to sovereignty itself.
constraint_indexing:constraint_classification(amendment_history__emergency_acts_1968, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(amendment_history__emergency_acts_1968_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(amendment_history__emergency_acts_1968, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(amendment_history__emergency_acts_1968, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(amendment_history__emergency_acts_1968_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, rising slowly over the interval (0.28→0.33→0.38). The emergency powers provide genuine governmental benefit (coordination), but asymmetric extraction toward the executive and majority against opposition. The rise reflects slow-motion expansion of emergency scope and frequency of declarations beyond initial intent; by t=10, the constraint has accumulated more extraction than originally designed. Suppression (0.62): Moderate-high, stable across interval (0.58→0.60→0.62). The emergency powers suppress normal dissent channels (procedures, opposition leverage) during crises, but the joint-committee requirement prevents total suppression. The slight rise reflects normalization of tension declarations and habituation to emergency procedures. Theater ratio (0.48): Moderate, stable (0.42→0.45→0.48). The constraint is neither purely performative (the joint-committee genuinely constrains) nor purely functional (the legitimacy discourse of sovereignty and necessity carries theatrical weight). The slight rise reflects increasing reliance on sovereignty rhetoric as emergency becomes normalized — the constraint's justification becomes more theatrical even as its functional role persists.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental perspectival gap runs between institutional beneficiaries and extra-parliamentary victims. The executive sees emergency powers as solving a genuine coordination problem — rapid response to border crises and defense tensions that normal procedure cannot handle (Rope). Parliamentary leadership also sees coordination, though with asymmetric benefit (Tangled Rope). But the extra-parliamentary opposition sees pure extraction — suppression of dissent channels with no beneficiary function for them (Snare). The parliamentary minority occupies the middle (Tangled Rope) — they benefit from legislative participation but lose leverage in crises. The Rechtsstaat tradition is identity-locked: it cannot question the emergency framework without abandoning the rule-of-law identity that justifies it. The analytical observer at civilizational scope risks seeing sovereignty as immutable (Mountain) when in fact the emergency authorization is a contingent institutional arrangement. The constitutional reformers see the framework as temporary scaffolding with a sunset clause (Article 146), but that sunset has never materialized — the scaffold has become permanent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position: beneficiaries of executive capacity hold low d (experiencing minimal effective extraction), while victims of suppressed dissent hold high d (experiencing maximum effective extraction). The government executive (institutional/arbitrage beneficiary) derives d≈0.15, producing negative or near-zero f(d), minimal experienced extraction. The extra-parliamentary opposition (powerless/trapped victim) derives d≈0.92, producing maximum f(d)≈1.38, maximum experienced extraction. The parliamentary majority (organized/constrained) derives d≈0.50, producing moderate f(d)≈0.65, moderate experienced extraction. The Rechtsstaat tradition (institutional/identity_locked) derives d≈0.35 based on structural tension between institutional benefit and identity-lock constraint. The canonical formula produces these values directly from the context tuple; no override is needed because the beneficiary/victim declarations and exit options capture the structural relationship accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing coordination function from extraction mechanism through the perspectival gap. The emergency powers genuinely coordinate rapid response to crises (coordination function present) while asymmetrically expanding executive power against opposition (extraction mechanism present). Both functions are real; neither is parasitic on the other. The tangled_rope classification captures this: χ=0.38×f(d)×σ(S) where f(d) varies by perspective. The joint-committee requirement proves the coordination is genuine — if pure extraction were the goal, no constraint would be needed. The expansion of emergency scope over time proves extraction is accumulating — the coordination benefit cannot explain the systematic expansion. The constraint is neither pure rope (no significant asymmetry) nor pure snare (no genuine coordination). It is hybrid: coordination mechanism with extractive overlay. This is the textbook tangled_rope structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergency_scope_creep,
    'Do declared states of ''tension'' and ''defense'' remain bounded to genuine security threats, or do they systematically expand to suppress extra-parliamentary dissent beyond security logic?',
    'Empirical analysis of emergency declarations: correlation between geopolitical threat level and scope of domestic suppression; cases where emergency declaration duration exceeds demonstrable security need',
    'If bounded: emergency powers remain coordination mechanism (Tangled Rope confirmed). If creep detected: system reveals Snare structure — domestic suppression mechanism disguised as security necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_scope_creep, empirical, 'Whether emergency scope remains bounded to genuine security threats or expands into domestic suppression').

omega_variable(
    joint_committee_effectiveness,
    'Does the joint-committee oversight structure meaningfully constrain executive emergency powers, or is it performative — a theater of constraint while executive discretion remains unchecked?',
    'Historical record of joint-committee decisions: frequency and efficacy of rejections of executive emergency proposals; cases where committee action prevented or revoked emergency authority',
    'If effective: tangled-rope classification confirmed — genuine coordination with asymmetric oversight. If performative: theater_ratio should rise significantly, and snare classification becomes more appropriate for opposition perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(joint_committee_effectiveness, empirical, 'Whether joint-committee oversight effectively constrains emergency powers or is performative').

omega_variable(
    allied_sovereignty_transfer,
    'Does constitutional emergency authority genuinely replace Allied reserved rights, or do Allied reservation mechanisms persist informally under the sovereignty cover story?',
    'Legal analysis of treaty texts and constitutional practice: explicit comparison of reserved-rights scope pre-1968 vs. post-1968; cases where informal Allied leverage was asserted despite constitutional claims to sovereignty',
    'If transfer complete: 1968 framework delivers genuine state capacity gain (beneficiary perspective confirmed). If persistence detected: sovereignty claim is performative cover for unchanged power distribution (false summit — naturalizing contingent constraint as sovereign necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allied_sovereignty_transfer, empirical, 'Whether constitutional authority genuinely replaces Allied reserved rights or persistence continues informally').

omega_variable(
    basic_law_temporality_ambiguity,
    'Is the 1968 emergency framework intended as permanent constitutional feature of a durable Basic Law, or as interim scaffolding awaiting full constitutional renewal (Article 146)?',
    'Analysis of constituent intent and constitutional text: legislative record of 1968 debates; interpretation of Article 146''s status as dormant promise vs. active commitment; subsequent constitutional amendments and their relationship to Article 146',
    'If permanent: scaffold classification is aspirational, not structural (extractiveness and theater_ratio remain high). If interim: scaffold classification is accurate, and the reading''s reference frame should privilege the reunification/Article 146 moment as the true terminal state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(basic_law_temporality_ambiguity, conceptual, 'Whether 1968 emergency framework is permanent or interim scaffolding').

omega_variable(
    kernel_reading_distinction,
    'Is the 1968 emergency constitution a reading of the sovereignty kernel (how authority to bind the state is grounded), or does it constitute a distinct constitutional commitment with its own kernel?',
    'Structural analysis: does 1968 reframe what sovereignty means (axiom revision), or does it implement a pre-existing sovereignty commitment through new procedures? Does Article 146 promise a genuine all-German constitutional moment (suggesting 1968 is interim), or is Article 146 rhetorical covering a permanent Basic Law?',
    'If reading of sovereignty kernel: the emergency act''s legitimacy depends on resolving what sovereignty means (links to rearmament_1956, reunification_amendments_1990). If distinct commitment: 1968 framework can be analyzed independently. Resolution affects whether this constraint family is a unified kernel controversy or multiple related but distinct constitutional commitments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Whether emergency powers are a reading of sovereignty kernel or a distinct constitutional commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(amendment_history__emergency_acts_1968, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emerg_1968_theater_t0, amendment_history__emergency_acts_1968, theater_ratio, 0, 0.42).
narrative_ontology:measurement(emerg_1968_theater_t5, amendment_history__emergency_acts_1968, theater_ratio, 5, 0.45).
narrative_ontology:measurement(emerg_1968_theater_t10, amendment_history__emergency_acts_1968, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(emerg_1968_extract_t0, amendment_history__emergency_acts_1968, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(emerg_1968_extract_t5, amendment_history__emergency_acts_1968, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(emerg_1968_extract_t10, amendment_history__emergency_acts_1968, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(emerg_1968_suppress_t0, amendment_history__emergency_acts_1968, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(emerg_1968_suppress_t5, amendment_history__emergency_acts_1968, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(emerg_1968_suppress_t10, amendment_history__emergency_acts_1968, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(amendment_history__emergency_acts_1968, enforcement_mechanism).
narrative_ontology:affects_constraint(amendment_history__emergency_acts_1968, rearmament_1956).
narrative_ontology:affects_constraint(amendment_history__emergency_acts_1968, reunification_amendments_1990).
narrative_ontology:affects_constraint(amendment_history__emergency_acts_1968, asylum_compromise_1993).

% DUAL FORMULATION NOTE:
% The 1968 emergency constitution is one reading of the sovereignty kernel contested across the amendment_history family. It is distinct from but structurally linked to: rearmament_1956 (state capacity via defense, preceding emergency authority), reunification_amendments_1990 (sovereignty claim completed through accession, making Article 146 renewal promise deferred), and asylum_compromise_1993 (citizenship rights constrained by safe-third-country logic, showing how constitutional frameworks narrow founding commitments over time). Each reading decomposes into its own constraint story with its own ε value; they are linked through the sovereignty kernel controversy, not through a single causal chain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(amendment_history__emergency_acts_1968, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
