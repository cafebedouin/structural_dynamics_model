% ============================================================================
% CONSTRAINT STORY: suspensive_veto_monarchy__varennes_broken_trust_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_suspensive_veto_monarchy__varennes_broken_trust_reading, []).

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
 *   constraint_id: suspensive_veto_monarchy__varennes_broken_trust_reading
 *   human_readable: Suspensive Veto Under Broken Trust: Varennes Reading
 *   domain: legal/doctrinal/constitutional_monarchy
 *
 * SUMMARY:
 *   The Varennes reading instantiates one doctrinal reading of the suspensive
 *   veto mechanism: the king's flight toward foreign armies on June 20-21,
 *   1791, collapsed the trust condition that the veto mechanism contained
 *   implicitly. Before Varennes, the suspensive veto was a genuine
 *   constitutional design — an executive delay that forced reconsideration
 *   without permitting absolutism. After Varennes, every subsequent veto
 *   became legible as a move coordinated with emigrant forces and foreign
 *   armies. The reading does not claim that the veto was *designed* as a
 *   sabotage mechanism; rather, it claims that Varennes revealed a fatal
 *   prerequisite: the veto's legitimacy rested entirely on the assumption
 *   that the king could be trusted to exercise it as a constitutional actor,
 *   not as a strategic agent aligned with military powers opposed to the
 *   Assembly. Once that assumption was violated, the formal design persisted
 *   but its function inverted. The veto remained a legal instrument, but
 *   using it became indistinguishable from treason. This reading generates a
 *   constraint that is distinctly a Snare for the constitutional-monarchist
 *   center and for those who believed in the design, because they find
 *   themselves bound by a mechanism that now suppresses legitimate
 *   constitutional action while empowering the instrument of their enemies.
 *
 * KEY AGENTS:
 *   - The King (Louis XVI): Primary actor whose flight at Varennes is the breach event. Power level depends on perspective: institutional before Varennes, analytically demonstrated to be aligned with foreign armies after Varennes.
 *   - The Constitutional-Monarchist Center: Primary victim (organized/constrained). Trapped inside the constitution they designed, now facing a veto that signals collusion with invading powers. Their legitimate constitutional positions are suppressed by their own mechanism.
 *   - The Republican Coalition: Primary beneficiary (powerful/arbitrage). The flight vindicates their structural claim: executive delay was always a vehicle for sabotage. Every subsequent veto is evidence for abolition.
 *   - The Assembly: Secondary institutional actor (organized/constrained). Must keep the constitution while managing the king's broken trust. Can pass laws but their legitimacy is constrained by constitutional commitment.
 *   - The Emigrant Forces and Foreign Armies: Implicit beneficiaries. The king's alignment with them transforms the veto from executive reserve into a coordinated instrument of foreign strategy.
 *   - The Analytical Observer: Detects the doctrinal cascade — the legal form survives but its function is corrupted by a violated trust condition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(suspensive_veto_monarchy__varennes_broken_trust_reading, 0.68).
domain_priors:suppression_score(suspensive_veto_monarchy__varennes_broken_trust_reading, 0.72).
domain_priors:theater_ratio(suspensive_veto_monarchy__varennes_broken_trust_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(suspensive_veto_monarchy__varennes_broken_trust_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(suspensive_veto_monarchy__varennes_broken_trust_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(suspensive_veto_monarchy__varennes_broken_trust_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(suspensive_veto_monarchy__varennes_broken_trust_reading, snare).
narrative_ontology:human_readable(suspensive_veto_monarchy__varennes_broken_trust_reading, "Suspensive Veto Under Broken Trust: Varennes Reading").
narrative_ontology:topic_domain(suspensive_veto_monarchy__varennes_broken_trust_reading, "legal/doctrinal/constitutional_monarchy").

domain_priors:requires_active_enforcement(suspensive_veto_monarchy__varennes_broken_trust_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(suspensive_veto_monarchy__varennes_broken_trust_reading, 'bfafc40e-888d-4b9c-b170-77a8b159b8a7').
narrative_ontology:cs_kernel_codification('bfafc40e-888d-4b9c-b170-77a8b159b8a7', fixed_text).
narrative_ontology:cs_authority_grounding('bfafc40e-888d-4b9c-b170-77a8b159b8a7', lineage).
narrative_ontology:cs_interpretation_layer_present('bfafc40e-888d-4b9c-b170-77a8b159b8a7').
narrative_ontology:cs_reading_relation('bfafc40e-888d-4b9c-b170-77a8b159b8a7', suspensive_veto_monarchy__constitutional_monarchy_design_reading, forecloses).
narrative_ontology:cs_reading_relation('bfafc40e-888d-4b9c-b170-77a8b159b8a7', suspensive_veto_monarchy__paralysis_mechanism_reading, coexists_with).
narrative_ontology:cs_axiom('bfafc40e-888d-4b9c-b170-77a8b159b8a7', foundational, executive_veto_requires_trusted_actor).
narrative_ontology:cs_axiom_status(executive_veto_requires_trusted_actor, holdable).
narrative_ontology:cs_axiom_grounding('bfafc40e-888d-4b9c-b170-77a8b159b8a7', executive_veto_requires_trusted_actor, deontological).
narrative_ontology:cs_axiom('bfafc40e-888d-4b9c-b170-77a8b159b8a7', foundational, varennes_demonstrates_executive_foreign_alignment).
narrative_ontology:cs_axiom_status(varennes_demonstrates_executive_foreign_alignment, holdable).
narrative_ontology:cs_axiom_grounding('bfafc40e-888d-4b9c-b170-77a8b159b8a7', varennes_demonstrates_executive_foreign_alignment, empirically_contingent).
narrative_ontology:cs_reference_frame('bfafc40e-888d-4b9c-b170-77a8b159b8a7', constitutional_executive_trust).
narrative_ontology:cs_drift_state('bfafc40e-888d-4b9c-b170-77a8b159b8a7', varennes_june_1791, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('bfafc40e-888d-4b9c-b170-77a8b159b8a7', '').
narrative_ontology:cs_kernel_id(suspensive_veto_monarchy__varennes_broken_trust_reading, suspensive_veto_monarchy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(suspensive_veto_monarchy__varennes_broken_trust_reading, republican_argument).
narrative_ontology:constraint_beneficiary(suspensive_veto_monarchy__varennes_broken_trust_reading, revolutionary_coalition).
narrative_ontology:constraint_victim(suspensive_veto_monarchy__varennes_broken_trust_reading, constitutional_monarchist_center).
narrative_ontology:constraint_victim(suspensive_veto_monarchy__varennes_broken_trust_reading, legal_legitimacy_of_executive_reserve).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The center that believed in constitutional monarchy faces the veto as pure extraction after Varennes: the king's flight demonstrates that the executive cannot be trusted to exercise delay legitimately. Every subsequent veto is now readable as a move toward the emigrant armies. The constitutional design persists formally but without credibility. The trapped agent cannot exit — they remain inside the constitution but experience it as hollowed.
constraint_indexing:constraint_classification(suspensive_veto_monarchy__varennes_broken_trust_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% The republicans see Varennes as vindication of their structural claim: the suspensive veto was always a mechanism for sabotage, and the king's flight proves it. They experience the constraint as pure coordination: communicating to the Assembly that executive delay is now synonymous with collusion. The veto becomes an instrument that proves the republic's necessity.
constraint_indexing:constraint_classification(suspensive_veto_monarchy__varennes_broken_trust_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% The Assembly faces a tangled coordination-extraction hybrid: they must keep the constitution (coordination function) while managing the king's broken trust (extraction mechanism). They have agency and can pass laws, but their legitimacy is constrained by their own constitutional commitment. The veto they created now suppresses their capacity to act, but they cannot revise it without admitting the design failed.
constraint_indexing:constraint_classification(suspensive_veto_monarchy__varennes_broken_trust_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The constitution as institutional text persists through Varennes as largely performative: it retains the formal structure of suspensive veto and royal prerogative, but the mechanism no longer functions in the way the design intended. The text lives on through inertia and the absence of a competitor framework, not because anyone believes the executive can operate it with legitimacy.
constraint_indexing:constraint_classification(suspensive_veto_monarchy__varennes_broken_trust_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From the civilizational perspective, the constraint is the structural fact that a veto cannot function legitimately when the agent wielding it has demonstrated allegiance to foreign powers. The observer sees Varennes as having inverted the sign of every subsequent veto: what was designed as executive reserve became executive sabotage. This is a doctrinal cascade — the legal form survives but its function is corrupted by the trust condition it contained implicitly.
constraint_indexing:constraint_classification(suspensive_veto_monarchy__varennes_broken_trust_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(suspensive_veto_monarchy__varennes_broken_trust_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(suspensive_veto_monarchy__varennes_broken_trust_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(suspensive_veto_monarchy__varennes_broken_trust_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(suspensive_veto_monarchy__varennes_broken_trust_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(suspensive_veto_monarchy__varennes_broken_trust_reading, TR),
    TR >= 0.70.

:- end_tests(suspensive_veto_monarchy__varennes_broken_trust_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, derived from the king's demonstrated alignment with foreign powers. The veto is no longer a neutral constitutional mechanism but a credible threat that every legislative action faces sabotage on behalf of invading armies. The measurement shows a sharp jump from 0.32 (pre-Varennes sincere design) to 0.68 immediately after the flight, reflecting the instantaneous delegitimation of the executive. Suppression (0.72): High, reflecting that constitutional-monarchists are trapped inside the very mechanism they created. They cannot exit or revise the constitution without admitting the design failed — which would vindicate republicanism. The veto suppresses their capacity to act legislatively while remaining formally available to them as a prerogative. Theater ratio (0.58): Moderate-high. Before Varennes, the veto was a functional design element. After Varennes, it becomes partially theatrical — a performed deference to constitutional form while the actual function is collusion with foreign armies. The Assembly must treat vetoes as legitimate constitutional acts while increasingly certain they are acts of sabotage. This is not maximum theater (that would be Piton) because the veto retains a real extraction function; it is Snare territory because extraction dominates coordination.
 *
 * PERSPECTIVAL GAP:
 *   The power of this reading is that it generates incompatible classifications from structurally identical observations of the same mechanism. The republicans see the veto as always having been a Snare (Rope from their perspective because they see through it as evidence for republic). The constitutional-monarchists see it as Rope before Varennes and Snare after — a sudden inversion caused by a single event. The king's perspective inverts from institutional/arbitrage (seeing his prerogative as coordinate) to effectively aligned with foreign armies (his perspective becomes analytically indistinguishable from the emigrant forces'). The temporal gap — before and after June 21, 1791 — is not a matter of degree but of doctrine. The same legal text has two different functional meanings depending on whether one assumes the executive is a constitutional actor or a strategic agent aligned with foreign invasion.
 *
 * DIRECTIONALITY LOGIC:
 *   The king's directionality d shifts sharply at Varennes. Before the flight, the king holds institutional power and can exercise arbitrage exit — he is constrained but has options. He appears as a beneficiary of the constitutional design. After Varennes, the king's structural relationship inverts: he is now explicitly aligned with foreign military powers, making him a threat to the entire constitutional framework rather than an actor within it. The Assembly (organized/constrained) experiences high d because they remain inside the mechanism while having discovered it is weapons-pointed at them. The constitutional-monarchist center (organized/constrained) experiences maximum d because they designed the veto and now experience it as treason's instrument. Directionality is derived from beneficiary/victim status: the republicans benefit from the collapse of trust (low d for them); the constitutional-monarchists are victimized by their own design (high d for them).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that this reading does not claim the veto is *inherently* extractive, only that it becomes extractive when the actor wielding it demonstrates allegiance to foreign powers. The constraint is doctrine-dependent: it applies only to readings that accept that Varennes revealed the king's alignment with invasion forces. The constitutional-monarchy design reading would classify this as misreading political context as doctrinal fact. The paralysis mechanism reading would agree that the veto became extractive but would locate the extraction earlier (the war dynamics rather than Varennes specifically). This reading's uniqueness is in the sharp discontinuity — a single event transforms the mechanism's function entirely. The extractiveness jump from 0.32 to 0.68 models this discontinuity precisely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trust_restoration_threshold,
    'Could the king have restored legitimacy to his veto through subsequent loyalty demonstrations, or did Varennes permanently sever the trust condition?',
    'Counterfactual analysis of historical trajectories: if the king had made different choices between June 1791 and September 1792, could the Assembly have re-accorded credibility to his veto? Or was the breach irreversible by doctrinal structure rather than prudential repair?',
    'If restorable: the constraint is temporarily high extraction with recovery potential (Tangled Rope). If irreversible: the constraint is permanently a Snare because the trust condition was constitutive, not incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trust_restoration_threshold, conceptual, 'Whether broken trust in executive can be restored within constitutional monarchy framework').

omega_variable(
    veto_sabotage_intentionality,
    'Did the king''s subsequent vetoes represent deliberate collusion with emigrant armies, or were they exercises of legitimate (if controversial) constitutional prerogative that the republicans reinterpreted as sabotage?',
    'Historical documentation of royal intent: secret correspondence, emigrant negotiations, deliberations within the royal council. Distinguish between vetoes motivated by constitutional principle vs. those coordinated with foreign military planning.',
    'If deliberate sabotage: extractiveness rises above 0.68 (the veto is weaponized). If principled prerogative: extractiveness may be lower, and the constraint is closer to Tangled Rope (legitimate disagreement over constitutional reach).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_sabotage_intentionality, empirical, 'Whether king''s vetoes were sabotage or principled constitutional exercise').

omega_variable(
    alternative_constitutional_closure,
    'Did the Varennes moment require the abolition of the suspensive veto (as the republicans argued), or could the constitutional-monarchist center have preserved the veto through some mechanism that restored executive credibility?',
    'Analysis of counterfactual constitutional designs proposed between June and September 1792: accountability structures, attestation clauses, or institutional checks that might have preserved veto authority while addressing the trust breach.',
    'If alternatives existed: the republicans'' use of Varennes to eliminate the veto was a choice, not a necessity — the constraint maps to extractive reading rather than structural inevitability. If no alternatives: Varennes functioned as a hard reset, and the constraint is unavoidable (closer to Mountain from the analytical view).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_constitutional_closure, conceptual, 'Whether constitutional-monarchist alternatives existed to republican solution').

omega_variable(
    reading_identity_varennes_as_kernel_event,
    'Is this reading genuinely instantiating the Varennes moment as a discrete constraint, or is it conflating the flight with the subsequent use of the flight as a political argument?',
    'Temporal analysis: does the constraint apply before Varennes (the veto as designed), at Varennes (the flight as revelation), or after Varennes (the veto as reinterpreted)? If the reading''s binding mechanism is the *interpretive act* rather than the flight itself, it may not be about the veto-as-designed but about the veto-as-politicized.',
    'If temporally prior-inclusive (applies before and after): the constraint is about the veto design''s latent vulnerability. If posterior-only (applies after the reinterpretation): the constraint is about how political argument corrupts a neutral mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_varennes_as_kernel_event, conceptual, 'Temporal scope of the constraint: does it precede Varennes or postdate it?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(suspensive_veto_monarchy__varennes_broken_trust_reading, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(varennes_theater_pre_flight_sincere_design, suspensive_veto_monarchy__varennes_broken_trust_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(varennes_theater_post_varennes_veto_as_signaling, suspensive_veto_monarchy__varennes_broken_trust_reading, theater_ratio, 1, 0.58).
narrative_ontology:measurement(varennes_theater_ritual_persists_credibility_gone, suspensive_veto_monarchy__varennes_broken_trust_reading, theater_ratio, 2, 0.62).

% Extraction over time
narrative_ontology:measurement(varennes_extractiveness_pre_flight, suspensive_veto_monarchy__varennes_broken_trust_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(varennes_extractiveness_post_varennes_june_1791, suspensive_veto_monarchy__varennes_broken_trust_reading, base_extractiveness, 1, 0.68).
narrative_ontology:measurement(varennes_extractiveness_veto_accumulation_1791_92, suspensive_veto_monarchy__varennes_broken_trust_reading, base_extractiveness, 2, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(varennes_suppression_pre_flight, suspensive_veto_monarchy__varennes_broken_trust_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(varennes_suppression_post_varennes_trust_collapse, suspensive_veto_monarchy__varennes_broken_trust_reading, suppression_requirement, 1, 0.72).
narrative_ontology:measurement(varennes_suppression_republican_consolidation, suspensive_veto_monarchy__varennes_broken_trust_reading, suppression_requirement, 2, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(suspensive_veto_monarchy__varennes_broken_trust_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(suspensive_veto_monarchy__varennes_broken_trust_reading, suspensive_veto_monarchy__constitutional_monarchy_design_reading).
narrative_ontology:affects_constraint(suspensive_veto_monarchy__varennes_broken_trust_reading, suspensive_veto_monarchy__paralysis_mechanism_reading).

% DUAL FORMULATION NOTE:
% The suspensive veto mechanism is instantiated by three distinct constraint readings corresponding to three doctrinal interpretations: the design reading (Rope), the paralysis reading (Tangled Rope or Snare depending on temporal focus), and the Varennes reading (Snare under broken trust). Each reading has its own extractiveness profile reflecting different assumptions about the king's motives and the mechanism's function. Decomposition is necessary because the three readings classify differently despite describing the same legal text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
