% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collective_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collective_security_reading, []).

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
 *   constraint_id: second_amendment_text__collective_security_reading
 *   human_readable: Second Amendment Collective Security Reading: State Regulatory Authority
 *   domain: constitutional_law/firearms_policy/political_theory
 *
 * SUMMARY:
 *   The Second Amendment collective security reading interprets the militia
 *   clause as conditioning the right to bear arms on the state's security
 *   interest rather than protecting an individual pre-political right. Under
 *   this reading, the Second Amendment protects a state power (to maintain
 *   organized militia for collective defense) rather than an individual
 *   liberty, making firearm ownership a privilege contingent on state
 *   approval. This constraint demonstrates the diagnostic value of kernel
 *   decomposition: the ambiguous founding text permits multiple internally
 *   coherent readings that instantiate structurally distinct constraints. The
 *   collective security reading positions state regulatory apparatus as
 *   primary beneficiary, individual gun owners as constrained/trapped class,
 *   and organized militia as asymmetric partners in the coordination
 *   function. Theater has increased over the interval (0.35 → 0.55) as the
 *   historical militia system (citizen-soldiers) has become increasingly
 *   disconnected from modern militia implementation (professional military,
 *   National Guard), creating a legitimation gap between constitutional
 *   rhetoric and institutional reality.
 *
 * KEY AGENTS:
 *   - State Regulatory Apparatus: Primary beneficiary (institutional/arbitrage) — controls licensing authority, gathers information, maintains monopoly on interpretation of legitimate militia membership
 *   - Individual Gun Owners: Primary victim (powerless/trapped under strict interpretation; moderate/constrained under permissive licensing) — face legal prohibition outside state permission, licensing costs, background checks, delayed access
 *   - State-Organized Militia (National Guard, state defense forces): Secondary beneficiary (organized/mobile) — gain constitutional authority and federal funding; extracted from because state maintains command and can redeploy resources
 *   - Organized Citizens (militia enthusiasts, preparedness groups): Secondary victim (moderate/constrained) — face legal barriers to autonomous militia organization; must operate within state-sanctioned structures
 *   - Interpretive Authority (courts, constitutional scholars): Institutional actor (institutional/arbitrage) — controls which reading prevails; benefits from stable interpretation regardless of which reading is adopted
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing one reading as natural law rather than contingent interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, 0.58).
domain_priors:suppression_score(second_amendment_text__collective_security_reading, 0.62).
domain_priors:theater_ratio(second_amendment_text__collective_security_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_security_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__collective_security_reading, "Second Amendment Collective Security Reading: State Regulatory Authority").
narrative_ontology:topic_domain(second_amendment_text__collective_security_reading, "constitutional_law/firearms_policy/political_theory").

domain_priors:requires_active_enforcement(second_amendment_text__collective_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_security_reading, 'f030d4c8-6474-4a5d-a614-15d81905cced').
narrative_ontology:cs_kernel_codification('f030d4c8-6474-4a5d-a614-15d81905cced', fixed_text).
narrative_ontology:cs_authority_grounding('f030d4c8-6474-4a5d-a614-15d81905cced', lineage).
narrative_ontology:cs_interpretation_layer_present('f030d4c8-6474-4a5d-a614-15d81905cced').
narrative_ontology:cs_reading_relation('f030d4c8-6474-4a5d-a614-15d81905cced', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('f030d4c8-6474-4a5d-a614-15d81905cced', second_amendment_text__originalist_civic_virtue_reading, influences).
narrative_ontology:cs_axiom('f030d4c8-6474-4a5d-a614-15d81905cced', foundational, state_regulatory_authority_over_collective_defense).
narrative_ontology:cs_axiom_status(state_regulatory_authority_over_collective_defense, holdable).
narrative_ontology:cs_axiom_grounding('f030d4c8-6474-4a5d-a614-15d81905cced', state_regulatory_authority_over_collective_defense, conventional).
narrative_ontology:cs_axiom('f030d4c8-6474-4a5d-a614-15d81905cced', foundational, individual_right_as_conditional_privilege).
narrative_ontology:cs_axiom_status(individual_right_as_conditional_privilege, holdable).
narrative_ontology:cs_axiom_grounding('f030d4c8-6474-4a5d-a614-15d81905cced', individual_right_as_conditional_privilege, conventional).
narrative_ontology:cs_reference_frame('f030d4c8-6474-4a5d-a614-15d81905cced', state_security_regulatory_authority).
narrative_ontology:cs_drift_state('f030d4c8-6474-4a5d-a614-15d81905cced', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f030d4c8-6474-4a5d-a614-15d81905cced', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_security_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, organized_militia_structures).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, unlicensed_firearm_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL GUN OWNER (SNARE) — Under the collective security reading, the right to bear arms is conditioned on militia membership or state-approved licensing. An individual without militia affiliation faces complete legal prohibition on firearm possession. The suppression is structural (legal prohibition) and the exit option is genuinely trapped — no alternative pathway to gun ownership exists outside state licensing. Maximum experienced extraction because the state apparatus has monopolized the legitimate claim to the text.
constraint_indexing:constraint_classification(second_amendment_text__collective_security_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LICENSABLE PERMIT-HOLDER (TANGLED ROPE) — An individual who submits to licensing and background-check requirements gains conditional firearm access. The constraint exhibits both coordination (the state provides security infrastructure, license verification enables trusted transactions) and extraction (the licensing process imposes costs, delays, administrative burden, and ongoing surveillance requirements). The permit-holder benefits from the legitimacy that state approval provides but bears significant friction costs. Exit is constrained but possible — one may choose not to pursue licensing, though at the cost of forgoing firearm ownership. Asymmetric extraction because the state captures information and control over who can access firearms.
constraint_indexing:constraint_classification(second_amendment_text__collective_security_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE REGULATORY APPARATUS (ROPE) — The collective security reading positions the state as the beneficiary and coordinator. The state solves a genuine coordination problem: enabling collective defense through organized militia structures while reducing private risk of uncontrolled proliferation. The state's exit option is arbitrage — it can choose licensing regimes, militia participation structures, or military spending as alternative mechanisms for collective security. The constraint appears as pure coordination to the state: managing firearms distribution to serve the stated security function. The state experiences no extraction; it experiences institutional leverage.
constraint_indexing:constraint_classification(second_amendment_text__collective_security_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZED MILITIA STRUCTURES (TANGLED ROPE) — State-sanctioned militia (National Guard, state defense forces) benefit from the constitutional framing that makes them the legitimate inheritors of the 'well-regulated Militia' clause. They gain institutional authority, federal funding, and legal recognition. However, the constraint also extracts from these structures: they bear the operational burden of militia readiness, are subject to state command authority, and cannot unilaterally interpret their constitutional mandate. This is mixed coordination (the state and militia jointly solve national defense) and asymmetric extraction (the state maintains command and can redeploy militia resources). Exit is mobile but costly — militia structures can lobby for different constitutional readings but face legal and political barriers to unilateral reinterpretation.
constraint_indexing:constraint_classification(second_amendment_text__collective_security_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: HISTORICAL MILITIA IDEAL (PITON) — The collective security reading invokes the historical militia system (18th-century citizen-soldiers) as the foundational image. Modern National Guard structures and state licensing regimes claim lineage to this ideal but operate through entirely different mechanisms (professional military, centralized logistics, modern weapons). The performative element is high (0.48 measured theater_ratio) because the constitutional language preserves the rhetoric of citizen militia while the actual institutional structure has atrophied that function. Militia readiness in the modern state is largely achieved through professional military and volunteer programs, not through widespread private firearms. The invocation of militia serves legitimation (connecting to founding documents) more than function (militia participation is not required for effective collective defense). The constraint persists through institutional inertia and legitimacy theater rather than functional necessity.
constraint_indexing:constraint_classification(second_amendment_text__collective_security_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal scope, some variant of the collective security principle appears immutable: every organized polity must coordinate defensive capability and restrict private arms to some degree. The principle that states regulate weapons to serve collective security could be seen as an irreducible feature of political organization itself. However, this perspective risks conflating a particular institutional reading of the Second Amendment with a natural law about states and weapons. The mountain classification is perspectival and vulnerable to false summit detection — the structural data reveals that the 'collective security reading' is one constructed interpretation among competing legitimate readings, not a law of nature.
constraint_indexing:constraint_classification(second_amendment_text__collective_security_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_security_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(second_amendment_text__collective_security_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_amendment_text__collective_security_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(second_amendment_text__collective_security_reading, TR),
    TR >= 0.70.

:- end_tests(second_amendment_text__collective_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting the asymmetric regulatory power. The state captures significant benefits (licensing control, firearm registry, authority to exclude) while bearing minimal costs (licensing administration is delegated or self-funded). Individual gun owners bear substantial costs (licensing fees, background checks, delayed access, surveillance). The constraint is not a pure snare because genuine coordination exists: the state legitimately addresses collective security by regulating firearm access, and some individuals benefit from state-provided security. However, the asymmetry is severe — the state's gains come directly from restricting individual choice. Suppression (0.62): High. Legal prohibition on unlicensed firearm ownership is a structural barrier that cannot be easily overcome. Individuals cannot exit the licensing regime without abandoning gun ownership entirely. The suppression is not total (licensing is available, at least in principle) but it is substantial. Theater ratio (0.48): Moderate. The historical militia rhetoric (citizen-soldiers, well-regulated militia) persists in constitutional language, but modern militia implementation occurs almost entirely through professional military and National Guard structures. Private citizens with firearms do not constitute the primary collective defense mechanism, yet the constitutional language continues to invoke militia. This gap between rhetoric (founding-era militia) and reality (modern professional military) creates performative content — the invocation of militia serves legitimation more than function. The theater ratio has increased over the century as the mechanization and professionalization of military forces has made citizen-soldier militia increasingly marginal to actual collective defense.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits sharp perspectival divergence. The state regulatory apparatus sees pure coordination (Rope) — it solves a legitimate problem of collective security. Individual gun owners see pure extraction and coercion (Snare) — they are prohibited from exercising a right outside state permission. Permissive interpreters see genuine coordination with mixed costs (Tangled Rope) — some individuals benefit from licensed access while others bear exclusion. The historical militia ideal, invoked as constitutional warrant, appears as institutional inertia (Piton) — the rhetoric persists but the mechanism has atrophied. From a civilizational scope, some state authority over weapons appears inevitable (Mountain), but this risks naturalizing a particular reading as law of nature. The perspectival range from Rope (state view) to Snare (excluded individual view) to Mountain (civilizational inevitability) reveals the depth of contestation embedded in the founding text.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is computed from the agent's structural position relative to THIS specific constraint. State regulatory apparatus (beneficiary + arbitrage) derives low d (~0.15), producing negative or near-zero effective extraction from the state's perspective — the constraint favors the state. Individual gun owner (victim + trapped) derives high d (~0.95), producing high f(d) (~1.42) and high experienced extractiveness — the constraint heavily burdens this agent. Organized militia (beneficiary but also constrained by state command) derives mid-range d (~0.45), producing moderate experienced extractiveness despite beneficiary status — the coordination function is genuine but state control is real. The piton classification does not depend on high extractiveness (chi is low for institutional actors with arbitrage) but on theater_ratio exceeding 0.70; at institutional power and arbitrage exit, theater drives the classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint story describes one reading of a contested constitutional kernel, not a unique fact about the world. The 'correct' classification depends on which reading of the Second Amendment is adopted — and that choice is a question of interpretive authority, not empirical fact. The analytics do not adjudicate between readings; they model what each reading entails structurally. Under the collective_security_reading: state regulatory power is substantial (institutional/arbitrage perspective yields Rope), individual gun owners face extraction (powerless/trapped yields Snare), organized militia occupies an asymmetric partnership (organized/mobile yields Tangled Rope). Under the individual_right_reading (not instantiated in this story): the state's regulatory authority would be bounded or eliminated, shifting the state perspective from Rope to Snare (the state is constrained by constitutional limits). The mandatrophy demonstrates that classification is not unique to the constraint facts — it is relative to the interpretive framing. The engine's perspectival structure is essential to handling this: each perspective is a different reading's structural analysis. The false summit detector (Mountain perspective) correctly flags the risk that the civilizational view naturalizes one reading as law of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_clause_scope_ambiguity,
    'Does ''well regulated Militia'' refer to all able-bodied citizens in militia capacity, or only to state-organized military structures?',
    'Historical linguistic analysis (founding-era usage of ''militia''); analysis of contemporaneous state constitutions and militia statutes; comparative law examination of militia systems in similar constitutional democracies',
    'If ''Militia'' = all citizens: the conditioning phrase applies only to the specific security function (collective defense), potentially permitting individual firearm ownership outside militia service. If ''Militia'' = state-organized structures only: state regulatory authority is plenary and individual ownership is contingent on state permission. This distinction shifts the constraint from tangled_rope (mixed coordination + asymmetric extraction) to snare (pure extraction with minimal coordination function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_clause_scope_ambiguity, empirical, 'Semantic scope of ''well regulated Militia'' — universal citizen capacity vs. state-organized apparatus').

omega_variable(
    necessity_condition_contestation,
    'Is collective security actually achieved through private firearm ownership, or is it a rationalization for a reading that prioritizes state monopoly on legitimate violence?',
    'Empirical analysis of defensive efficacy: do militia members with private firearms improve collective security outcomes compared to professional military + law enforcement? Cross-national comparison of security outcomes in regimes with high private ownership (militia emphasis) vs. centralized state control. Analysis of historical militia performance in contemporary security contexts.',
    'If private militia firearms actually improve security: the coordination function is genuine, supporting tangled_rope classification. If security is primarily achieved through professional military and law enforcement: the militia framing is theater (legitimation), supporting piton or snare classification. The constraint''s extractiveness and suppression values would need upward revision if the coordination function is found to be primarily legitimating rather than functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_condition_contestation, empirical, 'Whether private militia firearms actually serve collective security or serve as rationalization for state monopoly').

omega_variable(
    reading_kernel_contingency,
    'Is the ''collective security reading'' one defensible interpretation of the Second Amendment text, or is it the correct reading that siblings misinterpret?',
    'This is routed through omega variables per Rule 2 (committer frame) because it is a kernel-level ambiguity about the contested text itself. The resolution depends on which interpretive authority one recognizes (originalist scholarship, living constitution doctrine, comparative constitutionalism, state practice). No single empirical test resolves this — it is a question of constitutional reading authority.',
    'If the collective security reading is one legitimate reading among others: the constraint story is correct as written, and sibling readings (individual_right_reading, originalist_civic_virtue_reading) are equally valid constraints. If this reading is the correct interpretation and siblings are misreadings: the schema''s commitment to perspectival multiplicity becomes strained. If this reading is subordinate to a sibling: this constraint story''s framing needs revision. The engine''s false summit detector and the reading_relations declarations (coexists_with) embody the assumption that multiple readings are live.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contingency, conceptual, 'Whether the collective security reading is one legitimate reading or the uniquely correct interpretation').

omega_variable(
    licensing_regime_extractiveness_calibration,
    'What percentage of applicants are denied firearm licenses under state collective-security-model regulations, and what are the stated denial grounds?',
    'Statistical analysis of state licensing data (percentage approval rates, denial grounds by state); analysis of whether denial grounds track genuine security concerns or become pretexts for arbitrary exclusion; appeal rate analysis and successful challenge statistics',
    'If denial rates are low (>90% approval) and grounds are narrowly security-focused: extractiveness is toward the lower end of tangled_rope (ε ≈ 0.50). If denial rates are high (50-70% approval) or grounds are expansive/arbitrary: extractiveness shifts toward snare (ε > 0.65). High arbitrary denial rates indicate that the ''collective security'' framing is cover for state monopolization rather than genuine coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(licensing_regime_extractiveness_calibration, empirical, 'Quantification of state licensing denial rates and grounds — indicator of extractiveness').

omega_variable(
    original_intent_militia_service_requirement,
    'Did the founding generation condition Second Amendment rights on actual militia service, or did they understand militia membership as universal (all able-bodied citizens)?',
    'Historical analysis of state militia laws 1790-1810; founding-era commentary on militia obligation vs. individual right; comparative analysis with contemporaneous European militia systems; legislative records of early state constitutions and firearms regulations',
    'If militia service was understood as universal obligation (all men in militia capacity): the conditioning is on a status nearly everyone holds, making it a minimal restriction — strong support for individual_right_reading. If militia service was understood as requiring actual enrollment in state-organized structures: the conditioning is genuine and collective_security_reading is supported. This distinction directly affects whether the constraint is snare (state monopoly) or tangled_rope (genuine coordination with asymmetric extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_intent_militia_service_requirement, empirical, 'Whether founding-era militia service was universal obligation or state-organized enrollment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_security_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_collective_tr_t0, second_amendment_text__collective_security_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sa_collective_tr_t50, second_amendment_text__collective_security_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(sa_collective_tr_t100, second_amendment_text__collective_security_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(sa_collective_be_t0, second_amendment_text__collective_security_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(sa_collective_be_t50, second_amendment_text__collective_security_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(sa_collective_be_t100, second_amendment_text__collective_security_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sa_collective_su_t0, second_amendment_text__collective_security_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(sa_collective_su_t50, second_amendment_text__collective_security_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(sa_collective_su_t100, second_amendment_text__collective_security_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_security_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment kernel admits at least three structurally distinct constraint readings: collective_security_reading (this file), individual_right_reading, and originalist_civic_virtue_reading. Each reading has a different epsilon value reflecting the different structural analysis of state vs. individual rights. The readings are linked as siblings in the kernel, with reading_relations declared in cs_structure per Rule 4. Do NOT merge the readings into a single constraint story — each is a live interpretation with distinct extractiveness and suppression profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
