% ============================================================================
% CONSTRAINT STORY: senate_authority__advisory_only_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_senate_authority__advisory_only_reading, []).

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
 *   constraint_id: senate_authority__advisory_only_reading
 *   human_readable: Senate Authority as Advisory Power (Roman Republic Reading)
 *   domain: legal/constitutional/doctrinal
 *
 * SUMMARY:
 *   The Senate of the Roman Republic possessed no explicit legal power to
 *   bind magistrates or assembly votes. Its authority rested on
 *   auctoritas—the accumulated prestige, wisdom, and moral weight of an
 *   institution of ex-magistrates. Yet this advisory-only constraint
 *   coordinated the Republic's finances, foreign policy, and succession for
 *   centuries. The constraint's stability depended entirely on deference:
 *   magistrates honored senatorial advice not because law compelled it, but
 *   because ignoring the Senate carried social and political cost. The power
 *   'evaporated whenever someone called the bluff'—when a tribune like the
 *   Gracchi brothers asserted that tribuni plebis authority superseded
 *   senatorial advice, or when a general like Pompey or Caesar wielded
 *   extralegal power without consulting the Senate, the institution's binding
 *   force disappeared instantly. This reading instantiates the advisory-only
 *   constitutional position: the Senate legally only advised; its decrees
 *   bound no one without magisterial action or assembly vote; and its
 *   extractiveness (its capacity to constrain magistrate behavior) was
 *   contingent on belief rather than law.
 *
 * KEY AGENTS:
 *   - Senatorial Oligarchy: Primary beneficiary (institutional/arbitrage) — benefits from coordination mechanism during normal politics; loses credibility when tested
 *   - Magistrate with Imperium: Secondary beneficiary (institutional/arbitrage) — gains counsel and legitimacy from senatorial advice; retains legal authority to override
 *   - Tribuni Plebis: Primary challenger (organized/constrained) — organized group with independent authority; faces extraction cost when testing the boundary
 *   - Individual Senator: Variable position (moderate/constrained) — gains prestige from membership in advisory body; exposed to loss of auctoritas when Senate's bluff is called
 *   - The Gracchi Brothers: Historical testers (organized/identity_locked) — their political identity constituted through challenging oligarchic consensus; cannot exit without becoming something other than populares
 *   - Auctoritas (Institutional Authority): Structural victim (powerless/trapped) — abstract property of the institution that degrades when publicly challenged; has no agent to defend it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(senate_authority__advisory_only_reading, 0.52).
domain_priors:suppression_score(senate_authority__advisory_only_reading, 0.48).
domain_priors:theater_ratio(senate_authority__advisory_only_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(senate_authority__advisory_only_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(senate_authority__advisory_only_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(senate_authority__advisory_only_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(senate_authority__advisory_only_reading, tangled_rope).
narrative_ontology:human_readable(senate_authority__advisory_only_reading, "Senate Authority as Advisory Power (Roman Republic Reading)").
narrative_ontology:topic_domain(senate_authority__advisory_only_reading, "legal/constitutional/doctrinal").

domain_priors:requires_active_enforcement(senate_authority__advisory_only_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(senate_authority__advisory_only_reading, 'a5775fc3-429f-4600-b8a3-bd4270d1fec8').
narrative_ontology:cs_kernel_codification('a5775fc3-429f-4600-b8a3-bd4270d1fec8', distributed).
narrative_ontology:cs_authority_grounding('a5775fc3-429f-4600-b8a3-bd4270d1fec8', lineage).
narrative_ontology:cs_interpretation_layer_present('a5775fc3-429f-4600-b8a3-bd4270d1fec8').
narrative_ontology:cs_reading_relation('a5775fc3-429f-4600-b8a3-bd4270d1fec8', senate_authority__deliberative_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('a5775fc3-429f-4600-b8a3-bd4270d1fec8', senate_authority__oligarchic_capture_reading, coexists_with).
narrative_ontology:cs_axiom('a5775fc3-429f-4600-b8a3-bd4270d1fec8', foundational, senatorial_authority_consultative_only).
narrative_ontology:cs_axiom_status(senatorial_authority_consultative_only, holdable).
narrative_ontology:cs_axiom_grounding('a5775fc3-429f-4600-b8a3-bd4270d1fec8', senatorial_authority_consultative_only, deontological).
narrative_ontology:cs_axiom('a5775fc3-429f-4600-b8a3-bd4270d1fec8', foundational, deference_contingency_of_binding_force).
narrative_ontology:cs_axiom_status(deference_contingency_of_binding_force, holdable).
narrative_ontology:cs_axiom_grounding('a5775fc3-429f-4600-b8a3-bd4270d1fec8', deference_contingency_of_binding_force, deontological).
narrative_ontology:cs_reference_frame('a5775fc3-429f-4600-b8a3-bd4270d1fec8', senatorial_consultation_as_constitutional_courtesy).
narrative_ontology:cs_drift_state('a5775fc3-429f-4600-b8a3-bd4270d1fec8', late_republic_gracchi_period, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a5775fc3-429f-4600-b8a3-bd4270d1fec8', '').
narrative_ontology:cs_kernel_id(senate_authority__advisory_only_reading, senate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(senate_authority__advisory_only_reading, whoever_calls_the_bluff).
narrative_ontology:constraint_beneficiary(senate_authority__advisory_only_reading, magistrates_with_veto).
narrative_ontology:constraint_victim(senate_authority__advisory_only_reading, auctoritas_institutional_credibility).
narrative_ontology:constraint_victim(senate_authority__advisory_only_reading, senatorial_dignitas).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRIBUNE TESTING THE BOUNDARY (SNARE) — A tribune exercising imperium outside the pomerium discovers the Senate's decrees carry no force when tested. The institutional extraction is severe: the tribune pays the full cost of the test (legal vulnerability, retaliation by magistrates), while the revelation—that auctoritas is advisory only—undermines the entire coordinating function. The beneficiary of this extraction is clarity, but the tribunal bears the extraction cost. Trapped at biographical scale: once the test is performed, the boundary is exposed.
constraint_indexing:constraint_classification(senate_authority__advisory_only_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ORGANIZED RESISTANCE (TANGLED ROPE) — The Gracchi brothers call the Senate's bluff explicitly, claiming that tribuni plebis power supersedes senatorial advice. Organized groups can challenge the constraint with some agency, but face severe consequences (prosecution, violence). The constraint coordinates elite deliberation (genuine benefit to organized actors seeking legitimacy) while extracting submission: those who benefit from senatorial coordination must accept advisory status; those who challenge must absorb extraction. Mixed but leaning extractive from this perspective.
constraint_indexing:constraint_classification(senate_authority__advisory_only_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE MAGISTRATE WITH VETO POWER (ROPE) — A consul who chooses to honor or ignore senatorial advice experiences this as pure coordination. The advice has weight only if the magistrate respects it; the magistrate has the legal authority to override. From this perspective, the Senate's constraint is a coordination mechanism for sharing information and building legitimacy for decisions. The magistrate benefits from the Senate's deliberation (better intelligence, broader consensus) without legal constraint. This is rope: the magistrate enters the advice-seeking mechanism voluntarily and extracts information value.
constraint_indexing:constraint_classification(senate_authority__advisory_only_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE OLIGARCHIC ELITE PERSPECTIVE (SCAFFOLD) — The majority Senate faction sees their advisory system as a temporary stabilization mechanism: keep magistrates within consulted bounds during their one-year terms, building consensus through deliberation rather than explicit command. The sunset logic operates implicitly: as magistrates and tribuni increasingly challenge the advisory fiction, the Senate's coordinating power declines naturally. The oligarchy views this as a scaffolding structure managing succession and transition, extractiveness moderate because the elite themselves benefit from the deliberative process. Theater_ratio high because the performative aspect (advice-giving ritual) is doing significant work maintaining the fiction.
constraint_indexing:constraint_classification(senate_authority__advisory_only_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE SENATE'S OWN DEGRADED RITUAL (PITON) — By the late Republic, the Senate's own members recognize that their authority is largely performative: they issue decrees, magistrates largely ignore them unless consensus is already established elsewhere, tribuni veto liberally. The ritual persists—deliberation continues, advice is given—but everyone knows that the binding force disappeared. Theater_ratio very high (0.75+) because the institution's functional authority has eroded while ceremonial activity continues. Piton classification derives from the Senate's own consciousness that it is performing rather than commanding.
constraint_indexing:constraint_classification(senate_authority__advisory_only_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LEGAL FORMALISM (PITON) — From a civilizational perspective examining the written constitutional law of the Republic, the Senate's authority is precisely what the advisory_only reading states: consultative and non-binding. The legal reality matches the reading cleanly. Theater is high because Roman constitutionalism as a system is largely performative—roles, rituals, and precedent doing the work of written law. But this perspective risks naturalization: reading the constraint as inherent to Roman law when it is actually a contingent institutional arrangement (i.e., risking false-mountain classification). The analytical view stabilizes as piton: the advisory-only reading is legally accurate, but the system's persistence depends on performative maintenance, not intrinsic legal force.
constraint_indexing:constraint_classification(senate_authority__advisory_only_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(senate_authority__advisory_only_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(senate_authority__advisory_only_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(senate_authority__advisory_only_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(senate_authority__advisory_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(senate_authority__advisory_only_reading, TR),
    TR >= 0.70.

:- end_tests(senate_authority__advisory_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint coordinates elite deliberation (low-extractiveness benefit), but extraction emerges when the advisory mechanism is enforced through social pressure rather than law. Magistrates and tribuni who honor senatorial advice when they could legally override it are extracting legitimacy and stability from the Senate; the Senate is extracting submission. The constraint's extractiveness is contingent on belief—it rises when deference is strong and falls when someone calls the bluff. The measurement trajectory (0.35→0.52) reflects the late Republic's increasing extractiveness as the Senate became more defensive about its advisory-only status, intensifying the performative dimension. Suppression (0.48): Moderate. The suppression is not legal (there is no law suppressing alternatives) but social and institutional. Alternatives to senatorial consultation exist (magistrates could unilaterally decide; tribuni could override), but suppression emerges through reputational damage, loss of legitimacy, and risk of violence. The declining suppression measurement (0.55→0.48) reflects the late Republic's breakdown: as the bluff is called more often, suppression mechanisms fail. Theater ratio (0.65): High. Senatorial advice-giving is substantially performative: the ritual of consultation, deliberation, and formal decree carries ceremonial weight far exceeding its binding power. The theater rises over the interval (0.40→0.65) as the Senate compensates for declining actual authority by intensifying ritual performance.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces radical perspectival gaps because different observers experience the constraint's extractiveness very differently. The magistrate (institutional/arbitrage) experiences rope—they coordinate with the Senate without legal obligation. The tribune challenging the constraint (organized/constrained) experiences snare—they bear full extraction cost to expose the non-binding nature. The Senate itself (institutional/arbitrage in normal times) experiences rope as long as deference holds, then piton as the ritual persists without binding force. The analytical observer risks piton or false-mountain: naturalizing the advisory-only status as an inherent feature of Roman law when it is actually a contingent institutional arrangement dependent on suppression mechanisms that can fail. The perspectival gap reveals the constraint's instability: it is tangled_rope in normal politics (coordination + asymmetric submission) but collapses to snare + piton when tested, because the victims (auctoritas and the challenger) have no mechanism to defend the constraint once deference breaks.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading produces a distinctive directionality structure because the constraint's binding force is contingent. The beneficiary (whoever benefits from having the Senate's advice honored) derives d from their institutional position and ability to defect. A magistrate with arbitrage exit (can ignore the Senate and face only social cost, not legal penalty) experiences low d—they benefit from the advice without legal constraint. An organized group like the Gracchi with constrained exit (challenging the Senate triggers violent response) experiences high d—they bear extraction cost to expose the constraint's non-binding nature. The tribuni plebis are interesting: they have legal exit (their authority is constitutional), so d is computed from organized power + legal escape route, yielding moderate d. The beneficiary 'whoever_calls_the_bluff' has zero d (full beneficiary) because exposing the advisory-only status is purely beneficial for the challenger—they gain prestige and authority. The victim 'auctoritas_institutional_credibility' has d approaching 1.0 (full target)—the institution bears the extraction cost of being tested.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by explicitly embracing the hybrid structure: the Senate legally only advised (tangled_rope), coordinating elite deliberation while extracting deference and submission. The constraint is not pure coordination (it extracts), nor is it pure extraction (it coordinates). The mandate is the advisory-only constitutional position itself—the claim that the Senate's authority is consultative and non-binding. The trophy is the auctoritas that depends on the mandate being respected. Mandatrophy occurs when someone calls the bluff (Gracchi, Pompey, Caesar): the mandate is exposed as contingent on belief rather than law, and the trophy (institutional authority) degrades permanently. The resolution is institutional: either (a) the Senate accepts advisory-only status explicitly and reforms as a true deliberative body (scaffold perspective, sunset), or (b) the Senate reasserts binding authority through explicit legal reform or violence (oligarchic_capture_reading, transformation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deference_vs_legal_obligation,
    'Is the Senate''s binding force a matter of legal obligation (law) or social deference (convention)?',
    'Historical test cases: instances where magistrates or tribuni openly defied senatorial decrees without legal consequence vs. instances of legal penalty for defiance. Examination of magistrates'' private correspondence and contemporaries'' commentary on whether defiance was seen as legal violation or social transgression.',
    'If legal obligation: constraint is mountain (law binds regardless of deference). If convention: constraint is tangled_rope/snare (extraction contingent on belief and testable by defiance). This reading asserts convention; the oligarchic_capture reading asserts law; the deliberative_supremacy_reading avoids the binary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deference_vs_legal_obligation, empirical, 'Whether Senate authority rests on legal obligation or social convention').

omega_variable(
    gracchi_bluff_calling_event,
    'Did the Gracchi brothers'' actions constitute a definitive exposure of the advisory-only constraint, or did they operate within a pre-existing understanding that senatorial advice was non-binding?',
    'Textual analysis of contemporary sources (Livy, Appian, Plutarch) on whether the Gracchi framed their actions as defiance of law or as assertion of tribuni authority within an understood constitutional space. Examination of whether senatorial response to Gracchi actions invoked legal grounds or extra-legal retaliation.',
    'If Gracchi exposed a hidden constraint: the advisory-only reading is revealing suppressed knowledge (extractiveness higher). If Gracchi operated within known bounds: the advisory-only reading is documenting an honest constitutional arrangement (extractiveness lower, more rope-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gracchi_bluff_calling_event, empirical, 'Whether Gracchi exposed or operated within known advisory-only status').

omega_variable(
    beneficiary_identity_instability,
    'Who benefits from the advisory-only constraint when its binding force is contingent on deference?',
    'Observation of instances where the constraint''s enforcement flips: when does the magistrate benefit from honoring advice vs. when does the law-checker benefit from testing it? Tracking of senatorial decrees that were honored vs. defied and the actors gaining status in each case.',
    'If beneficiary is stable (always the magistrate, always the advice-giver): constraint stabilizes as rope or scaffold. If beneficiary flips with each test (whoever calls the bluff gains credibility; whoever was bluffed loses authority): constraint is unstable tangled_rope with extractiveness contingent on the specific test event.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_instability, conceptual, 'Instability of beneficiary identity when deference is the binding mechanism').

omega_variable(
    auctoritas_as_victim,
    'Can auctoritas (institutional moral authority) itself be the victim of this constraint, or is it only the agent bearing auctoritas who is victimized?',
    'Examination of how senatorial auctoritas changed as a result of visible tests (Gracchi defiance, Sulla dictatorship, Pompey''s extra-legal influence): did the institution''s authority to advise degrade after public challenges, or did individual senators maintain prestige?',
    'If auctoritas itself is victimized: the constraint has a structural victim (institutional credibility) distinct from individual agents. This refines the victims list and suggests the constraint''s function is maintaining institutional fiction rather than transferring resources.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(auctoritas_as_victim, conceptual, 'Whether auctoritas itself is victimized by the advisory-only constraint').

omega_variable(
    reading_identity_ambiguity,
    'Is this reading (advisory_only) a genuine constitutional position held within Roman jurisprudence, or is it a modern legal realist''s extraction of what the law was underneath the oligarchic narrative?',
    'Search for explicit statements by Roman jurists or magistrates claiming advisory-only status for the Senate. Comparison with how the oligarchic_capture_reading and deliberative_supremacy_reading appear in ancient sources.',
    'If held explicitly by Romans: this is a contested kernel with three competing readings, all live. If invented by modern analysis: this reading is a deconstruction of a false summit (the ''Senate rules'' mythology), and the omega itself is an instance of the oracle gap (Theorem 4).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_ambiguity, conceptual, 'Whether the advisory-only reading is an ancient position or a modern deconstruction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(senate_authority__advisory_only_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sena_tr_t0, senate_authority__advisory_only_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(sena_tr_t100, senate_authority__advisory_only_reading, theater_ratio, 100, 0.55).
narrative_ontology:measurement(sena_tr_t200, senate_authority__advisory_only_reading, theater_ratio, 200, 0.65).

% Extraction over time
narrative_ontology:measurement(sena_be_t0, senate_authority__advisory_only_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sena_be_t100, senate_authority__advisory_only_reading, base_extractiveness, 100, 0.48).
narrative_ontology:measurement(sena_be_t200, senate_authority__advisory_only_reading, base_extractiveness, 200, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(sena_su_t0, senate_authority__advisory_only_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sena_su_t100, senate_authority__advisory_only_reading, suppression_requirement, 100, 0.5).
narrative_ontology:measurement(sena_su_t200, senate_authority__advisory_only_reading, suppression_requirement, 200, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(senate_authority__advisory_only_reading, attachment_coordination).
narrative_ontology:affects_constraint(senate_authority__advisory_only_reading, senate_authority__deliberative_supremacy_reading).
narrative_ontology:affects_constraint(senate_authority__advisory_only_reading, senate_authority__oligarchic_capture_reading).
narrative_ontology:affects_constraint(senate_authority__advisory_only_reading, tribuni_plebis_veto_authority).
narrative_ontology:affects_constraint(senate_authority__advisory_only_reading, gracchi_reform_constraint).

% DUAL FORMULATION NOTE:
% This constraint is part of the senate_authority kernel family. All three readings (advisory_only, deliberative_supremacy, oligarchic_capture) should be instantiated as separate JSON files with different constraint_ids and linked via network.affects_constraints. The advisory_only_reading has the lowest extractiveness (0.52, tangled_rope) because it posits genuine legal limits on senatorial power. The oligarchic_capture_reading should have higher extractiveness (snare) because it posits hidden binding authority. The deliberative_supremacy_reading should emphasize coordination (rope) because it centers the Senate's positive deliberative function. All three share the same interval and base doctrinal context but instantiate different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
