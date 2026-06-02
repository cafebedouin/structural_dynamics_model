% ============================================================================
% CONSTRAINT STORY: krypteia_opaque_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_krypteia_opaque_enforcement, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: krypteia_opaque_enforcement
 *   human_readable: Krypteia: Opaque Kernel Enforcement in Spartan Helot Control
 *   domain: ancient_politics/state_violence
 *
 * SUMMARY:
 *   The krypteia represents a paradigmatic case of opaque kernel enforcement
 *   — a state apparatus that operates through information asymmetry rather
 *   than transparent rule application. Young Spartan citizens were secretly
 *   dispatched among the helot population to identify and kill those deemed
 *   dangerous or troublesome. The killings were formally legalized through
 *   recurring declarations of war that treated helots as military enemies,
 *   obscuring murder as legitimate state action. The core innovation was
 *   opacity: helots could not know which fellow residents were krypteia
 *   agents, when or where killings would occur, or what behaviors triggered
 *   targeting. This information asymmetry created a suppression mechanism far
 *   more effective than transparent enforcement could achieve — the
 *   unpredictability itself was the weapon. The constraint reveals why
 *   anchored systems (those claiming fixed, unchangeable structures) often
 *   require opaque enforcement layers: transparent enforcement would expose
 *   the arrangement's contingency and invite organized resistance. The
 *   krypteia is simultaneously pure extraction from the helot perspective
 *   (snare), coordinated population management from the Spartan state
 *   perspective (rope), mixed coordination-plus-extraction from the young
 *   citizen perspective (tangled rope), a degraded ritual apparatus (piton),
 *   and risks naturalization as inevitable institutional response (false
 *   summit mountain). The perspectival gap is maximal.
 *
 * KEY AGENTS:
 *   - Helot Population: Primary victim (powerless/trapped) — subjugated agricultural population bound to Spartan territory, subject to random lethal violence through opaque mechanism
 *   - Young Spartan Citizens (Krypteia Initiands): Secondary agent (powerful/mobile but identity-locked) — citizens required to participate in killings as part of agoge training; mobile exit exists but identity-locked to Spartan civic identity
 *   - Spartan State Apparatus: Primary beneficiary (institutional/arbitrage) — maintains subjugated labor force through distributed, deniable violence without open warfare that would destabilize citizen-soldier recruitment
 *   - Ephorate and Gerousia: Institutional decision-makers (institutional/arbitrage) — bodies that authorized krypteia declarations and maintained the legal framework
 *   - Ritual War Declaration Mechanism: Performative apparatus (institutional/constrained) — legal scaffolding that provides deniability; itself a degraded institution (piton) whose theater obscures its pure extraction function
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent institutional violence as inevitable law of subjugation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(krypteia_opaque_enforcement, 0.78).
domain_priors:suppression_score(krypteia_opaque_enforcement, 0.88).
domain_priors:theater_ratio(krypteia_opaque_enforcement, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(krypteia_opaque_enforcement, extractiveness, 0.78).
narrative_ontology:constraint_metric(krypteia_opaque_enforcement, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(krypteia_opaque_enforcement, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(krypteia_opaque_enforcement, snare).
narrative_ontology:human_readable(krypteia_opaque_enforcement, "Krypteia: Opaque Kernel Enforcement in Spartan Helot Control").
narrative_ontology:topic_domain(krypteia_opaque_enforcement, "ancient_politics/state_violence").

domain_priors:requires_active_enforcement(krypteia_opaque_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(krypteia_opaque_enforcement, spartan_citizen_class).
narrative_ontology:constraint_beneficiary(krypteia_opaque_enforcement, spartan_military_apparatus).
narrative_ontology:constraint_victim(krypteia_opaque_enforcement, helot_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HUNTED HELOT (SNARE) — Trapped within Spartan territory with no exit option. Subject to random lethal violence administered by unknown agents following incomprehensible rules. Maximum suppression through information asymmetry: helots cannot know which Spartiates are krypteia, when violence will strike, or what triggers targeting. The constraint operates as pure extraction — coordination benefit is zero, coercion is total. Experienced extractiveness at maximum.
constraint_indexing:constraint_classification(krypteia_opaque_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: YOUNG SPARTIATE INITIAND (TANGLED ROPE) — Initiated into krypteia practice as part of citizen development. Experiences coordination: the practice solves the Spartan state's problem of helot population management without open warfare that would destabilize the citizen-soldier class. But also experiences extraction: the initiate is coerced into committing murders, internalized as duty, which fuses their civic identity to the extraction mechanism itself. Mobile exit exists (can flee Sparta) but identity-locked: to leave is to cease being Spartan. Genuine coordination function + asymmetric extraction + active enforcement = tangled rope.
constraint_indexing:constraint_classification(krypteia_opaque_enforcement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: SPARTAN STATE APPARATUS (ROPE) — Sees krypteia as pure coordination mechanism for population control. The practice solves the collective action problem of managing a subjugated population larger than the master class. Without organized killing, helot revolts would threaten Spartan dominance. With krypteia, population pressure is managed through distributed, deniable violence. The state benefits from this arrangement (arbitrage exit: can abandon the practice if better alternatives emerge). From the state's perspective, this is coordination with no experienced extraction — the extracted value flows toward the state, not away.
constraint_indexing:constraint_classification(krypteia_opaque_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: RITUAL DECLARATION OF WAR (PITON) — The krypteia formally operated under declarations of war (presumably renewed annually) that legalized killing under military law rather than civil murder statutes. This ritual framing is substantially performative: the 'war' is non-reciprocal, against an enemy incapable of fighting back, with predetermined outcome. The theater ratio is moderate-high because the entire legal apparatus exists to provide deniability rather than actual enforcement. Over time, as the institution's core function (population control through fear) becomes decoupled from its stated purpose (military discipline), the ritual persists through inertia — it is maintained not because it works but because it provides the deniability Spartan elites require. This is degraded institutional machinery masquerading as legitimate governance.
constraint_indexing:constraint_classification(krypteia_opaque_enforcement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN CANDIDATE) — A civilizational-scale observer might perceive the krypteia as an inevitable institutional response to the structural problem of subjugation. Every system of forced labor requires a suppression mechanism; the krypteia is merely the specific form that suppression takes when transparency would expose the arrangement's contingency. From this view, opaque enforcement appears as a natural law: 'Any system extracting value through permanent subjugation requires opacity to prevent coordinate rebellion.' This is a FALSE SUMMIT — a naturalized reading of a contingent institutional practice. The falsity is revealed by the structural data: identifiable beneficiaries (Spartan citizens, military apparatus), identifiable victims (helot population), and an active enforcement mechanism dependent on specific ritual scaffolding all indicate construction rather than inevitability.
constraint_indexing:constraint_classification(krypteia_opaque_enforcement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: HELOT COLLECTIVE (GENERATIONAL SNARE) — Across generational timescale, helots remain trapped in the same extraction mechanism. Children born into helotry inherit the same opacity, the same suppression, the same information asymmetry. The krypteia does not merely extract from individuals — it perpetuates the extraction structure across time. Generational perpetuation at maximum suppression with zero exit options = mountain-level immutability from the helot position. Yet from the Spartan position, it is contingent — a Spartan could organize resistance, could abolish the practice, could negotiate. The perspectival gap reveals that 'inevitable' and 'unchangeable' are position-dependent. The helot cannot change it; the Spartan could but chooses not to.
constraint_indexing:constraint_classification(krypteia_opaque_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(krypteia_opaque_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(krypteia_opaque_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(krypteia_opaque_enforcement, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(krypteia_opaque_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(krypteia_opaque_enforcement, TR),
    TR >= 0.70.

:- end_tests(krypteia_opaque_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. The krypteia extracts total subjugation — life itself is contingent on state tolerance. Helots are killed without trial, without warning, without transparent criteria. The extraction rises over the interval (0.65 to 0.78) as the practice becomes more embedded in Spartan culture and more institutionalized; the initial period may have had lower intensity or less systematic application. Suppression (0.88): Near-maximum. Information asymmetry is the primary suppression mechanism: helots cannot predict or prepare for violence because they do not know which of their neighbors are state agents or when violence will occur. No legal recourse, no due process, no escape routes. Suppression is limited only by the Spartan requirement to keep helots alive as a labor force — total annihilation would defeat the subjugation's economic purpose. Theater ratio (0.65): Moderate-high. The ritual declaration of war provides performative legal cover for what is functionally murder. The theater serves the elites (provides deniability) more than it constrains the violence (killings occur regardless of legal framing). The theater ratio may rise over time as the practice becomes more ritualized and the gap between legal fiction and operational reality widens. Claimed type: Snare. From the helot perspective, this is unambiguously snare — maximum extractiveness, maximum suppression, no coordination benefit, no exit option, existence depends entirely on remaining below the threshold for state-approved killing.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a near-maximal perspectival gap. Helots classify it as snare (pure extraction, no coordination benefit, no exit). Young Spartiates classify it as tangled rope (coordination function for population control, but also coerced participation, identity fusion). The Spartan state classifies it as rope (pure coordination, solves population management problem). The analytical observer at civilizational scope risks classifying it as mountain (inevitable response to subjugation) when the structural data reveals it as a false summit (contingent institutional choice with identifiable beneficiaries and victims). The gap is sharpest between the helot perspective (trapped/biographical) and the state perspective (institutional/generational). The helot sees immutable lethal threat; the state sees manageable governance mechanism. Neither perspective is wrong — they are measuring from incommensurable structural positions. The krypteia's extractiveness appears nearly infinite from the helot perspective because it literally threatens life itself; it appears moderate from the state perspective because it maintains a stable subjugated population without requiring constant overt force. The theater ratio divides along the same line: helots experience the ritual declaration as purely theatrical (their actual status is 'perpetually in danger'), while the state treats the ritual as genuine legal mechanism that transforms murder into lawful killing.
 *
 * DIRECTIONALITY LOGIC:
 *   Helot position: Victim + trapped exit = maximum d (approaching 1.0) → maximum f(d) → maximum χ. Helots bear the full weight of the extraction with no agency and no exit capacity. Spartiate position: Beneficiary + mobile exit (can leave Sparta, can refuse participation in principle) but constrained by identity fusion = moderate d. The derivation from victim status would normally produce high d, but the Spartiates are beneficiaries, not victims, of this constraint. However, they are also coerced into participating in killings. This is the critical asymmetry: from the state's perspective, the Spartiates are willing agents (beneficiaries) of coordination. From the young citizen's perspective, they are coerced into extraction (victims of the identity-locking mechanism). The tangled rope classification reflects this: genuine coordination (population control) layered with genuine coercion (identity requirement to kill). State apparatus position: Beneficiary + arbitrage exit = low d. The state experiences no extraction; it only extracts. The constraint runs entirely in one direction from the state's perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The krypteia resolves the mandatrophy by demonstrating that opaque enforcement emerges not from coordination requirements but from the need to maintain extraction while preventing organized resistance. A transparent system (e.g., 'all helots aged 20-30 may be executed without cause') would expose the subjugation's contingency and invite coordinated rebellion. Opacity accomplishes two functions simultaneously: (1) Information asymmetry prevents coordination among potential resisters, and (2) The unpredictability of enforcement creates terror that subdues resistance intent. This is pure snare from the victim perspective because no coordination benefit exists — helots gain nothing from the arrangement except continued survival at the sufferance of the state. From the beneficiary perspective, it is tangled rope because the state apparatus genuinely solves a coordination problem (maintaining subjugation without constant overt warfare) while actively coercing citizens into extraction (killing). The mandatrophy is resolved by recognizing that tangled rope and snare are the same physical arrangement viewed from opposite positions. The constraint is not ambiguous in type — it is unambiguously snare from the helot perspective and unambiguously tangled rope from the Spartan state perspective. Both classifications are correct because they measure the same constraint from structurally incommensurable positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_war_declaration_efficacy,
    'Do the ritual declarations of war against helots actually provide legal cover that Spartan society genuinely accepted, or are they post-hoc justifications that elites use while citizens understand the practice as extra-legal?',
    'Historical analysis of Spartan legal texts, ephoral decrees, and citizen discourse; examination of whether helot killings outside krypteia context were prosecuted differently or if killing helots was broadly decriminalized',
    'If accepted as legitimate: ritual framing is functional (theater ratio lower, more Rope from state perspective). If post-hoc: ritual is purely performative (theater ratio higher, confirms Piton classification of the legal mechanism itself).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ritual_war_declaration_efficacy, empirical, 'Whether ritual war declarations provided genuine legal cover or were post-hoc justification').

omega_variable(
    helot_knowledge_of_krypteia_scope,
    'Did helots possess any reliable knowledge about the scale, frequency, or targeting criteria of krypteia killings, or was opacity complete?',
    'Analysis of helot resistance patterns, rebellion triggers, and helot-authored accounts (limited but existent in ancient sources); correlation between helot uprisings and periods of intense vs relaxed krypteia activity',
    'If helots had reliable knowledge: suppression was lower than estimated, information asymmetry was partial, tactics for resistance were possible. If opacity was complete: suppression at maximum, resistance required collective action without information, catastrophic coordination barriers. Affects classification from helot perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(helot_knowledge_of_krypteia_scope, empirical, 'Degree of helot knowledge about krypteia operations').

omega_variable(
    citizen_identity_fusion_mechanism,
    'To what extent did krypteia participation become fused with Spartan civic identity such that citizens could not imagine refusing participation without ceasing to be Spartan?',
    'Analysis of Spartan education narratives, warrior culture mythology, and historical accounts of resistance to or critique of the practice; examination of whether any known Spartan citizens refused krypteia participation and what happened to them',
    'High fusion: tangled rope classification from Spartiate perspective is correct (identity-locked exit). Low fusion: classification should shift toward snare-from-Spartiate position if killing is coerced by law despite low identity fusion. Affects whether the constraint binds through internalization or external force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizen_identity_fusion_mechanism, conceptual, 'Identity fusion between Spartan citizenship and krypteia participation').

omega_variable(
    alternative_population_control_viability,
    'Were transparent, less lethal population control mechanisms (e.g., regulated breeding restrictions, deportation, incentivized emigration) viable alternatives that Spartan elites consciously rejected, or structurally impossible given Spartan demographics and ideology?',
    'Comparative analysis of population control in other ancient subjugation systems; economic modeling of helot reproduction rates and Spartan labor demand; ideological analysis of whether Spartan values could have accommodated non-lethal alternatives',
    'If alternatives were viable: opacity is a deliberate choice to maximize terror (pure snare, no coordination function). If alternatives were impossible: the tangled rope perspective (coordination + extraction) is more accurate — the Spartiates genuinely face a coordination problem with limited solutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_population_control_viability, empirical, 'Viability of alternative population control mechanisms').

omega_variable(
    false_summit_naturalization,
    'Is the perception of krypteia as inevitable institutional response to subjugation a genuine structural law, or a naturalized reading of a contingent Spartan choice?',
    'Comparative analysis of subjugation systems across history: do all require opaque enforcement, or did some maintain subjugation through transparency? Analysis of whether Sparta could have maintained helot suppression through transparent law and organized force rather than distributed opacity.',
    'If inevitable: mountain classification is accurate. If contingent: mountain is false summit. Reveals whether opacity is intrinsic to extraction or instrumental choice by Spartan elites.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether opaque enforcement is inevitable for subjugation or contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(krypteia_opaque_enforcement, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(krypt_tr_t0, krypteia_opaque_enforcement, theater_ratio, 0, 0.55).
narrative_ontology:measurement(krypt_tr_t50, krypteia_opaque_enforcement, theater_ratio, 50, 0.6).
narrative_ontology:measurement(krypt_tr_t100, krypteia_opaque_enforcement, theater_ratio, 100, 0.65).

% Extraction over time
narrative_ontology:measurement(krypt_be_t0, krypteia_opaque_enforcement, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(krypt_be_t50, krypteia_opaque_enforcement, base_extractiveness, 50, 0.72).
narrative_ontology:measurement(krypt_be_t100, krypteia_opaque_enforcement, base_extractiveness, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(krypteia_opaque_enforcement, enforcement_mechanism).
narrative_ontology:affects_constraint(krypteia_opaque_enforcement, helot_rebellion_suppression).
narrative_ontology:affects_constraint(krypteia_opaque_enforcement, spartan_citizen_identity_militarization).
narrative_ontology:affects_constraint(krypteia_opaque_enforcement, ancient_subjugation_transparency_paradox).

% DUAL FORMULATION NOTE:
% The krypteia is related to but structurally distinct from transparent helot law enforcement. A companion constraint story (helot_rebellion_suppression) models the general problem of maintaining subjugation; this story models the specific krypteia solution of opaque distributed enforcement. Another companion story (spartan_citizen_identity_militarization) models the fusion of Spartan identity with martial violence, which the krypteia institutionalizes. A third story (ancient_subjugation_transparency_paradox) models the general claim that all subjugation systems require opacity; the krypteia is a concrete instantiation of that claim at the specific/regional scope rather than the universal scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(krypteia_opaque_enforcement, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
