% ============================================================================
% CONSTRAINT STORY: dionysaic_frenzy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dionysaic_frenzy, []).

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
 *   constraint_id: dionysaic_frenzy
 *   human_readable: Dionysiac Religious Possession
 *   domain: religious/social
 *
 * SUMMARY:
 *   Dionysiac religious possession in classical Greece creates a structural
 *   tension between genuinely altered ecstatic experience and institutionally
 *   controlled ritual extraction. Worshippers — predominantly women and
 *   enslaved persons — are called by Dionysus to abandon social roles and
 *   participate in mountain rites involving ecstatic dancing, self-wounding,
 *   animal sacrifice, and transgressive behavior. The state and priesthood
 *   institutionalize this potentially destabilizing phenomenon by licensing
 *   it, setting ritual calendars, and incorporating it into official
 *   religion. This transforms spontaneous madness into managed religious
 *   duty. The constraint operates as hybrid coordination-extraction: it
 *   coordinates female religious participation and manages social tension
 *   through controlled transgression, while simultaneously extracting
 *   autonomy, bodily agency, and labor from those seized by possession. Over
 *   the interval from classical to Hellenistic periods, the theater ratio
 *   increases as philosophical critiques (Plato, Aristotle) provide
 *   non-theological explanations for ecstatic states, and Roman-period
 *   sources show possession becoming increasingly performative rather than
 *   compulsive. The endpoint of this trajectory is a Piton — the ritual form
 *   persists through institutional inertia and aesthetic tradition while the
 *   underlying mechanism of genuine divine compulsion has degraded.
 *
 * KEY AGENTS:
 *   - Maenads and Possessed Subjects: Primary victims (powerless/trapped) — seized by ecstatic compulsion with no socially recognized refusal option; abandoned family and social status
 *   - Families and Households: Secondary victims (moderate/constrained) — lose members to ecstatic practice; experience extraction of female labor and autonomy; constrained exit (social cost of opposition)
 *   - Dionysian Priesthood: Primary beneficiary (institutional/arbitrage) — manage the ritual apparatus, interpret divine will, maintain religious authority and institutional prestige
 *   - State Cult Authority: Primary beneficiary (institutional/arbitrage) — institutionalize ecstatic impulse to prevent destabilizing spontaneous movements; integrate possession into official religion for legitimacy and social cohesion
 *   - Philosophical Critique Movement: Organized actors (organized/constrained) — develop natural philosophy explanations for possession, creating intellectual pathway to sunset the constraint through medicalization and rationalization
 *   - Ritual Ceremonial Apparatus: Institutional actor (institutional/arbitrage) — elaborate machinery of masks, costumes, sacrifice persists through inertia despite degradation of genuine possession mechanism
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — reveals how divine compulsion naturalizes institutional extraction; identifies false justification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dionysaic_frenzy, 0.52).
domain_priors:suppression_score(dionysaic_frenzy, 0.68).
domain_priors:theater_ratio(dionysaic_frenzy, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dionysaic_frenzy, extractiveness, 0.52).
narrative_ontology:constraint_metric(dionysaic_frenzy, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dionysaic_frenzy, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dionysaic_frenzy, tangled_rope).
narrative_ontology:human_readable(dionysaic_frenzy, "Dionysiac Religious Possession").
narrative_ontology:topic_domain(dionysaic_frenzy, "religious/social").

domain_priors:requires_active_enforcement(dionysaic_frenzy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dionysaic_frenzy, dionysian_priesthood).
narrative_ontology:constraint_beneficiary(dionysaic_frenzy, state_cult_authority).
narrative_ontology:constraint_victim(dionysaic_frenzy, maenads).
narrative_ontology:constraint_victim(dionysaic_frenzy, possessed_subjects).
narrative_ontology:constraint_victim(dionysaic_frenzy, family_social_bonds).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE POSSESSED MAENAD (SNARE) — Women seized by ecstatic possession experience the constraint as total extraction. Once called by Dionysus, they abandon family, property, and social status. No exit mechanism exists — resistance to the call is framed as impiety. Suppression is maximal: the religious framework naturalizes the seizure as divine compulsion, eliminating the cognitive space for refusal. The maenad's structural position is trapped victim with no alternative but flight to mountain rites or social death.
constraint_indexing:constraint_classification(dionysaic_frenzy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: HOUSEHOLD AND FAMILY BONDS (TANGLED ROPE) — Families experience the constraint as both coordination and extraction. The possession system coordinates seasonal ritual participation and creates institutional identity through cultic membership. But it also systematically extracts family members — daughters, wives, enslaved women — into ecstatic practices. Families have partial exit options (relocation, social status defense) but at high cost. The coordination benefit (religious legitimacy, calendar coordination) is asymmetrically distributed: state and priesthood benefit more than households.
constraint_indexing:constraint_classification(dionysaic_frenzy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE DIONYSIAN PRIESTHOOD (ROPE) — The priesthood experiences the possession system as pure coordination. They manage the ritual calendar, interpret divine will, and maintain the theurgic machinery that channels Dionysiac madness into controllable ecstatic events. Their exit options are maximal arbitrage: they can reframe the ritual emphasis, introduce new interpretations, or migrate to alternative cults. The constraint solves their coordination problem — maintaining social order while channeling dangerous ecstatic impulses into managed seasonal rituals. Net benefit: institutional authority, ritual prestige, continued social legitimacy.
constraint_indexing:constraint_classification(dionysaic_frenzy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE STATE CULT AUTHORITY (ROPE) — The state integrates Dionysiac possession into official religion to manage social cohesion and contain potentially destabilizing ecstatic impulses. By institutionalizing the madness — licensing priesthoods, setting ritual calendars, making possession a state-recognized religious duty — the authority converts a threat into a managed resource. Exit options are arbitrage: the state can adjust licensing, reinterpret the god's will, or shift ritual emphasis. The constraint provides coordination benefit: legitimacy for female participation in religious life (limited but real), outlet for social tension, and ritual reinforcement of civic order.
constraint_indexing:constraint_classification(dionysaic_frenzy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE PHILOSOPHICAL CRITIQUE MOVEMENT (SCAFFOLD) — By the 5th-4th centuries BCE, Platonic and Aristotelian philosophers begin systematically questioning ecstatic possession as divine rather than psychological or neurological. This organized critique (Plato's Phaedrus acknowledges mania as divine, but later thinkers medicalize it) creates an alternative explanatory framework that removes possession from the domain of religious compulsion. The sunset mechanism is intellectual: as natural philosophy provides non-theological explanations for ecstatic states, the theological compulsion loses force. Theater ratio declines as possession becomes rationalizable rather than inherently mysterious. The philosophers have constrained exit (risk of impiety charges) but see a generational pathway out.
constraint_indexing:constraint_classification(dionysaic_frenzy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: THE RITUAL CEREMONIAL APPARATUS (PITON) — The elaborate machinery of Dionysiac rites — costumes, masks, processions, animal sacrifice, sacred space — persists long after the mechanism of genuine ecstatic seizure has degraded. By the Roman period, Dionysiac possession becomes largely performative: participants wear the costumes and go through the motions, but the experience of divine compulsion has become optional or theatricalized. The ritual form persists through institutional inertia (tradition, legal requirement, aesthetic investment) rather than functional constraint. Theater ratio is high (0.70+) because the ritual's primary function has shifted from channeling real possession to maintaining cultural continuity and entertainment value.
constraint_indexing:constraint_classification(dionysaic_frenzy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational anthropological perspective, the Dionysiac possession system is a structurally hybrid coordination-extraction mechanism. It coordinates seasonal religious participation, manages gender norms through controlled transgression, and provides outlet for social tension. But it systematically extracts labor, autonomy, and bodily agency from those most available for possession (women, enslaved persons). The state and priesthood benefit more than subjects. The mechanism requires active enforcement through religious authority and social sanction. The analytical view reveals the false justification: 'divine compulsion' naturalizes what is actually institutional extraction packaged as religious experience.
constraint_indexing:constraint_classification(dionysaic_frenzy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dionysaic_frenzy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dionysaic_frenzy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dionysaic_frenzy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dionysaic_frenzy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dionysaic_frenzy, TR),
    TR >= 0.70.

:- end_tests(dionysaic_frenzy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The original subjects (maenads) experience maximal extraction — loss of autonomy, social status, family bonds, bodily control. But from the analytical view, the constraint is not pure extraction: it genuinely coordinates female religious participation (real coordination benefit) and provides outlet for social tension (real structural function). The extraction is real but embedded in genuine coordination. The value 0.52 reflects that this is legitimately a hybrid system, not a pure extractor. Suppression (0.68): High. The religious framework naturalizes seizure as divine compulsion, eliminating cognitive space for refusal. Social status loss for refusing the call is severe. But suppression is not total (0.95): some participants may have partial agency within the ecstatic framework, and philosophical critiques begin creating alternative explanations. Theater ratio (0.58): Moderate-high and rising. Classical-period accounts emphasize genuine altered states and compulsion. By Hellenistic and Roman periods, theatrical elements increase: costumes, masks, dramatic narratives overlay or replace descriptions of spontaneous ecstasy. The trajectory from 0.32 to 0.58 shows increasing performative content as the mechanism of genuine possession degrades. Claimed type: Tangled Rope. The constraint has both coordination function (manages seasonal ritual, integrates female participation, outlets for social tension) and extraction mechanism (takes autonomy, labor, bodily agency from victims). Active enforcement is required (priesthood authority, social sanction for refusal). Beneficiaries and victims are structurally distinct (priesthood/state vs. maenads/families).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence across contexts. The possessed maenad sees Snare: total extraction with no exit. The family sees Tangled Rope: some coordination benefit (religious status, seasonal participation) mixed with extraction (loss of female members). The priesthood sees pure Rope: they are coordinating an important social function and experiencing no extraction, only benefit. The state sees Rope: they are solving a coordination problem of containing ecstatic impulses. The philosophical critique movement sees Scaffold: they are building an intellectual exit pathway that will sunset the constraint through naturalization. The ritual apparatus sees Piton: the elaborate form persists despite degradation of function. The analytical observer sees Tangled Rope: genuine coordination embedded in extraction system, with false theological justification. The perspectival gap arises because beneficiaries (priesthood, state) genuinely experience coordination benefits while victims (maenads) genuinely experience extraction. Both are structurally correct from their positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from structural position. Maenads occupy the victim/trapped position: high d (0.90+) → high f(d) → high experienced extraction (χ). Families are victims with constrained exit: moderate-high d (0.70-0.80) → high f(d) → moderate-high extraction. Priesthood occupies beneficiary/arbitrage position: low d (0.10-0.15) → low f(d) → negative extraction (benefits flow toward them). State occupies beneficiary/arbitrage position: low d (0.05-0.10) → negative f(d) → net benefit. The scope modifier σ(S) = 0.9 (regional) slightly dampens but does not reverse these directional flows. The perspectival gap is mathematically encoded: the same constraint produces high χ for trapped victims and negative χ for beneficiaries with arbitrage options.
 *
 * MANDATROPHY ANALYSIS:
 *   The Dionysiac frenzy resolves the mandatrophy by showing that the constraint is legitimately hybrid rather than mislabeled. The false temptation is to classify it as pure Snare (which would emphasize extraction) or pure Rope (which would emphasize coordination). The Tangled Rope classification captures that both are structurally real: the coordination function (seasonal ritual management, female religious participation, social outlet) is genuine, not theatrical. The extraction (loss of autonomy, status, family bonds) is also genuine, not incidental. The active enforcement (priesthood authority, social sanction) is required to maintain both the coordination and the extraction simultaneously. A pure Snare classification would miss the real coordination benefit and wrongly imply that the system serves no function. A pure Rope classification would miss the systematic extraction from maenads and the asymmetric benefit distribution. The Tangled Rope correctly identifies a system where both mechanisms operate and both beneficiaries and victims are structurally essential to the constraint's persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_vs_induced_possession,
    'Is Dionysiac ecstatic possession a genuine altered neurological state (naturally occurring seizure-like phenomena, pharmacologically induced, or dissociative) or a learned social performance reinforced by religious authority?',
    'Neurological analysis of descriptions in Greek medical texts (Hippocratic corpus); cross-cultural comparison with documented possession states; pharmacological analysis of substances used in Dionysiac ritual (ergot contamination, wine fermentation compounds, psychoactive plants); ethnographic documentation of modern reenactors'' phenomenology',
    'If genuine altered state: possession system is coordination of natural phenomenon (constraint type shifts toward Mountain or Rope). If learned performance: system is pure extraction through authority manipulation (constraint confirms as Snare or Tangled Rope). If mixed: current Tangled Rope classification is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_vs_induced_possession, empirical, 'Whether possession is genuine altered state or socially learned performance').

omega_variable(
    female_agency_and_resistance,
    'To what extent did maenads experience possession as liberatory transgression (agency) versus coercive extraction (victimization)? Did participants have meaningful refusal options or cognitive space for resistance?',
    'Close reading of Euripides'' Bacchae and other dramatic texts for signs of female perspective; analysis of social position before and after participation; comparison with accounts from less institutionalized ecstatic traditions; examination of punishment for refusal or failed possession',
    'If primarily liberatory: classification shifts toward Rope (coordination benefit) from female perspective. If primarily coercive: classification confirms Snare. If mixed by class/status: Tangled Rope confirmed with refined beneficiary/victim categories.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(female_agency_and_resistance, conceptual, 'Degree of female agency and refusal options in possession practice').

omega_variable(
    state_control_vs_containment,
    'Did the state institutionalize Dionysiac possession to increase social control, or to contain potentially destabilizing spontaneous ecstatic movements?',
    'Historical comparison of state response to unlicensed versus official Dionysiac cults; examination of legal restrictions on possession and ritual participation; analysis of geographical spread and intensity of state-sanctioned versus suppressed movements; textual evidence of state anxiety about uncontrolled possession',
    'If control: state is clear beneficiary extracting compliance via religious mechanism (Snare from state perspective). If containment: state is solving coordination problem of managing threat (Rope from state perspective). Likely both, supporting Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_control_vs_containment, empirical, 'Whether state institutionalization was driven by control or containment logic').

omega_variable(
    theater_ratio_trajectory,
    'At what historical point did Dionysiac possession shift from genuine ecstatic experience to performative ritual theater? What structural changes marked the transition?',
    'Diachronic analysis of Greek texts (Aeschylus through late Hellenistic period) for language of compulsion vs. choice; Roman sources showing medicalization and dramatization; iconographic changes in art depicting possession; rise of theatrical Dionysiac drama alongside ritual practice',
    'If early (5th century): theater ratio was always high, and constraint was Piton from inception. If late (Hellenistic/Roman): theater ratio increased over time, supporting lifecycle drift to Piton. Either way, confirms current theater_ratio measurement as end-state rather than steady state.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_ratio_trajectory, empirical, 'Historical timing of shift from genuine ecstasy to performative theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dionysaic_frenzy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dion_tr_t0, dionysaic_frenzy, theater_ratio, 0, 0.32).
narrative_ontology:measurement(dion_tr_t5, dionysaic_frenzy, theater_ratio, 5, 0.45).
narrative_ontology:measurement(dion_tr_t10, dionysaic_frenzy, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(dion_be_t0, dionysaic_frenzy, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(dion_be_t5, dionysaic_frenzy, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(dion_be_t10, dionysaic_frenzy, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dionysaic_frenzy, enforcement_mechanism).
narrative_ontology:affects_constraint(dionysaic_frenzy, greek_gender_norms).
narrative_ontology:affects_constraint(dionysaic_frenzy, state_religious_authority).

% DUAL FORMULATION NOTE:
% The Dionysiac possession system can be decomposed into (1) the neurological/phenomenological constraint of ecstatic altered states (empirical question of whether genuine vs. performed) and (2) the institutional constraint of religious authority extraction through possession framing. These are linked but distinct: the first is potentially a natural law or coordination mechanism, the second is institutional extraction. The current story emphasizes the institutional layer (which is why theater_ratio is non-trivial) and treats the phenomenological layer as background. A fully decomposed analysis would separate empirical possession dynamics from institutional control mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dionysaic_frenzy, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
