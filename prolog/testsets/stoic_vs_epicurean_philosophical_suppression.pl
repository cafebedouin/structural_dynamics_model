% ============================================================================
% CONSTRAINT STORY: stoic_vs_epicurean_philosophical_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stoic_vs_epicurean_philosophical_suppression, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: stoic_vs_epicurean_philosophical_suppression
 *   human_readable: Stoic vs Epicurean Philosophical Suppression of Pleasure and Desire
 *   domain: philosophy/ethics/intellectual_history
 *
 * SUMMARY:
 *   The suppression of pleasure and desire through stoic and ascetic
 *   philosophical frameworks represents a constraint that has operated across
 *   cultures and centuries, shaping how billions of humans understand
 *   wellbeing, virtue, and flourishing. The constraint exhibits the classical
 *   tangled rope structure: genuine coordination function (stoic frameworks
 *   enable social cohesion, institutional discipline, and shared moral
 *   meaning) layered over asymmetric extraction (suppression of epicurean
 *   alternatives, internalized guilt around pleasure-seeking, institutional
 *   gatekeeping that prevents heterodox wellbeing pathways). The constraint
 *   has degraded over the interval (0-20): extractiveness declined from 0.72
 *   to 0.52 as neuroscience, positive psychology, and pluralist philosophy
 *   increasingly legitimized moderate pleasure-seeking and revealed ascetic
 *   suppression as partly institutional theater. Theater ratio rose from 0.35
 *   to 0.48, reflecting that performative ascetic display (monastic ritual,
 *   self-denial signaling) now constitutes a larger portion of the
 *   constraint's mechanism as rational justification has weakened. The
 *   suppression mechanism operates through multiple channels: identity fusion
 *   (practitioners internalize the moral hierarchy that pleasure = spiritual
 *   failure), institutional gatekeeping (ascetic frameworks control
 *   educational and religious authority), and false naturalization (the
 *   hedonic treadmill is claimed as a law of nature rather than a contingent
 *   neurological fact).
 *
 * KEY AGENTS:
 *   - Practitioners Seeking Wellbeing: Primary victim (powerless/identity_locked) — internalized renunciation of pleasure; identity constituted through ascetic virtue; cannot exit without abandoning self-concept
 *   - Epicurean Heterodox Minority: Secondary victim (moderate/constrained) — socially penalized for pursuing hedonistic frameworks; face intellectual ostracism and institutional barriers to influence
 *   - Stoic Philosophical Establishment: Primary beneficiary (institutional/arbitrage) — controls epistemic authority over wellbeing definitions; suppresses competing frameworks; maintains gatekeeping power
 *   - Ascetic Institutional Authority: Secondary beneficiary (institutional/arbitrage) — religious, monastic, and educational institutions enforce suppression; benefit from compliance signals and social control
 *   - Philosophical Pluralist Coalition: Tertiary agent (organized/mobile) — increasingly treating frameworks as complementary; reducing suppression through institutional pluralization
 *   - Embodied Human Flourishing: Abstract victim (powerless/trapped) — the constraint limits exploration of wellbeing pathways that might maximize flourishing; cannot organize or exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stoic_vs_epicurean_philosophical_suppression, 0.58).
domain_priors:suppression_score(stoic_vs_epicurean_philosophical_suppression, 0.65).
domain_priors:theater_ratio(stoic_vs_epicurean_philosophical_suppression, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stoic_vs_epicurean_philosophical_suppression, extractiveness, 0.58).
narrative_ontology:constraint_metric(stoic_vs_epicurean_philosophical_suppression, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(stoic_vs_epicurean_philosophical_suppression, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stoic_vs_epicurean_philosophical_suppression, tangled_rope).
narrative_ontology:human_readable(stoic_vs_epicurean_philosophical_suppression, "Stoic vs Epicurean Philosophical Suppression of Pleasure and Desire").
narrative_ontology:topic_domain(stoic_vs_epicurean_philosophical_suppression, "philosophy/ethics/intellectual_history").

domain_priors:requires_active_enforcement(stoic_vs_epicurean_philosophical_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stoic_vs_epicurean_philosophical_suppression, stoic_philosophical_establishment).
narrative_ontology:constraint_beneficiary(stoic_vs_epicurean_philosophical_suppression, ascetic_institutional_authority).
narrative_ontology:constraint_victim(stoic_vs_epicurean_philosophical_suppression, practitioners_seeking_wellbeing).
narrative_ontology:constraint_victim(stoic_vs_epicurean_philosophical_suppression, epicurean_philosophical_tradition).
narrative_ontology:constraint_victim(stoic_vs_epicurean_philosophical_suppression, embodied_human_flourishing).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRACTITIONER SEEKING WELLBEING (SNARE) — Identity-locked to the philosophical framework that defines wellbeing as renunciation of pleasure. The practitioner has internalized the moral hierarchy: pursuit of pleasure = spiritual failure. Exit would require abandoning the identity frame that constitutes their self-concept as ethically evolved. Structurally mobile (could pursue hedonism), but cognitively trapped by the identity fusion with ascetic virtue. Maximum experienced extraction because the suppression mechanism is internalized.
constraint_indexing:constraint_classification(stoic_vs_epicurean_philosophical_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: EPICUREAN HETERODOX MINORITY (SNARE) — Constrained by social penalty, intellectual ostracism, and institutional gatekeeping. Pursuing hedonistic philosophy in societies dominated by stoic/ascetic institutional frameworks incurs reputation damage, employment risk, and exclusion from respectable discourse. Material barriers (not philosophical persuasion) suppress epicurean alternatives. High extraction — the constraint prevents exit through coercive overhead, not internalized frame.
constraint_indexing:constraint_classification(stoic_vs_epicurean_philosophical_suppression, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STOIC PHILOSOPHICAL ESTABLISHMENT (TANGLED ROPE) — Genuine coordination function: stoic frameworks coordinate community virtue ethics, social cohesion, and institutional discipline across cultures and centuries. Provides real benefits to followers (meaning, community, moral clarity). Simultaneously extracts through gatekeeping epistemic authority, suppressing competing frameworks (epicureanism) that might offer alternative wellbeing pathways. Net beneficiary with arbitrage exit — can shift frameworks without material cost but benefits from the constraint's enforcement.
constraint_indexing:constraint_classification(stoic_vs_epicurean_philosophical_suppression, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PHILOSOPHICAL PLURALIST COALITION (ROPE) — Organized agents (comparative philosophy programs, secular humanists, wellbeing researchers) increasingly treat stoic and epicurean frameworks as complementary rather than antagonistic. Mobile exit — can shift intellectual landscape through institutional channels (universities, publishing, cross-cultural dialogue). Low effective extraction because the coalition has agency and declining suppression as pluralist norms spread. Pure coordination function: enabling coexistence of frameworks rather than dominance of one.
constraint_indexing:constraint_classification(stoic_vs_epicurean_philosophical_suppression, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: ASCETIC INSTITUTIONAL THEATER (PITON) — Religious and educational institutions maintain stoic/ascetic suppression of pleasure largely through performative ritual: monastic vows, ascetic diet restrictions, self-denial displays. The theater persists because it signals virtue and discipline to followers, not because empirical evidence supports that renunciation increases wellbeing. Institutional inertia maintains the constraint despite weakening functional rationale (neuroscience and positive psychology increasingly validate moderate pleasure-seeking). Theater ratio 0.48 reflects mixed function and performance.
constraint_indexing:constraint_classification(stoic_vs_epicurean_philosophical_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL LAW VIEW (MOUNTAIN) — From a naturalizing perspective, human capacity for sustained pleasure is inherently limited; organisms habituate to stimuli; the hedonic treadmill is a law of neuroscience. Epicureanism's promise of stable wellbeing through pleasure is therefore impossible — the constraint emerges naturally from psychological limits. However, this perspective risks naturalizing what is partly contingent institutional framing. The engine's false summit detection will flag this classification as a potential misapplication of the mountain type.
constraint_indexing:constraint_classification(stoic_vs_epicurean_philosophical_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stoic_vs_epicurean_philosophical_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(stoic_vs_epicurean_philosophical_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(stoic_vs_epicurean_philosophical_suppression, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(stoic_vs_epicurean_philosophical_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(stoic_vs_epicurean_philosophical_suppression, TR),
    TR >= 0.70.

:- end_tests(stoic_vs_epicurean_philosophical_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high but declining. Initial assessment (0.72) reflected maximum suppression when ascetic frameworks had near-monopoly on institutional authority and philosophical respectability. Current value (0.52) reflects erosion as secular wellbeing science, positive psychology, and pluralist norms have legitimized pleasure-seeking and revealed ascetic suppression as partly institutional rather than empirically necessary. The constraint retains extractiveness because internalized identity-fusion still binds many practitioners, and institutional gatekeeping still penalizes epicurean alternatives. Suppression (0.65): Moderate-high and stable. The suppression is not total coercion (practitioners are not physically prevented from pursuing pleasure) but operates through social penalty, institutional gatekeeping, and internalized moral judgment. Theater ratio (0.48): Moderate and rising. Initial theater ratio (0.35) reflected substantial functional coordination (stoic frameworks genuinely enable social cohesion). Rising theater ratio reflects increasing performative content (ascetic displays signaling virtue rather than functional necessity) as rational justification for suppression has weakened. The constraint is becoming increasingly piton-like — maintained through institutional inertia and performative ritual rather than robust functional or empirical justification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates profound perspectival divergence from identical structural data. The practitioner sees a snare — they are trapped by internalized moral hierarchy that makes pleasure unthinkable; maximum experienced extraction. The stoic establishment sees a rope — they perceive genuine coordination function (community virtue, social stability) and their own position as beneficiary-but-not-exploiter. The pluralist coalition sees a rope shifting toward scaffold — they perceive sunset logic as intellectual pluralism and neuroscience legitimize alternative frameworks. The analytical observer risks seeing a mountain (natural law of psychological limitation via hedonic treadmill) but structural analysis reveals this as false naturalization. The perspectival gaps emerge because: (1) the binding mechanisms differ (identity-fusion vs institutional gatekeeping vs genuine coordination); (2) the time horizons differ (practitioner in biographical trap; establishment in immediate beneficiary position; coalition in generational exit pathway; analyst in civilizational naturalization); (3) the exit options differ (identity_locked practitioners cannot exit by changing external circumstances; constrained heterodox minorities face material barriers; institutional beneficiaries face no barrier). These gaps are not perceptual errors but structurally real differences in experienced constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The stoic_vs_epicurean constraint operates through multiple extraction channels with different directionalities: (1) Identity-fusion mechanism: practitioners internalize the moral framing that pleasure = spiritual failure; their d value is near-maximum (0.90) despite nominally 'choosing' suppression because the choice is made within a captured identity frame; (2) Institutional gatekeeping: epicurean frameworks are penalized in academic, religious, and educational institutions; heterodox agents experience high d (0.80) due to reputational and employment barriers; (3) Philosophical authority: stoic establishment controls which frameworks count as 'sophisticated' vs 'base'; beneficiaries experience low d (0.15) because the extraction runs toward them through epistemic privilege. The directionality derivation captures these mechanisms: beneficiary status → low d; victim + trapped exit → high d; victim + constrained exit → moderately high d; beneficiary + arbitrage exit → low d.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is partially but not fully resolved. The tangled rope classification is robust: the stoic/ascetic framework demonstrates genuine coordination function (social cohesion, institutional discipline, meaning-making) alongside asymmetric extraction (suppression of competing wellbeing frameworks, internalized guilt, gatekeeping authority). The constraint cannot be classified as pure rope (coordination only) because the suppression of epicureanism is not functionally necessary to achieve coordination — pluralist societies demonstrate equivalent social stability with coexisting frameworks. The constraint cannot be classified as pure snare (extraction only) because genuine coordination benefits flow to practitioners (community, moral clarity, structured meaning). The tangled rope classification holds. The mandatrophy risk is the mountain perspective — false naturalization of institutional suppression as psychological law. The engine's false summit detector will flag this: the mountain classification requires ε ≤ 0.25 and suppression ≤ 0.05, but this constraint has ε = 0.58 and suppression = 0.65, failing both gates. The natural law reading is revealed as a cover story for institutional extraction. Philosophical pluralism and neuroscience together demonstrate that the 'law' is contingent: pleasure-seeking need not lead to hedonic treadmill; moderate hedonism can coexist with virtue ethics; institutional suppression is not empirically justified. The constraint is clarified as tangled rope, not mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_fusion_vs_rational_persuasion,
    'Is the practitioner''s suppression of pleasure driven by internalized identity (genuinely believes pleasure-seeking is evil) or by rational acceptance of stoic arguments that renunciation leads to better outcomes?',
    'Philosophical dialogue and existential confrontation: does the practitioner maintain commitment to asceticism when rational arguments for moderate hedonism are presented? If commitment persists despite loss of rational justification, binding is identity-fusion; if commitment updates with argument, binding is rational persuasion.',
    'If identity-fusion: classification is snare with identity_locked exit (cognitive capture). If rational persuasion: classification shifts to rope (voluntary coordination with stoic framework). Identity-fusion suggests the constraint is more extractive than it appears; rational persuasion suggests it is more legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_vs_rational_persuasion, empirical, 'Whether suppression of pleasure is identity-based or rationally persuaded').

omega_variable(
    pleasure_measurement_incommensurability,
    'Can wellbeing from stoic virtue (meaning, community, moral clarity) and epicurean pleasure (sensory satisfaction, positive affect) be measured on a common scale, or are they incommensurable goods?',
    'Empirical wellbeing science: cross-cultural studies comparing life satisfaction in ascetic vs hedonistic populations; longitudinal tracking of individuals transitioning between frameworks; neuroscience of meaning vs pleasure pathways.',
    'If commensurable: the constraint''s extraction can be quantified (stoic framework suppresses superior wellbeing pathways). If incommensurable: the constraint is a preference conflict without objective extraction (tangled rope becomes rope — pure coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pleasure_measurement_incommensurability, empirical, 'Whether pleasure and virtue-wellbeing are commensurable goods').

omega_variable(
    hedonic_treadmill_empirical_scope,
    'Does the hedonic treadmill (habituation to pleasure) apply equally to all pleasures, or only to material/sensory pleasures? Do epicurean frameworks account for the treadmill, or do they naively promise stable wellbeing?',
    'Neuroscience and behavioral economics: identify which pleasure pathways show treadmill effects and which maintain sustained engagement; examine whether sophisticated epicureanism (e.g., Epicurus''s own writings) acknowledges habituation and recommends variety/minimalism.',
    'If treadmill is universal: stoic rejection of pleasure has empirical merit (mountain-like constraint on wellbeing pathways). If treadmill is limited: epicureanism can acknowledge habituation and still offer superior wellbeing through strategic pleasure-seeking; stoic suppression is not empirically justified, revealing the constraint as institutional extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hedonic_treadmill_empirical_scope, empirical, 'Whether hedonic treadmill is universal or limited to certain pleasure types').

omega_variable(
    institutional_gatekeeping_necessity,
    'Is stoic suppression of pleasure necessary to maintain social cohesion and discipline in large-scale institutions, or do pluralist societies demonstrate that coexistence of frameworks enables equal or superior institutional stability?',
    'Institutional comparative analysis: correlation between institutional endorsement of ascetic suppression and social stability/cohesion; cross-cultural studies of pluralist societies with weak ascetic norms.',
    'If suppression is necessary: constraint is rope (legitimate coordination overhead). If pluralist societies show equal stability: suppression is extractive overhead (snare), justified by institutional gatekeeping rather than functional necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_gatekeeping_necessity, empirical, 'Whether stoic suppression is functionally necessary for institutional stability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stoic_vs_epicurean_philosophical_suppression, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stoic_epicu_tr_t0, stoic_vs_epicurean_philosophical_suppression, theater_ratio, 0, 0.35).
narrative_ontology:measurement(stoic_epicu_tr_t10, stoic_vs_epicurean_philosophical_suppression, theater_ratio, 10, 0.42).
narrative_ontology:measurement(stoic_epicu_tr_t20, stoic_vs_epicurean_philosophical_suppression, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(stoic_epicu_be_t0, stoic_vs_epicurean_philosophical_suppression, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(stoic_epicu_be_t10, stoic_vs_epicurean_philosophical_suppression, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(stoic_epicu_be_t20, stoic_vs_epicurean_philosophical_suppression, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stoic_vs_epicurean_philosophical_suppression, identity_coordination).
narrative_ontology:boltzmann_floor_override(stoic_vs_epicurean_philosophical_suppression, 0.12).
narrative_ontology:affects_constraint(stoic_vs_epicurean_philosophical_suppression, monastic_vow_enforcement).
narrative_ontology:affects_constraint(stoic_vs_epicurean_philosophical_suppression, ascetic_institutional_gatekeeping).
narrative_ontology:affects_constraint(stoic_vs_epicurean_philosophical_suppression, hedonic_treadmill_naturalization).

% DUAL FORMULATION NOTE:
% The stoic_vs_epicurean constraint decomposes into three related stories: (1) philosophical_suppression (this story) — the high-level constraint on discourse and wellbeing frameworks; (2) monastic_vow_enforcement (downstream) — institutional implementation of ascetic suppression through ritual and community pressure; (3) hedonic_treadmill_naturalization (downstream) — neuroscientific false naturalization that the hedonic treadmill is universal, justifying suppression as inevitable. Each story has distinct ε values: philosophical suppression (0.58), monastic enforcement (0.72), treadmill naturalization (0.42). All three are linked by institutional and epistemic dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(stoic_vs_epicurean_philosophical_suppression, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
