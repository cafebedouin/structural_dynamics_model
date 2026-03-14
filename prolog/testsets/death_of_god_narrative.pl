% ============================================================================
% CONSTRAINT STORY: death_of_god_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_death_of_god_narrative, []).

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
 *   constraint_id: death_of_god_narrative
 *   human_readable: Death of God Narrative as Epistemological and Social Constraint
 *   domain: philosophy/theology/epistemology/social_cohesion
 *
 * SUMMARY:
 *   The death of God narrative—the claim that modernity has rendered
 *   transcendent metaphysical frameworks intellectually untenable and
 *   epistemically illegitimate—operates as a constraint that both coordinates
 *   secular intellectual life and extracts from those unable or unwilling to
 *   abandon meaning-seeking through metaphysical frameworks. Originating in
 *   19th-century European philosophy (Nietzsche, Feuerbach, Marx) and
 *   institutionalized through 20th-century scientific establishment
 *   dominance, the narrative now functions as a quasi-religious orthodoxy
 *   that suppresses legitimate metaphysical inquiry while claiming to
 *   liberate from dogma. The constraint exhibits characteristics of a Tangled
 *   Rope at the analytical level: genuine coordination function (enables
 *   secular governance, human rights independent of revelation, scientific
 *   progress) coupled with asymmetric extraction (suppression of metaphysical
 *   questions, delegitimization of traditional meaning-structures,
 *   identity-lock for believers unable to adopt materialism). The measurement
 *   trajectory shows theater ratio and extractiveness both rising over the
 *   100-year interval, indicating that the constraint is calcifying from
 *   genuine intellectual framework (early 20th century: philosophy genuinely
 *   engaged with metaphysical alternatives) into institutional performance
 *   (late 20th-century: metaphysical inquiry increasingly forbidden in
 *   academic contexts despite being intellectually unresolved).
 *
 * KEY AGENTS:
 *   - Traditional Religious Communities: Primary victims (powerless/identity_locked) — structurally mobile but identity-fused with theological frameworks; face suppression of meaning-making within secular institutions
 *   - Secular Intellectual Class: Primary beneficiary (institutional/arbitrage) — captures epistemic authority and career advancement through materialist frameworks; enjoys liberation from theological constraints but trapped by own suppression mechanisms
 *   - State and Scientific Establishment: Institutional beneficiary (institutional/arbitrage) — uses death-of-god narrative to legitimize secular authority and expertise without competing transcendent moral claims
 *   - Religious Institutional Apparatus: Secondary actor (institutional/arbitrage) — maintains ritual structures through inertia after cosmological grounding has been intellectually undermined; exhibits high theater ratio
 *   - Intellectual Elites (Secular and Religious): Secondary actors (powerful/constrained) — both profit from death-of-god as generative problem; neither can fully resolve the crisis without ending profitable engagement
 *   - Post-Religious Meaning-Making Movements: Organized agents (organized/constrained) — developing alternative meaning-structures to replace suppressed metaphysical inquiry; perceive sunset clause as open-science alternatives mature
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional suppression as inevitable epistemological law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(death_of_god_narrative, 0.58).
domain_priors:suppression_score(death_of_god_narrative, 0.65).
domain_priors:theater_ratio(death_of_god_narrative, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(death_of_god_narrative, extractiveness, 0.58).
narrative_ontology:constraint_metric(death_of_god_narrative, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(death_of_god_narrative, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(death_of_god_narrative, tangled_rope).
narrative_ontology:human_readable(death_of_god_narrative, "Death of God Narrative as Epistemological and Social Constraint").
narrative_ontology:topic_domain(death_of_god_narrative, "philosophy/theology/epistemology/social_cohesion").

domain_priors:requires_active_enforcement(death_of_god_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(death_of_god_narrative, secular_intellectual_class).
narrative_ontology:constraint_beneficiary(death_of_god_narrative, scientific_materialist_frameworks).
narrative_ontology:constraint_beneficiary(death_of_god_narrative, state_legitimacy_apparatus).
narrative_ontology:constraint_victim(death_of_god_narrative, traditional_religious_communities).
narrative_ontology:constraint_victim(death_of_god_narrative, metaphysical_meaning_structures).
narrative_ontology:constraint_victim(death_of_god_narrative, transcendent_value_frameworks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BELIEVER IN CRISIS (SNARE) — A member of traditional religious community faces suppression of transcendent meaning-making within dominant intellectual frameworks. Structurally mobile (could exit community, could adopt materialism) but identity is fused with theological commitments. The constraint suppresses alternative frameworks while presenting materialist secularism as inevitable. The believer cannot exit without becoming a 'different person' — identity_locked rather than merely trapped. Maximum extraction: faith traditions must either pretend secularism has not won or consciously maintain alternative meaning-systems against massive cultural pressure.
constraint_indexing:constraint_classification(death_of_god_narrative, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: SECULAR INTELLECTUAL TRADITION (TANGLED ROPE) — Benefits from the death-of-god narrative as a liberation framework (from dogma, from authority structures) but also trapped by its own constraints. Must suppress metaphysical questions, must perform materialism even when insufficient to explain aesthetic/meaning domains. Coordination function: unites secular scholars around common epistemic standards. Extraction: those metaphysical questions remain unanswered, and the tradition cannot ask them without undermining its legitimacy. Constrained exit — leaving materialism costs professional standing but is possible for those with alternative institutional anchors.
constraint_indexing:constraint_classification(death_of_god_narrative, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE AND SCIENTIFIC ESTABLISHMENT (ROPE) — Primary beneficiary with arbitrage options. The death-of-god narrative legitimizes secular state authority and scientific expertise without competition from transcendent moral frameworks. Genuine coordination function: enables secular governance, human rights frameworks independent of revelation, scientific progress unburdened by theological constraints. Net beneficiary — extraction runs toward this agent. Can arbitrage between defending secular order and strategic accommodations with religious constituencies.
constraint_indexing:constraint_classification(death_of_god_narrative, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: POST-RELIGIOUS MEANING-MAKING (SCAFFOLD) — Organized agents (secular existentialism, humanist ethics, meaning-from-community movements) see the death-of-god as a temporary constraint being overcome through new meaning-structures. Sunset logic: as secular frameworks develop sufficient depth (Buddhist-materialist synthesis, humanist transcendence, emergent complexity ethics), the suppression of metaphysical inquiry is perceived as temporary. Theater is being replaced with genuine meaning-coordination. Constrained exit because these movements must work within secular frameworks, but they see a path forward.
constraint_indexing:constraint_classification(death_of_god_narrative, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: RELIGIOUS INSTITUTIONAL APPARATUS (PITON) — Traditional religious institutions maintain their authority structures and ritual practices long after their cosmological grounding has been intellectually undermined. Theater ratio is high (0.68+): churches continue ceremonies whose metaphysical basis is publicly denied by the intellectual culture. The apparatus persists through institutional inertia, nostalgia, community habit, and psychological function — not because adherents believe the metaphysical claims anymore. Clergy often exhibit identity_locked + institutional positioning: they are trapped by professional identity and institutional role even as their private epistemology may have shifted. The ritual function persists after meaning has degraded.
constraint_indexing:constraint_classification(death_of_god_narrative, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INTELLECTUAL ELITE (TANGLED ROPE) — Secular philosophers and theologians both profit from the death-of-god narrative as a generative problem (more books, more lectures, more career advancement through debate). Genuine coordination: the narrative disciplines inquiry and produces shared conversation frameworks. Extraction: neither side can fully resolve the metaphysical crisis — doing so would end the profitable intellectual engagement. Both secular and religious intellectuals are constrained by their professional investment in the problem's perpetuation. Constrained exit because abandoning the framing would damage career, but significant agency to shape the terms of the debate.
constraint_indexing:constraint_classification(death_of_god_narrative, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, the death of god might be seen as inevitable structural change: as societies modernize, metaphysical authority naturally erodes because institutional coordination no longer requires transcendent grounding. The constraint is presented as a law of epistemic development. However, this perspective risks false summit — the erosion is contingent on specific institutional arrangements (secular education monopoly, scientific authority capture of meaning-making, state legitimacy transfer), not on timeless laws of epistemology.
constraint_indexing:constraint_classification(death_of_god_narrative, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(death_of_god_narrative_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(death_of_god_narrative, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(death_of_god_narrative, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(death_of_god_narrative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(death_of_god_narrative, TR),
    TR >= 0.70.

:- end_tests(death_of_god_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The narrative genuinely liberates from certain forms of dogmatic authority but suppresses legitimate metaphysical inquiry. The extraction is not total because the suppression is incomplete — metaphysics persists in philosophy departments, theology persists, meaning-seeking continues despite delegitimization. The value reflects that the constraint extracts cognitive space and institutional legitimacy from metaphysical traditions but has not eliminated them. Suppression (0.65): Moderate-high. Multiple mechanisms: career penalties for metaphysical inquiry in science and philosophy, publishing gatekeeping that privileges materialist frameworks, educational curricula that teach materialism as settled truth rather than contested framework, social stigma attached to religious or metaphysical belief among educated classes. But suppression is incomplete — people continue to believe, metaphysical philosophy persists, religious communities maintain parallel institutions. Theater ratio (0.68): High and rising. The measurement trajectory shows theater increasing over the interval, indicating that the narrative is increasingly performative: secular institutions perform materialism even when inadequate to aesthetic, meaning-making, and ethical domains. Religious institutions perform rituals whose metaphysical basis they no longer intellectually defend. Both sides are increasingly engaged in theater rather than genuine epistemological combat.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The believer in crisis experiences Snare (pure extraction, no coordination benefit) while the secular intellectual tradition experiences Tangled Rope (genuine coordination plus extraction). The state experiences Rope (pure coordination with no experienced extraction) while religious institutions experience Piton (performative function degraded by loss of metaphysical grounding). The post-religious movements experience Scaffold (temporary constraint with sunset clause) while the analytical observer risks falsely categorizing the entire phenomenon as Mountain (inevitable consequence of modernization). These gaps reveal the constraint's extractive asymmetry: what the state and secular establishment perceive as progress and liberation, traditional believers perceive as suppression and delegitimization. The piton perspective on religious institutions is particularly diagnostic: their theater ratio (0.68+) indicates that clergy and congregations increasingly participate in rituals whose metaphysical premises they no longer intellectually defend, yet the rituals persist through institutional inertia and psychological function.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by the agent's structural position relative to extraction flows. Believers face d ≈ 0.88 (identity_locked victims): they are structurally mobile (could adopt materialism) but identity-fused with religious frameworks, so they experience suppression as insurmountable. Secular intellectuals face d ≈ 0.42 (beneficiaries with constraints): they gain epistemic authority and career advancement but are trapped by their own suppression of metaphysical inquiry — cannot resolve the fundamental questions their frameworks raise without undermining their legitimacy. State and scientific establishment face d ≈ 0.15 (institutional beneficiaries with arbitrage): the narrative legitimizes their authority without generating competing moral claims from transcendent sources. Religious institutions face d ≈ 0.50 (both beneficiary and victim): they maintain social function and institutional persistence even as their cosmological claims are delegitimized, but they cannot fully exit the constraint without acknowledging the legitimacy of secular authority. The identity_locked exit option for believers distinguishes this constraint from simple material suppression: believers could adopt materialism (exit is structurally possible) but their identity is so fused with theological commitments that exit would constitute a different self.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing that the death-of-god narrative performs dual structural roles depending on observer position: genuine coordination (for secular institutions building authority structures independent of revelation) and genuine extraction (for traditional communities losing intellectual legitimacy). The narrative is not 'truly' one or the other — the six-fold classification accurately captures that different agents have genuinely incompatible structural experiences of the same constraint. The mandatrophy resolution identifies the false summit: the analytical 'natural law' view that death-of-god is inevitable consequence of modernization. This naturalizes what is actually a contingent institutional arrangement where secular institutions captured epistemic authority and suppressed legitimate alternatives. The constraint would not exist (or would be radically different) in societies that modernized without full secularization of intellectual authority — Japan, post-socialist states with residual Orthodox belief, pluralist democracies that permit metaphysical inquiry in academic contexts. The false summit exposes how the constraint maintains itself: by claiming to be inevitable rather than contingent, it preempts the possibility of institutional reform that would restore metaphysical pluralism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_necessity_vs_contingency,
    'Is the death of god a necessary consequence of modernization and scientific knowledge, or a contingent institutional arrangement that suppresses legitimate metaphysical inquiry?',
    'Comparative historical analysis: societies that modernized without full suppression of metaphysical frameworks (Japan, some pluralist democracies); logical analysis of whether scientific materialism actually falsifies all metaphysical claims or merely sidesteps them',
    'If necessary: the narrative is mountain-like (unchangeable structural fact). If contingent: it is a Snare or Tangled Rope (artificially maintained suppression). The interpretation determines whether reform is futile or liberation is possible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metaphysical_necessity_vs_contingency, conceptual, 'Whether death of god is inevitable or contingent institutional outcome').

omega_variable(
    meaning_substitutability,
    'Can secular frameworks (humanism, existentialism, meaning-from-community) truly replace transcendent meaning-structures, or do they perform the same psychological function through disguised theism?',
    'Psychological and anthropological analysis of whether secular meaning-systems exhibit the same belief-structure and comfort patterns as religious ones; analysis of whether they suppress metaphysical questioning in ways analogous to religious dogmatism',
    'If substitutable: the scaffold perspective is correct and the sunset is real. If non-substitutable: believers face permanent extraction (Snare remains Snare), and the secular tradition is self-deceived about the adequacy of its frameworks (identity_locked at institutional level).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meaning_substitutability, empirical, 'Whether secular meaning-systems adequately substitute for transcendent ones').

omega_variable(
    suppression_internalization_mechanism,
    'Is the suppression of metaphysical inquiry enforced structurally (career risk, publishing gate-keeping, institutional pressure) or internalized (believers themselves adopt materialism as epistemic norm)?',
    'Analysis of whether practitioners maintain metaphysical questions privately while suppressing them publicly; comparison of intellectual vigor in metaphysical philosophy before and after institutional materialist capture; examination of epistemic diversity in peer review',
    'If structural: suppression declines if institutions change. If internalized: the constraint persists through cognitive capture even after structural barriers are removed. Identity_locked becomes the dominant exit mode rather than trapped or constrained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    intellectual_stagnation_risk,
    'Has the suppression of metaphysical inquiry under the death-of-god narrative actually produced intellectual progress, or has it created a piton-like stagnation where certain questions are forbidden rather than solved?',
    'Comparison of philosophical innovation rates in metaphysical domains pre- and post-death-of-god; analysis of whether modern materialism has generated genuinely new insights or merely repeated negations; examination of conceptual progress in meaning-making versus suppression-of-meaning',
    'If progress: the narrative is justified. If stagnation: the constraint is primarily extractive (Snare) disguised as progress, and the piton perspective is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intellectual_stagnation_risk, empirical, 'Whether death-of-god narrative has advanced or stalled metaphysical inquiry').

omega_variable(
    belief_persistence_paradox,
    'Why do belief systems and meaning-seeking persist so powerfully across all secular societies despite the death-of-god narrative''s intellectual dominance?',
    'Psychological and sociological analysis of belief persistence; analysis of whether modern spirituality, scientism, political ideology, and consumer culture are functional replacements for religion or genuine alternatives; examination of whether the constraint is weakening or merely disguising itself',
    'If persistence reflects inadequacy of secular frameworks: the scaffold perspective is aspirational, not structural; belief communities are legitimately trapped (Snare) rather than identity_locked. If persistence reflects psychological universal: the constraint is not truly suppressive but merely shifts the form of meaning-making.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(belief_persistence_paradox, empirical, 'Why belief systems persist despite death-of-god narrative dominance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(death_of_god_narrative, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dog_tr_t0, death_of_god_narrative, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dog_tr_t25, death_of_god_narrative, theater_ratio, 25, 0.52).
narrative_ontology:measurement(dog_tr_t50, death_of_god_narrative, theater_ratio, 50, 0.68).
narrative_ontology:measurement(dog_tr_t75, death_of_god_narrative, theater_ratio, 75, 0.72).

% Extraction over time
narrative_ontology:measurement(dog_be_t0, death_of_god_narrative, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(dog_be_t25, death_of_god_narrative, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(dog_be_t50, death_of_god_narrative, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(dog_be_t75, death_of_god_narrative, base_extractiveness, 75, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(death_of_god_narrative, identity_coordination).
narrative_ontology:affects_constraint(death_of_god_narrative, secularization_paradox).
narrative_ontology:affects_constraint(death_of_god_narrative, meaning_crisis_institutional_responses).
narrative_ontology:affects_constraint(death_of_god_narrative, scientific_materialism_hegemony).

% DUAL FORMULATION NOTE:
% The death-of-god narrative decomposes into multiple structurally distinct constraints. The metaphysical claim (transcendence is false) has different extractiveness than the institutional claim (metaphysical inquiry is illegitimate). The sociological claim (belief systems persist despite intellectualization) has different extractiveness than the epistemological claim (materialism is sufficient framework). Each decomposition reveals different omega variables and perspectival gaps. This story captures the institutional enforcement constraint that makes the metaphysical claim binding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(death_of_god_narrative, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
