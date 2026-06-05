% ============================================================================
% CONSTRAINT STORY: misunderstanding_as_mismatch
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_misunderstanding_as_mismatch, []).

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
 *   constraint_id: misunderstanding_as_mismatch
 *   human_readable: Social Pressure for Worldview Assimilation
 *   domain: social/psychological
 *
 * SUMMARY:
 *   Social pressure for worldview assimilation is the structural constraint
 *   that generates misunderstanding as a default outcome in cognitively
 *   heterogeneous groups. When individuals hold incompatible interpretive
 *   frameworks and face costs for expressing them, they develop alternative
 *   public and private vocabularies. This code-switching creates systematic
 *   misalignment between expressed and held beliefs, making genuine
 *   communication impossible. The constraint is not the existence of diverse
 *   worldviews — diversity per se is not extractive. Rather, the constraint
 *   is the institutional enforcement mechanism that penalizes deviation from
 *   dominant cognitive frameworks through social sanction, professional
 *   exclusion, reputational damage, and economic pressure. Over the
 *   measurement interval (100-year timescale), assimilation pressure has
 *   intensified as institutional reach has expanded (corporate monoculture,
 *   algorithmic amplification of consensus, globalization of Western
 *   epistemic standards) while performative inclusion rituals have
 *   proliferated to mask the underlying extraction. This creates the
 *   theatrical gap: institutions claim to welcome diverse worldviews while
 *   maintaining severe penalties for actual cognitive deviance.
 *
 * KEY AGENTS:
 *   - Dissenting Individual: Primary victim (powerless/trapped) — bears full cost of maintaining cognitive integrity; cannot exit without severe social harm
 *   - Cautious Minority: Secondary victim (moderate/constrained) — experience mixed coordination and extraction; can build alternative communities but at resource/status cost
 *   - Dominant Coalition: Primary beneficiary (institutional/arbitrage) — benefits from worldview alignment; maintains extraction mechanism through institutional channels
 *   - Counter-Culture Organization: Secondary beneficiary and victim (moderate/mobile) — benefit from coordination within alternative frameworks but face extraction from dominant society
 *   - Performative Inclusion Ritual: Institutional actor (institutional/arbitrage) — perpetuates illusion of cognitive freedom while maintaining assimilation pressure; piton from self-perception
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — risks naturalizing contingent assimilation as inherent human group dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(misunderstanding_as_mismatch, 0.52).
domain_priors:suppression_score(misunderstanding_as_mismatch, 0.68).
domain_priors:theater_ratio(misunderstanding_as_mismatch, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(misunderstanding_as_mismatch, extractiveness, 0.52).
narrative_ontology:constraint_metric(misunderstanding_as_mismatch, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(misunderstanding_as_mismatch, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(misunderstanding_as_mismatch, tangled_rope).
narrative_ontology:human_readable(misunderstanding_as_mismatch, "Social Pressure for Worldview Assimilation").
narrative_ontology:topic_domain(misunderstanding_as_mismatch, "social/psychological").

domain_priors:requires_active_enforcement(misunderstanding_as_mismatch).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(misunderstanding_as_mismatch, dominant_worldview_coalition).
narrative_ontology:constraint_beneficiary(misunderstanding_as_mismatch, social_coherence_maintainers).
narrative_ontology:constraint_victim(misunderstanding_as_mismatch, alternative_viewpoint_holders).
narrative_ontology:constraint_victim(misunderstanding_as_mismatch, cognitive_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISSENTING INDIVIDUAL (SNARE) — Trapped within immediate social network (family, workplace, community). Cannot exit without severe social cost (isolation, reputation damage, economic harm). Bears full extraction: cognitive suppression, emotional labor of code-switching, systematic dismissal of alternative views. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.57.
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CAUTIOUS MINORITY (TANGLED ROPE) — Constrained by social fear but mobile enough to seek alternative communities (religious minorities, political dissidents, cultural immigrants). Experiences both extraction (pressure to conform in dominant spaces) and coordination benefit (community verification through consensus checking, mutual support for maintaining alternative worldview). d≈0.65, f(d)≈0.95, σ=0.9 → χ≈0.49.
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DOMINANT COALITION (ROPE) — Institutional actors (media, academia, political parties, corporations) that benefit from worldview alignment. Experience the constraint as pure coordination: assimilation mechanisms maintain epistemic coherence and operational efficiency. Arbitrage exit: can freely express dominant view without cost. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.04. Net beneficiary; negative extraction indicates subsidy.
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COUNTER-CULTURE ORGANIZATION (TANGLED ROPE) — Organized groups explicitly rejecting assimilation pressure (social movements, alternative communities, intellectual subcultures). Mobile enough to build parallel institutions but still subject to extraction from dominant society (material deprivation, institutional barriers, delegitimization). Also provide coordination within their own epistemic framework. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.34.
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: PERFORMATIVE INCLUSION RITUAL (PITON) — Institutional diversity/inclusion programs that claim to celebrate worldview diversity while maintaining assimilatory pressure. Theater ratio (0.55) reflects that inclusion rhetoric persists despite minimal structural change: 'welcoming alternative views' remains performative as long as social costs for dissent remain high and dominant frameworks are unexamined. The mechanism has degraded from genuine pluralism (if it ever existed) to theatrical tolerance.
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, social pressure toward cognitive alignment may appear inherent to human group cognition: all coherent communities require some shared framework, and deviation imposes coordination costs. This perspective risks treating contingent institutional assimilation as an immutable feature of social existence. However, ε=0.52, suppression=0.68, theater=0.55 contradict mountain gates; this is a false summit revealing that extractive assimilation is institutionally contingent, not natural.
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(misunderstanding_as_mismatch_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(misunderstanding_as_mismatch, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(misunderstanding_as_mismatch, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(misunderstanding_as_mismatch, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(misunderstanding_as_mismatch, TR),
    TR >= 0.70.

:- end_tests(misunderstanding_as_mismatch_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts significant cognitive labor from dissenters (self-monitoring, code-switching, emotional regulation) and blocks alternative knowledge production (dissidents cannot build institutions without strategic compromise). However, extraction is not total — alternative communities persist and some institutional spaces permit bounded deviance. The trajectory shows intensification from 0.38 to 0.52 as institutional reach expands and consensus-enforcement mechanisms (media, algorithms, professional gatekeeping) strengthen. Suppression (0.68): High. Multiple structural barriers prevent expression of alternative worldviews: economic dependence on dominant institutions, social isolation costs, professional exclusion, legal/political persecution in extreme cases. But suppression is not absolute — underground networks, diaspora communities, and historical periods of openness show variation. Theater ratio (0.55): Moderate. Performative inclusion practices (diversity statements, welcoming language, ritual acknowledgment of alternative views) mask the underlying assimilation mechanism. The ratio reflects genuine contradiction: some institutions have made real progress toward cognitive pluralism, while others use inclusion rhetoric to legitimize unexamined dominance. The rising trajectory (0.38 → 0.55) tracks the proliferation of 'woke' corporate inclusion alongside hardened epistemic gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   The dissenting individual sees a snare: they are trapped and cannot maintain cognitive integrity without social cost. The cautious minority sees tangled rope: they can build alternative communities but remain subordinated in broader society. The dominant coalition sees rope: maintaining shared worldview is coordination that enables efficient collaboration. The counter-culture organization sees tangled rope: they provide coordination within their epistemic community but extract from members (demand for ideological loyalty) while being extracted from by dominant society. The performative inclusion ritual sees itself as degraded (piton): the institution acknowledges that its diversity claims don't match structural reality. The civilizational observer risks seeing a mountain: human groups always need epistemic coherence, therefore assimilation is natural. The engine will flag this as a false summit because the structural data (measurable suppression, identifiable beneficiaries/victims, rising theater) shows contingent institutional enforcement, not immutable law.
 *
 * DIRECTIONALITY LOGIC:
 *   Dissenting individual: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; no exit option. Cautious minority: Victim + constrained → d≈0.65, f(d)≈0.95. Significant extraction with partial exit. Counter-culture organization: Mixed (provides internal coordination, faces external extraction) + mobile → d≈0.50, f(d)≈0.65. Moderate extraction externally, coordination internally. Dominant coalition: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary; can freely operate. Performative inclusion: Institutional + arbitrage → d≈0.08, f(d)≈-0.08. Piton classification emerges from theater (0.55), not from high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates how indexical classification prevents conflating coordination with extraction. A naive analysis might treat assimilation pressure as 'natural social cohesion' (mountain) or 'coordination mechanism' (rope). But the indexed perspectives reveal the extraction: from the victim's perspective it's snare (trapped, suppressed, extracted from); from the beneficiary's perspective it's rope (coordination feels costless when you set the frame); from the organized counter-cultural perspective it's tangled rope (mixed benefits and costs); from the institutional self-perception it's piton (degraded, performative). The mandatrophy is resolved by recognizing that these are not competing truths about which type is 'really' correct. Instead, the presheaf of perspectives reveals that the constraint simultaneously provides coordination for the dominant coalition AND extracts from those whose worldviews deviate. It is both rope and snare, depending on which side of the assimilation boundary you occupy. The analytical mountain perspective is a false summit because it naturalizes what the structural data reveals as an institutional mechanism with identifiable beneficiaries (dominant coalition), identifiable victims (dissenters), measurable suppression (0.68), and rising theater (0.55 trajectory). This is contingent institutional extraction, not immutable law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distinction_between_coordination_and_extraction,
    'How much of assimilation pressure is necessary for legitimate group coordination versus unnecessary extraction of cognitive conformity?',
    'Comparative analysis: societies/groups with high coordination benefits and low assimilation cost vs. those with high cost and low coordination benefits. Measurement of epistemic function with and without conformity pressure.',
    'If most coordination requires minimal conformity: assimilation is largely extractive (snare dominant). If coordination requires strong alignment: assimilation is legitimate overhead (rope dominant, piton as degradation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distinction_between_coordination_and_extraction, conceptual, 'Whether assimilation is necessary for coordination or unnecessary extraction').

omega_variable(
    exit_option_availability_variance,
    'What fraction of populations in a given institutional context actually have mobile or arbitrage exit options versus trapped or constrained options?',
    'Demographic analysis: migration capacity, alternative community availability, economic independence by socioeconomic status, geographic location. Measurement of actual exit costs versus perceived exit costs.',
    'If most are trapped (>70%): constraint is primarily snare (extraction from majority). If most are constrained (50-70%): tangled rope dominant. If most are mobile/arbitrage (>50%): rope or scaffold classifications increase.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_availability_variance, empirical, 'Distribution of exit options across population').

omega_variable(
    alternative_worldview_sustainability,
    'Can alternative worldviews sustain themselves without constant defense against assimilation pressure, or does assimilation resistance itself become extractive (blocking internal evolution)?',
    'Historical case studies: long-term stability of counter-cultural communities, measurement of ideological flexibility within alternatives vs. rigidity driven by external pressure, analysis of internal diversity in communities under siege.',
    'If alternatives require constant defensive extraction: counter-pressure becomes a secondary snare (victims are members of alternative groups). If alternatives can evolve sustainably: scaffold sunset is real (assimilation pressure is temporary institutional imbalance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_worldview_sustainability, empirical, 'Whether alternative worldviews require perpetual assimilation resistance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(misunderstanding_as_mismatch, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mism_tr_t0, misunderstanding_as_mismatch, theater_ratio, 0, 0.38).
narrative_ontology:measurement(mism_tr_t50, misunderstanding_as_mismatch, theater_ratio, 50, 0.48).
narrative_ontology:measurement(mism_tr_t100, misunderstanding_as_mismatch, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(mism_be_t0, misunderstanding_as_mismatch, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(mism_be_t50, misunderstanding_as_mismatch, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(mism_be_t100, misunderstanding_as_mismatch, base_extractiveness, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(misunderstanding_as_mismatch, information_standard).
narrative_ontology:affects_constraint(misunderstanding_as_mismatch, echo_chamber_reinforcement).
narrative_ontology:affects_constraint(misunderstanding_as_mismatch, epistemic_gatekeeping_by_consensus).
narrative_ontology:affects_constraint(misunderstanding_as_mismatch, cognitive_liberty_suppression).

% DUAL FORMULATION NOTE:
% Worldview assimilation pressure is downstream of broader institutional coordination mechanisms but represents a distinct structural constraint focusing on cognitive conformity extraction. Related constraints (echo chambers, epistemic gatekeeping, cognitive suppression) share the same institutional beneficiaries and victims but operate through different mechanisms and have different ε values reflecting their structural specificity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(misunderstanding_as_mismatch, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
