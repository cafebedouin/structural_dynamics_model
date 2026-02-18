% ============================================================================
% CONSTRAINT STORY: linguistic_relativity_cultural_framing
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-11
% ============================================================================

:- module(constraint_linguistic_relativity_cultural_framing, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: linguistic_relativity_cultural_framing
 *   human_readable: Sapir-Whorf Cultural Application to English-Chinese Differences
 *   domain: social/linguistic/political
 *
 * SUMMARY:
 *   The Sapir-Whorf hypothesis — that language structure influences or
 *   determines habitual thought — provides a genuine coordination framework
 *   for cross-linguistic cognitive research. However, its application to
 *   English-Chinese comparisons has historically carried asymmetric cultural
 *   extraction. From Whorf's original framing through mid-20th-century
 *   structural linguistics and into contemporary pop-science narratives,
 *   the framework has repeatedly been deployed to position English-language
 *   cognitive patterns as the unmarked norm and Chinese-language patterns as
 *   the marked deviation — "Chinese lacks tense" rather than "English
 *   requires redundant temporal marking." This framing asymmetry channels
 *   research funding, shapes language pedagogy, influences business
 *   communication norms, and sustains a hierarchy in which analytic-
 *   inflectional languages are implicitly treated as cognitively superior
 *   to isolating-pragmatic ones.
 *
 *   The genuine coordination function is real: linguistic relativity research
 *   has produced valuable insights into color perception (Winawer et al. 2007),
 *   temporal reasoning (Boroditsky 2001, Chen 2013), spatial cognition
 *   (Levinson 2003), and counterfactual reasoning (Au 1983, Bloom 1981).
 *   The extraction is also real: these findings are selectively cited in
 *   corporate cross-cultural training, English-language-teaching policy,
 *   and popular media to reinforce linguistic hierarchies.
 *
 * KEY AGENTS (by structural relationship):
 *   - Heritage Chinese speakers in anglophone contexts:
 *       Primary target (powerless/trapped) — bears extraction through
 *       deficit framing of their native language's cognitive patterns
 *   - Anglophone research institutions and ELT industry:
 *       Primary beneficiary (institutional/arbitrage) — controls research
 *       framing, publication norms, and language-teaching markets
 *   - Chinese-medium research institutions:
 *       Secondary institutional actor (institutional/constrained) — must
 *       publish in English-language journals to gain recognition, accepting
 *       the framing conventions of anglophone linguistics
 *   - Comparative cognitive scientists:
 *       Analytical observer — sees both the coordination value and the
 *       extraction embedded in the framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(linguistic_relativity_cultural_framing, 0.42).
domain_priors:suppression_score(linguistic_relativity_cultural_framing, 0.55).
domain_priors:theater_ratio(linguistic_relativity_cultural_framing, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(linguistic_relativity_cultural_framing, extractiveness, 0.42).
narrative_ontology:constraint_metric(linguistic_relativity_cultural_framing, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(linguistic_relativity_cultural_framing, theater_ratio, 0.35).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(linguistic_relativity_cultural_framing, tangled_rope).
narrative_ontology:human_readable(linguistic_relativity_cultural_framing, "Sapir-Whorf Cultural Application to English-Chinese Differences").
narrative_ontology:topic_domain(linguistic_relativity_cultural_framing, "social/linguistic/political").

% --- Binary flags ---
domain_priors:requires_active_enforcement(linguistic_relativity_cultural_framing).

% --- Structural relationships ---
% Who benefits from the Sapir-Whorf framing persisting as-is?
narrative_ontology:constraint_beneficiary(linguistic_relativity_cultural_framing, anglophone_research_institutions).
narrative_ontology:constraint_beneficiary(linguistic_relativity_cultural_framing, english_language_teaching_industry).

% Who bears disproportionate cost from the deficit framing?
narrative_ontology:constraint_victim(linguistic_relativity_cultural_framing, heritage_chinese_speakers_in_anglophone_contexts).
narrative_ontology:constraint_victim(linguistic_relativity_cultural_framing, chinese_medium_research_institutions).

% Gate requirements for Tangled Rope:
%   beneficiary (above) + victim (above) + requires_active_enforcement (above)
%   All three present.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context/4 terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: HERITAGE CHINESE SPEAKER IN ANGLOPHONE CONTEXT (PRIMARY TARGET)
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95
%   f(d) ≈ 1.42, σ(national) = 1.0
%   χ ≈ 0.42 × 1.42 × 1.0 ≈ 0.60
% At ε = 0.42 and suppression = 0.55, this meets tangled_rope thresholds
% (ε ≥ 0.30, supp ≥ 0.40, 0.40 ≤ χ ≤ 0.90) but not snare
% (needs ε ≥ 0.46 and supp ≥ 0.60).
%
% These speakers experience the constraint most sharply: their native
% language is framed as "lacking" features English "has," positioning
% their cognitive patterns as deficient. They cannot exit this framing
% while operating in anglophone educational and professional settings.
constraint_indexing:constraint_classification(linguistic_relativity_cultural_framing, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ANGLOPHONE RESEARCH INSTITUTIONS (PRIMARY BENEFICIARY)
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05
%   f(d) ≈ -0.12, σ(national) = 1.0
%   χ ≈ 0.42 × (-0.12) × 1.0 ≈ -0.05
% Negative χ → classified as rope (χ ≤ 0.35).
%
% From this perspective, linguistic relativity is a productive research
% paradigm — it structures inquiry, generates testable hypotheses, and
% coordinates cross-disciplinary collaboration. The framing asymmetry
% is invisible: the institutional perspective sees the constraint as
% pure coordination because the extraction flows outward.
constraint_indexing:constraint_classification(linguistic_relativity_cultural_framing, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: COMPARATIVE COGNITIVE SCIENTIST (ANALYTICAL OBSERVER)
% Engine derives d from: observer → d ≈ 0.72
%   f(d) ≈ 1.15, σ(global) = 1.2
%   χ ≈ 0.42 × 1.15 × 1.2 ≈ 0.58
% Tangled rope: sees both the genuine coordination value AND the
% asymmetric cultural extraction embedded in the framing conventions.
constraint_indexing:constraint_classification(linguistic_relativity_cultural_framing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: CHINESE-MEDIUM RESEARCH INSTITUTIONS (SECONDARY INSTITUTIONAL)
% Engine derives d from: victim membership + constrained exit → d ≈ 0.55
%   f(d) ≈ 0.75, σ(global) = 1.2
%   χ ≈ 0.42 × 0.75 × 1.2 ≈ 0.38
% Near the rope/tangled_rope boundary. These institutions experience
% moderate extraction — they must adopt anglophone framing to publish
% internationally, but have enough institutional power to partially
% resist (publishing alternative framings in Chinese-language journals,
% developing Chinese-medium cognitive science traditions).
constraint_indexing:constraint_classification(linguistic_relativity_cultural_framing, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(linguistic_relativity_cultural_framing_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(
        linguistic_relativity_cultural_framing, TypeTarget,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(
        linguistic_relativity_cultural_framing, TypeBeneficiary,
        context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    narrative_ontology:constraint_metric(
        linguistic_relativity_cultural_framing, extractiveness, E),
    E >= 0.30,
    E =< 0.90.

test(tangled_rope_gate) :-
    % Verify all three tangled rope structural requirements are present.
    narrative_ontology:constraint_beneficiary(
        linguistic_relativity_cultural_framing, _),
    narrative_ontology:constraint_victim(
        linguistic_relativity_cultural_framing, _),
    domain_priors:requires_active_enforcement(
        linguistic_relativity_cultural_framing).

:- end_tests(linguistic_relativity_cultural_framing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness 0.42: The Sapir-Whorf framework extracts meaningful
 *   value when applied to English-Chinese comparisons. The extraction is not
 *   as raw as a debt trap (snare), but it is structurally asymmetric. English-
 *   language framing of the research defines "normal" cognition as whatever
 *   English grammar makes obligatory. Chinese grammatical features are
 *   described via absence ("lacks tense," "has no articles") rather than
 *   presence ("uses aspect markers," "employs topic-comment structure").
 *   This deficit framing shapes hiring assessments, educational placement,
 *   corporate communication norms, and research funding priorities.
 *
 *   Suppression 0.55: Moderate-high. Heritage Chinese speakers in anglophone
 *   contexts cannot easily opt out of the deficit framing. It is embedded in
 *   ESL pedagogy ("correct your tense errors"), standardized testing (TOEFL
 *   penalizes tense omission), workplace communication norms ("be more
 *   direct/explicit"), and popular culture ("Chinese is ambiguous"). The
 *   suppression is structural rather than intentional — no single actor
 *   designs it — but it is actively maintained through institutional
 *   practices.
 *
 *   Theater ratio 0.35: Relatively low. Most of the constraint's operation
 *   is functional, not performative. The research paradigm genuinely produces
 *   knowledge. The extraction is a side effect of framing conventions, not
 *   theatrical maintenance of a hollow institution.
 *
 * PERSPECTIVAL GAP:
 *   The primary target (heritage Chinese speakers, powerless/trapped) sees
 *   tangled_rope: they experience real extraction through deficit framing
 *   but also benefit from the research coordination (bilingual cognition
 *   studies have produced insights that help bilingual education programs).
 *   The primary beneficiary (anglophone institutions, institutional/arbitrage)
 *   sees rope: from their perspective, the framework is a productive
 *   research coordination mechanism, and the asymmetric framing is invisible
 *   because it aligns with their institutional defaults.
 *
 *   This gap — tangled_rope vs rope — is the signature of a constraint
 *   whose extraction is embedded in framing rather than overt coercion.
 *   The beneficiary does not see the extraction because the extraction IS
 *   the frame through which they see.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: Anglophone research institutions control publication
 *   norms, citation networks, and the default framing of cross-linguistic
 *   comparison. The ELT industry profits directly from deficit framing
 *   (every "tense error" is a market opportunity for English instruction).
 *   Both have arbitrage exit — they can adopt or discard the Sapir-Whorf
 *   framework without institutional cost.
 *
 *   Victims: Heritage Chinese speakers in anglophone contexts are trapped
 *   in the deficit framing — their professional credibility depends on
 *   conforming to anglophone communication norms that treat their native
 *   cognitive patterns as deficient. Chinese-medium institutions are
 *   constrained — they have partial exit (domestic publication) but lose
 *   international visibility.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   Anglophone institutions (arbitrage exit) vs Chinese-medium institutions
 *   (constrained exit) illustrates how the same institutional power level
 *   can produce different directionalities. Both are "institutional," but
 *   the anglophone institution sets the framing norms while the Chinese-
 *   medium institution must either adopt those norms or accept reduced
 *   visibility. The exit asymmetry (arbitrage vs constrained) is what the
 *   engine uses to differentiate their d values.
 *
 * CULTURAL-COGNITIVE DEPTH:
 *   The cognitive effects documented in the upstream Mountain story
 *   (english_chinese_tense_structure) are real but neutral — different
 *   languages channel attention differently, not better or worse. This
 *   Tangled Rope story captures what happens when that neutral structural
 *   fact gets embedded in a cultural hierarchy. The same finding — "English
 *   speakers attend to tense more than Chinese speakers" — becomes either
 *   "English enables more precise temporal reasoning" (extraction framing)
 *   or "Chinese enables more flexible temporal integration" (coordination
 *   framing) depending on who controls the narrative.
 *
 *   At the cultural level, the framing constraint shapes how each
 *   civilization's intellectual traditions are perceived internationally.
 *   Chinese philosophical traditions that emphasize process, context, and
 *   relational thinking (correlative cosmology, Daoist process metaphysics)
 *   are sometimes dismissed as "pre-scientific" when evaluated through
 *   the lens of English-language analytic philosophy. The linguistic
 *   relativity framework, when deployed asymmetrically, provides pseudo-
 *   scientific cover for this cultural ranking.
 *
 * MANDATROPHY ANALYSIS:
 *   Tangled rope classification prevents two errors:
 *   (1) Calling it pure rope would erase the asymmetric extraction that
 *       heritage Chinese speakers actually experience — the deficit framing
 *       has real consequences for education, employment, and cultural status.
 *   (2) Calling it pure snare would erase the genuine coordination value —
 *       linguistic relativity research has produced real and valuable
 *       scientific insights. The framework is not only an extraction device;
 *       it also structures productive inquiry. The problem is in the framing
 *       conventions, not the research program itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_framing_reversibility,
    'Can the deficit framing of Chinese linguistic features in anglophone research be reversed within the existing institutional structure of cross-linguistic cognitive science, or does reversal require a structural shift in which language community controls publication norms?',
    'Track citation patterns, conference keynote demographics, journal editorial board composition, and framing conventions in top-20 cognitive science journals over a 15-year window. Compare with parallel metrics in Chinese-language cognitive science publication.',
    'If reversible within existing structure: suppression may decline toward 0.30, potentially reclassifying from tangled_rope to rope at the analytical level. If structural shift required: the constraint may intensify before resolving, with a period of higher extraction as established framings resist challenge.',
    confidence_without_resolution(low)
).

omega_variable(
    omega_cognitive_depth_vs_framing,
    'To what extent do documented cognitive differences between English and Chinese speakers reflect genuine structural channeling (the upstream Mountain) versus experimental designs that embed anglophone cognitive norms as the measurement standard?',
    'Meta-analysis of cross-linguistic cognition experiments, coding each study for whether the experimental paradigm treats English-pattern responses as the baseline or whether it uses language-neutral baseline tasks. Separate effect sizes for paradigm-neutral vs paradigm-biased studies.',
    'If mostly genuine channeling: ε may increase slightly (the extraction is built on real cognitive differences, making it harder to challenge). If mostly paradigm bias: ε may decrease (the extraction is built on methodological artifacts, making it fragile once exposed). Either way, the tangled_rope classification holds — the question is about the magnitude, not the structure.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(linguistic_relativity_cultural_framing, 0, 10).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: this is an information standard — the Sapir-Whorf
% framework standardizes how cross-linguistic cognitive differences are
% described, measured, and published.
narrative_ontology:coordination_type(linguistic_relativity_cultural_framing, information_standard).

% Network relationships — this tangled rope is downstream of the mountain.
% The structural linguistic difference (upstream) provides the empirical
% foundation that the cultural framing (this story) builds upon and
% selectively interprets.

% DUAL FORMULATION NOTE:
% This constraint is one of 2 stories decomposed from "English-Chinese
% linguistic structure and its cognitive-cultural effects."
% Decomposed because ε differs across observables (ε-invariance principle).
% The structural fact (sibling story, ε=0.05) is Mountain — nobody extracts
% from grammatical typology. The framing of that fact for cultural
% conclusions (this story, ε=0.42) is Tangled Rope — genuine research
% coordination mixed with asymmetric cultural extraction.
% Related stories:
%   - english_chinese_tense_structure (ε=0.05, Mountain)

narrative_ontology:affects_constraint(linguistic_relativity_cultural_framing, english_chinese_tense_structure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
