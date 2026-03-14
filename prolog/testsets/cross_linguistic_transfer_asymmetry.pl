% ============================================================================
% CONSTRAINT STORY: cross_linguistic_transfer_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cross_linguistic_transfer_asymmetry, []).

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
 *   constraint_id: cross_linguistic_transfer_asymmetry
 *   human_readable: Cross-Linguistic Transfer Asymmetry in Language Learning and Linguistic Science
 *   domain: linguistics/cognitive_science/education
 *
 * SUMMARY:
 *   Cross-linguistic transfer asymmetry describes the structural phenomenon
 *   whereby English-speaking learners of other languages experience primarily
 *   positive transfer (English structures facilitate acquisition of
 *   typologically similar languages), while speakers of non-English languages
 *   experience significant negative transfer when acquiring English
 *   (interference from L1 structures that differ from English patterns).
 *   Simultaneously, linguistic science itself treats English structures as
 *   the baseline against which other languages are compared, leading to
 *   theoretical frameworks that classify non-English phenomena as marked
 *   exceptions rather than equally valid solutions. The constraint combines
 *   genuine cognitive mechanisms (transfer difficulty is real) with
 *   institutional asymmetry (the treatment of English as normative is
 *   constructed). This manifests as a Tangled Rope: real coordination
 *   function (comparative linguistics requires some baseline; language
 *   pedagogy requires explicit contrast structures) co-exists with asymmetric
 *   extraction (English learners and English-medium institutions benefit
 *   disproportionately from the arrangement; non-English languages are
 *   systematically marginalized in theory and practice). The theater_ratio
 *   (0.68) reflects that pedagogical explanations of English-centric
 *   instruction often invoke scientific necessity while masking power
 *   asymmetry—institutional dominance is performed as neutral methodology.
 *
 * KEY AGENTS:
 *   - Non-English Speaker Learners: Primary victims (powerless/trapped) — experience negative transfer costs and English-biased pedagogy; cannot exit due to global English dominance
 *   - Speakers of Non-English Languages: Victim category (powerless/trapped) — linguistic structures systematically classified as marked/exceptional in English-based theory
 *   - English-Speaking Learners: Primary beneficiaries (institutional/arbitrage) — experience positive transfer and teaching optimized for their linguistic profile
 *   - English-Medium Educational Institutions: Beneficiary institutions (institutional/arbitrage) — benefit from aligned expectations and reduced pedagogical complexity
 *   - Linguistics Research Community: Organized enforcer (organized/constrained) — maintains English-centric theoretical frameworks; benefits from coherent baseline while constrained by path dependence
 *   - Generative Linguistics Establishment: Framework maintainer (institutional/arbitrage) — perpetuates UG and English-based theory through disciplinary institutions; sees gradual empirical challenges (Piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional arrangement as cognitive necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cross_linguistic_transfer_asymmetry, 0.52).
domain_priors:suppression_score(cross_linguistic_transfer_asymmetry, 0.58).
domain_priors:theater_ratio(cross_linguistic_transfer_asymmetry, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cross_linguistic_transfer_asymmetry, extractiveness, 0.52).
narrative_ontology:constraint_metric(cross_linguistic_transfer_asymmetry, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(cross_linguistic_transfer_asymmetry, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cross_linguistic_transfer_asymmetry, tangled_rope).
narrative_ontology:human_readable(cross_linguistic_transfer_asymmetry, "Cross-Linguistic Transfer Asymmetry in Language Learning and Linguistic Science").
narrative_ontology:topic_domain(cross_linguistic_transfer_asymmetry, "linguistics/cognitive_science/education").

domain_priors:requires_active_enforcement(cross_linguistic_transfer_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cross_linguistic_transfer_asymmetry, english_dominant_learners).
narrative_ontology:constraint_beneficiary(cross_linguistic_transfer_asymmetry, linguists_studying_english_structures).
narrative_ontology:constraint_victim(cross_linguistic_transfer_asymmetry, speakers_of_non_english_languages).
narrative_ontology:constraint_victim(cross_linguistic_transfer_asymmetry, linguistic_theory_generalizability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-ENGLISH SPEAKER LEARNER (SNARE) — Structurally trapped in asymmetric transfer conditions. Lacks exit options: global dominance of English-medium instruction, standardized testing, and career requirements make avoiding English-based learning impossible. Bears full cost of negative transfer (errors fossilized from L1-English differences) while receiving minimal benefit from asymmetric pedagogical design. Maximum experienced extraction.
constraint_indexing:constraint_classification(cross_linguistic_transfer_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-ENGLISH LANGUAGE LINGUISTIC VALIDITY (SNARE) — Structural victim without agency. Theoretical frameworks derived from English structure are treated as universal baselines; deviations from English patterns are classified as exotic, marked, or defective rather than as equally valid linguistic solutions. Cannot exit this framing; bears the cost of marginalization in linguistic science.
constraint_indexing:constraint_classification(cross_linguistic_transfer_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MULTILINGUAL LEARNER WITH STRATEGIC AGENCY (TANGLED ROPE) — Constrained but not trapped. Faces high costs to exit (switching to non-English pathways reduces career mobility), but retains some agency through strategic language choice and metalinguistic awareness. Benefits from English acquisition (career access, global communication) while bearing costs of transfer interference. Asymmetric but not one-way.
constraint_indexing:constraint_classification(cross_linguistic_transfer_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ENGLISH-SPEAKING LEARNERS AND ENGLISH-MEDIUM INSTITUTIONS (ROPE) — Net beneficiaries experiencing the constraint as pure coordination. English-speaking learners have positive transfer when acquiring structurally similar languages; English-medium educational institutions benefit from a global learner base with aligned linguistic-educational expectations. Extraction runs toward this agent; they experience the constraint as solving a legitimate coordination problem.
constraint_indexing:constraint_classification(cross_linguistic_transfer_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LINGUISTICS RESEARCH COMMUNITY (TANGLED ROPE) — Organized agents (researchers, textbook authors, pedagogical theorists) who are both beneficiaries and enforcers of the asymmetry. Benefit from English-centric frameworks (easier comparative analysis, large English corpus data, English-language publication dominance); constrained by path dependence of established theoretical frameworks. Genuine coordination function (comparative linguistics requires some baseline) co-exists with extractive enforcement (marginalizing non-English structures as 'exceptions').
constraint_indexing:constraint_classification(cross_linguistic_transfer_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: UNIVERSAL GRAMMAR FRAMEWORK (PITON) — Institutional framework (generative linguistics establishment) maintains theoretical apparatus largely through disciplinary inertia despite significant empirical challenges. The UG framework provides organizing principle for linguistic research (genuine coordination function historically), but its enforcement mechanism—privileging English and European languages as primary data sources—persists primarily through academic tradition rather than empirical necessity. Theater ratio reflects the performative maintenance of English-centric data selection despite universal theory claims.
constraint_indexing:constraint_classification(cross_linguistic_transfer_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / COGNITIVE STRUCTURE VIEW (MOUNTAIN) — From a civilizational/universal perspective, some transfer asymmetry is inherent to multilingual acquisition: linguistic differences create differential learning difficulty, and L1 influence is an immutable feature of the bilingual mind. This perspective treats the constraint as natural law. However, the distinction between inherent cognitive asymmetry and institutionally-enforced asymmetry is obscured by this framing.
constraint_indexing:constraint_classification(cross_linguistic_transfer_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cross_linguistic_transfer_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cross_linguistic_transfer_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cross_linguistic_transfer_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cross_linguistic_transfer_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cross_linguistic_transfer_asymmetry, TR),
    TR >= 0.70.

:- end_tests(cross_linguistic_transfer_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from non-English learners and non-English linguistic traditions through multiple mechanisms: (1) pedagogical design optimized for English-speaking learners increases acquisition difficulty for others; (2) theoretical frameworks treat English structures as universal baseline, marginalizing non-English phenomena; (3) global English-medium instruction means non-English learners bear adaptation costs not borne by English speakers. The extraction is not total because multilingual learners gain genuine benefits from English acquisition (career access, global communication), and the constraint does enable some coordination (comparative linguistics genuinely needs baseline for comparison). The increase from 0.35 to 0.52 over the interval reflects intensification of English-medium instruction globally and increasing institutional enforcement through standardized testing and English-language publication requirements. Suppression (0.58): Moderate-high. Significant structural barriers prevent exit: global English dominance in education, technology, science publication, and business creates dependencies that make avoiding English impossible. Non-English learners cannot choose equivalent alternative pathways without major career/economic costs. Suppression is not absolute because alternative systems exist (some countries have strong non-English education); it reflects weighted probability that alternatives are unavailable or inferior in global terms. Theater_ratio (0.68): High and increasing. English-centric pedagogy and theory invoke empirical/scientific justification ('English is more regular,' 'UG is universal') while masking power dynamics and institutional convenience. The performative aspect has increased as the theoretical foundations (UG, universal grammar) face empirical challenges but remain institutionally entrenched. Pedagogical theater involves ritual practices (drilling English-specific phonemic distinctions) presented as necessary rather than optional.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates divergent classification across structural positions. Non-English learners experience Snare (pure extraction, no exit). The linguistics research community experiences Tangled Rope (real coordination function in comparative analysis, genuine extraction through framework enforcement). English-speaking learners and English-medium institutions experience Rope (pure coordination—the system genuinely coordinates learning for them). The research establishment experiences Piton (framework increasingly performative as empirical challenges mount but institutional inertia maintains it). The analytical observer risks Mountain (naturalizing English-centric arrangements as cognitive laws). The perspectival gap reveals that the constraint's classification depends entirely on whether the observer benefits from English dominance or bears its costs—and whether they have institutional power to maintain the framework despite empirical challenges.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derived from beneficiary/victim status and exit options. English-speaking learners (beneficiary + arbitrage options) derive low d → negative f(d) → extraction flows toward them (they are net beneficiaries). Non-English learners (victim + trapped) derive high d → high f(d) → experience maximum extraction. Linguistics research institutions (beneficiary + constrained by framework path dependence) derive moderate d reflecting mixed position: they benefit from the current framework but are locked into it. The organized coalition (researchers, institutions) has exit options available (they could shift to non-English baselines) but faces high costs (retraining, theoretical paradigm shift, loss of comparative coherence), so constrained rather than mobile. The analytical observer at civilizational scope faces the oracle gap: their native observational instruments (the English-based frameworks taught in linguistics courses) cannot detect the power imbalance that cross-position analysis reveals.
 *
 * MANDATROPHY ANALYSIS:
 *   STRUCTURAL DISAMBIGUATION: The mandatrophy is resolved by decomposing 'cross-linguistic transfer asymmetry' into (1) genuine cognitive asymmetry inherent to language learning (inherent negative transfer is real), and (2) institutional asymmetry through English-dominant pedagogy and theory (English-centric frameworks are constructed). The first is closer to Mountain or Rope (coordination around a real cognitive problem). The second is Snare/Tangled Rope (extractive institutional power). The single constraint story absorbs both because they are inseparable in practice: we cannot observe 'pure' cognitive asymmetry independent of institutional contexts. The Tangled Rope classification reflects this: genuine coordination function (comparing languages requires baseline) is inseparable from asymmetric extraction (English chosen as baseline despite alternatives). The classification prevents mislabeling the arrangement as either pure necessity (Mountain falsely naturalized as law) or pure extraction (ignoring real coordination benefit of comparative framework).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_vs_institutional_asymmetry,
    'How much of the observed transfer asymmetry is inherent to language cognitive processing versus institutionally constructed through English-medium pedagogy and theory dominance?',
    'Comparative analysis of transfer patterns across language pairs with equal institutional support; longitudinal study of learners in non-English-dominant educational systems; neuroimaging of transfer mechanisms across typologically different language pairs',
    'If asymmetry is primarily cognitive/inherent: constraint is closer to Mountain (ε ≤ 0.30), suppression reflects natural learning difficulty. If primarily institutional: constraint is Snare (ε ≥ 0.60), suppression reflects power imbalance in knowledge production.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_vs_institutional_asymmetry, empirical, 'Inherent cognitive asymmetry versus institutional construction').

omega_variable(
    generalizability_of_english_structures,
    'Do theoretical frameworks based on English structures genuinely capture universal linguistic principles or do they impose English-specific categories onto typologically diverse languages?',
    'Systematic analysis of phenomena classified as ''exceptions'' in English-based frameworks that are unmarked in other language families; cross-linguistic reanalysis of core theoretical claims (argument structure, word order, case systems) without English as baseline',
    'If English structures are genuinely universal: current framework legitimately coordinates linguistic science. If English is one solution among many: current framework represents extractive paradigm dominance rather than scientific necessity. Determines whether beneficiary/victim classification is justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generalizability_of_english_structures, conceptual, 'Whether English-based structures capture universals or impose specific category system').

omega_variable(
    alternative_pedagogical_effectiveness,
    'Do pedagogical systems that treat non-English L1 and English as equal baselines (rather than English-dominant) produce equivalent or superior learning outcomes for multilingual learners?',
    'Randomized controlled trials comparing English-dominant pedagogy with symmetrical multilingual pedagogy; measurement of fossilization rates, transfer efficiency, and long-term retention across language pairs',
    'If equivalent or superior: current English-dominant system is extractive choice, not efficiency necessity. Supports Snare classification from non-English learner perspective. If English-dominant proves superior: current arrangement has functional justification, reducing Snare aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pedagogical_effectiveness, empirical, 'Learning outcomes under English-dominant versus symmetrical multilingual pedagogy').

omega_variable(
    citation_and_institutional_power_concentration,
    'Does English-language scientific publication requirement create institutional barriers that enforce theoretical dominance, independent of the validity of English-centric linguistic frameworks?',
    'Analysis of citation patterns and institutional advancement rates for researchers working on non-English languages versus English; measurement of access barriers to publication for non-English-speaking researchers; comparison of theory adoption rates when same findings are published in English versus native language venues',
    'If yes: suppression mechanism is partly structural (language publication requirement), not just epistemic. Validates suppression ≥ 0.58 and requires_active_enforcement = true. Indicates Tangled Rope structure (coordination + enforcement mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(citation_and_institutional_power_concentration, empirical, 'Language publication requirement as institutional enforcement mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cross_linguistic_transfer_asymmetry, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clta_tr_t0, cross_linguistic_transfer_asymmetry, theater_ratio, 0, 0.52).
narrative_ontology:measurement(clta_tr_t25, cross_linguistic_transfer_asymmetry, theater_ratio, 25, 0.62).
narrative_ontology:measurement(clta_tr_t50, cross_linguistic_transfer_asymmetry, theater_ratio, 50, 0.68).
narrative_ontology:measurement(clta_tr_t75, cross_linguistic_transfer_asymmetry, theater_ratio, 75, 0.7).

% Extraction over time
narrative_ontology:measurement(clta_be_t0, cross_linguistic_transfer_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clta_be_t25, cross_linguistic_transfer_asymmetry, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(clta_be_t50, cross_linguistic_transfer_asymmetry, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(clta_be_t75, cross_linguistic_transfer_asymmetry, base_extractiveness, 75, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cross_linguistic_transfer_asymmetry, information_standard).
narrative_ontology:boltzmann_floor_override(cross_linguistic_transfer_asymmetry, 0.08).
narrative_ontology:affects_constraint(cross_linguistic_transfer_asymmetry, english_language_hegemony_in_science).
narrative_ontology:affects_constraint(cross_linguistic_transfer_asymmetry, linguistic_theory_universality_claims).
narrative_ontology:affects_constraint(cross_linguistic_transfer_asymmetry, multilingual_education_resource_allocation).

% DUAL FORMULATION NOTE:
% Cross-linguistic transfer asymmetry is upstream of specific language pedagogy and linguistic theory claims. The constraint operates at the intersection of cognitive science (transfer mechanisms are real) and institutional power (English-centric frameworks are enforced). Decomposition into separate stories (cognitive asymmetry story at ε ≤ 0.30 vs institutional enforcement story at ε ≥ 0.55) is possible but loses the important structural insight that institutional arrangements exploit and amplify real cognitive differences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cross_linguistic_transfer_asymmetry, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
