% ============================================================================
% CONSTRAINT STORY: genesis_creation_narrative__allegorical_ancient_near_east
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_narrative__allegorical_ancient_near_east, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: genesis_creation_narrative__allegorical_ancient_near_east
 *   human_readable: Genesis Creation Narrative (Allegorical ANE Reading)
 *   domain: religious_studies/biblical_hermeneutics/science_religion_interface
 *
 * SUMMARY:
 *   This constraint represents the interpretive framework that reads Genesis
 *   1-2 as ancient Near Eastern mythopoetic literature, primarily concerned
 *   with theological truths about God and humanity, rather than providing a
 *   scientific or historical account of creation. This reading decouples the
 *   biblical text from scientific claims, allowing for compatibility with
 *   modern cosmology and biology. It is claimed as a 'mountain' because its
 *   truth value is seen as inherent to the nature of the text and its
 *   historical context, not as a human construct for extraction. The low
 *   extractiveness and suppression reflect its status as an academic and
 *   theological consensus within certain circles, offering intellectual
 *   freedom rather than imposing costs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__allegorical_ancient_near_east, 0.05).
domain_priors:suppression_score(genesis_creation_narrative__allegorical_ancient_near_east, 0.1).
domain_priors:theater_ratio(genesis_creation_narrative__allegorical_ancient_near_east, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, extractiveness, 0.05).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__allegorical_ancient_near_east, mountain).
narrative_ontology:human_readable(genesis_creation_narrative__allegorical_ancient_near_east, "Genesis Creation Narrative (Allegorical ANE Reading)").
narrative_ontology:topic_domain(genesis_creation_narrative__allegorical_ancient_near_east, "religious_studies/biblical_hermeneutics/science_religion_interface").

domain_priors:emerges_naturally(genesis_creation_narrative__allegorical_ancient_near_east).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__allegorical_ancient_near_east, '1cd00477-91ae-44cd-a531-19116dff5134').
narrative_ontology:cs_kernel_codification('1cd00477-91ae-44cd-a531-19116dff5134', fixed_text).
narrative_ontology:cs_authority_grounding('1cd00477-91ae-44cd-a531-19116dff5134', expertise).
narrative_ontology:cs_interpretation_layer_present('1cd00477-91ae-44cd-a531-19116dff5134').
narrative_ontology:cs_reading_relation('1cd00477-91ae-44cd-a531-19116dff5134', genesis_creation_narrative__literal_young_earth, forecloses).
narrative_ontology:cs_reading_relation('1cd00477-91ae-44cd-a531-19116dff5134', genesis_creation_narrative__theistic_evolutionary, coexists_with).
narrative_ontology:cs_axiom('1cd00477-91ae-44cd-a531-19116dff5134', foundational, genesis_as_mythopoetic_genre).
narrative_ontology:cs_axiom_status(genesis_as_mythopoetic_genre, holdable).
narrative_ontology:cs_axiom_grounding('1cd00477-91ae-44cd-a531-19116dff5134', genesis_as_mythopoetic_genre, conventional).
narrative_ontology:cs_axiom('1cd00477-91ae-44cd-a531-19116dff5134', foundational, biblical_authority_is_theological_not_scientific).
narrative_ontology:cs_axiom_status(biblical_authority_is_theological_not_scientific, holdable).
narrative_ontology:cs_axiom_grounding('1cd00477-91ae-44cd-a531-19116dff5134', biblical_authority_is_theological_not_scientific, deontological).
narrative_ontology:cs_reference_frame('1cd00477-91ae-44cd-a531-19116dff5134', ancient_near_eastern_literary_context).
narrative_ontology:cs_drift_state('1cd00477-91ae-44cd-a531-19116dff5134', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1cd00477-91ae-44cd-a531-19116dff5134', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, theologians_and_scholars).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, science_religion_dialogue_advocates).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, biblical_inerrancy_of_purpose).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, ancient_near_eastern_contextualization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a hermeneutic that allows for robust theological interpretation of Genesis 1-2 without conflict with modern scientific understanding. This reading provides intellectual coherence and academic credibility.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, theologians_and_scholars, beneficiary,
    institutional, generational, mobile, global).

% Advocate for this reading as a way to bridge perceived gaps between religious faith and scientific inquiry, fostering mutual respect and understanding. They benefit from the reduction of conflict.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, science_religion_dialogue_advocates, beneficiary,
    organized, biographical, mobile, global).

% Represent interpretive communities that reject this allegorical reading, insisting on a literal, historical-scientific interpretation of Genesis. They are excluded from the academic consensus that underpins this reading.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, literalist_interpretations, excluded,
    organized, generational, identity_locked, global).

% Observes the theological discourse, largely indifferent to internal hermeneutical debates as long as religious claims do not impinge on scientific methodology or findings. This reading is seen as compatible with scientific inquiry.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, scientific_community, observer,
    institutional, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates theological understanding of creation with scientific cosmology and biology by defining the scope of biblical authority as primarily theological and existential, not scientific.
% TRANSFER_FUNCTION: Transfers interpretive authority over natural history from the biblical text to scientific inquiry, while retaining theological authority for the text's message about God's relationship with creation.
% ABSENT_VOICES: Literalist interpreters are absent from the academic discourse that champions this reading; they would argue that this reading compromises biblical authority and leads to theological liberalism.
% DISAPPEARANCE_RATIONALE: If this interpretive constraint vanished, the scientific world would remain unchanged, as it does not rely on biblical texts for its understanding of cosmology. The theological world would revert to a state of greater tension with science, but the underlying texts and scientific facts would persist.
% FOUNDING_PROBLEM: The perceived conflict between modern scientific discoveries (e.g., evolution, Big Bang cosmology) and traditional literal interpretations of Genesis 1-2, leading to intellectual dissonance for many believers.
% FOUNDING_PROBLEM_CORROBORATION: The problem is widely attested by scholars, theologians, and laypeople engaged in science-religion dialogue, as well as by surveys indicating high rates of belief in both science and religion among the general population. This corroboration comes from outside the direct beneficiaries of this specific reading.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__allegorical_ancient_near_east, world_unchanged).
narrative_ontology:founding_problem_status(genesis_creation_narrative__allegorical_ancient_near_east, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__allegorical_ancient_near_east, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(genesis_creation_narrative__allegorical_ancient_near_east, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, ExtMetricName, E),
    domain_priors:suppression_score(genesis_creation_narrative__allegorical_ancient_near_east, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(genesis_creation_narrative__allegorical_ancient_near_east),
    narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) reflects that this reading primarily offers intellectual coherence and resolves conflict, rather than imposing significant costs or extracting resources. Suppression (0.1) is minimal, as this reading is largely self-sustaining within its academic and theological context, requiring little active enforcement beyond scholarly consensus. Theater ratio is 0.0 as there is no performative aspect; its function is purely interpretive. Accessibility collapse is high (0.9) because once the ANE context is understood, alternative scientific-literal readings become largely untenable within this framework. Resistance is low (0.05) from within the adopting community, though significant resistance exists from external literalist communities.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap within this reading's adherents, as it aims to resolve conflict. The gap exists between this reading and literalist interpretations, which view this approach as a compromise of biblical authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Theologians, scholars, and science-religion dialogue advocates are beneficiaries, as this reading provides a coherent framework for their work. The scientific community is an observer, largely unaffected as this reading does not challenge scientific methodology. Literal interpretations are 'excluded' as their premises are incompatible with this reading's foundational axioms.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its 'mandate' is to provide a coherent theological interpretation. Its function remains live as long as the tension between literalist readings and scientific discovery persists. It prevents mislabeling genuine theological insight as extraction by clarifying the text's intended genre and purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genre_classification_ambiguity,
    'Is the classification of Genesis 1-2 as ''ancient Near Eastern mythopoetic literature'' an objective textual analysis or a theological choice driven by a desire to reconcile with science?',
    'Comparative literary analysis of a broader corpus of ANE creation myths and ancient Hebrew narrative forms, independent of modern scientific concerns.',
    'If primarily a theological choice, the ''emerges_naturally'' claim for this reading''s genre classification would be weakened, potentially shifting its classification from Mountain to a more constructed type (e.g., Rope or Scaffold) for those who do not share the theological motivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genre_classification_ambiguity, conceptual, 'Ambiguity in the genre classification of Genesis 1-2.').

omega_variable(
    dominion_metaphor_normative_force,
    'Does the ''dominion'' metaphor in Genesis 1:28, when read allegorically, retain any normative force for human environmental ethics, or is its ethical content entirely decoupled from the text''s original context?',
    'Analysis of contemporary theological and ethical discourse that explicitly derives environmental ethics from this allegorical reading, and its reception by environmental advocates.',
    'If the dominion metaphor loses all normative force, this reading might be seen as extracting ethical guidance from the text, rather than merely reinterpreting its genre. If it retains a transformed normative force (e.g., stewardship), the reading''s ethical implications are preserved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dominion_metaphor_normative_force, preference, 'Normative force of the dominion metaphor in an allegorical reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__allegorical_ancient_near_east, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1900, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1900, 0.0).
narrative_ontology:measurement(gene_tr_t1950, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1950, 0.0).
narrative_ontology:measurement(gene_tr_t2000, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 2000, 0.0).
narrative_ontology:measurement(gene_tr_t2024, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(gene_be_t1900, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement(gene_be_t1950, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(gene_be_t2000, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(gene_be_t2024, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1900, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(gene_su_t1950, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(gene_su_t2000, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(gene_su_t2024, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__allegorical_ancient_near_east, information_standard).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__literal_young_earth).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__theistic_evolutionary).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'genesis_creation_narrative' kernel. This 'allegorical_ancient_near_east' reading decouples Genesis from scientific claims, influencing but not foreclosing other readings. The 'literal_young_earth' reading asserts historical-scientific inerrancy, while the 'theistic_evolutionary' reading seeks compatibility with science through different interpretive strategies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
