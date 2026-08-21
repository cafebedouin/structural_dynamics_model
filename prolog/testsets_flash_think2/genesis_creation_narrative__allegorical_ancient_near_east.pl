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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Genesis Creation Narrative as Ancient Near Eastern Mythopoetic Literature
 *   domain: religious_studies/biblical_hermeneutics/science_religion_interface
 *
 * SUMMARY:
 *   This constraint describes the interpretive framework that understands
 *   Genesis 1-2 as ancient Near Eastern mythopoetic literature, rather than a
 *   literal historical or scientific account. This reading emphasizes the
 *   text's theological and literary functions, decoupling it from modern
 *   scientific claims. It is a conciliatory approach that seeks to resolve
 *   perceived conflicts between religious faith and scientific understanding.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__allegorical_ancient_near_east, 0.15).
domain_priors:suppression_score(genesis_creation_narrative__allegorical_ancient_near_east, 0.1).
domain_priors:theater_ratio(genesis_creation_narrative__allegorical_ancient_near_east, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, extractiveness, 0.15).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__allegorical_ancient_near_east, rope).
narrative_ontology:human_readable(genesis_creation_narrative__allegorical_ancient_near_east, "Genesis Creation Narrative as Ancient Near Eastern Mythopoetic Literature").
narrative_ontology:topic_domain(genesis_creation_narrative__allegorical_ancient_near_east, "religious_studies/biblical_hermeneutics/science_religion_interface").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__allegorical_ancient_near_east, '8b4c73ba-645a-4664-ac3f-6de47b9e8f63').
narrative_ontology:cs_kernel_codification('8b4c73ba-645a-4664-ac3f-6de47b9e8f63', fixed_text).
narrative_ontology:cs_authority_grounding('8b4c73ba-645a-4664-ac3f-6de47b9e8f63', expertise).
narrative_ontology:cs_interpretation_layer_present('8b4c73ba-645a-4664-ac3f-6de47b9e8f63').
narrative_ontology:cs_reading_relation('8b4c73ba-645a-4664-ac3f-6de47b9e8f63', genesis_creation_narrative__literal_young_earth, forecloses).
narrative_ontology:cs_reading_relation('8b4c73ba-645a-4664-ac3f-6de47b9e8f63', genesis_creation_narrative__theistic_evolutionary, coexists_with).
narrative_ontology:cs_axiom('8b4c73ba-645a-4664-ac3f-6de47b9e8f63', foundational, genesis_as_ancient_near_eastern_mythopoetic_genre).
narrative_ontology:cs_axiom_status(genesis_as_ancient_near_eastern_mythopoetic_genre, holdable).
narrative_ontology:cs_axiom_grounding('8b4c73ba-645a-4664-ac3f-6de47b9e8f63', genesis_as_ancient_near_eastern_mythopoetic_genre, conventional).
narrative_ontology:cs_axiom('8b4c73ba-645a-4664-ac3f-6de47b9e8f63', foundational, theological_truth_not_scientific_fact).
narrative_ontology:cs_axiom_status(theological_truth_not_scientific_fact, holdable).
narrative_ontology:cs_axiom_grounding('8b4c73ba-645a-4664-ac3f-6de47b9e8f63', theological_truth_not_scientific_fact, deontological).
narrative_ontology:cs_reference_frame('8b4c73ba-645a-4664-ac3f-6de47b9e8f63', modern_biblical_criticism_framework).
narrative_ontology:cs_drift_state('8b4c73ba-645a-4664-ac3f-6de47b9e8f63', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8b4c73ba-645a-4664-ac3f-6de47b9e8f63', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, theologians_biblical_scholars).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, science_religion_dialogue_advocates).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, scientific_community).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, lay_adherents_seeking_coherence).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, mythopoetic_interpretation_doctrine).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, complementarity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These scholars develop and propagate the interpretive framework, benefiting from its intellectual coherence and the academic legitimacy it provides for biblical studies in a scientific age. They set the agenda for how the text is understood within academic and progressive religious circles.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, theologians_biblical_scholars, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__allegorical_ancient_near_east, theologians_biblical_scholars, beneficiary).

% Advocates for constructive engagement between science and religion find this reading highly beneficial, as it removes a major point of conflict and allows for a more nuanced discussion of faith and reason. It provides a common ground for dialogue.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, science_religion_dialogue_advocates, beneficiary,
    organized, biographical, mobile, global).

% The scientific community benefits from this reading by having religious texts decoupled from scientific claims, reducing perceived conflict and allowing scientific inquiry to proceed without theological interference. It fosters an environment of intellectual respect.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, scientific_community, beneficiary,
    institutional, generational, analytical, universal).

% Individuals who wish to reconcile their religious faith with scientific understanding find this reading provides intellectual and spiritual coherence, alleviating cognitive dissonance. Their exit options are constrained by their personal faith commitments.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, lay_adherents_seeking_coherence, beneficiary,
    moderate, biographical, constrained, local).

% From the perspective of this reading, those who insist on a literal, historical-scientific interpretation of Genesis are excluded from the interpretive conversation. They perceive this reading as undermining biblical authority and theological truth, and are identity-locked into their own framework.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, literalists_young_earth_creationists, excluded,
    organized, generational, identity_locked, national).

% These critics observe the debate, often viewing this reading as a strategic retreat by religion to avoid scientific refutation. They are not directly impacted by the constraint but analyze its implications for the broader science-religion discourse.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, secular_critics_of_religion, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_narrative__allegorical_ancient_near_east, diffuse).
narrative_ontology:fixing_cost_class(genesis_creation_narrative__allegorical_ancient_near_east, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide an interpretive framework for Genesis 1-2 that respects its ancient literary context and theological purpose, thereby resolving perceived conflicts with modern scientific understanding and fostering intellectual coherence for adherents.
% TRANSFER_FUNCTION: This reading transfers interpretive authority for Genesis 1-2 from literal-historical-scientific claims to its ancient Near Eastern mythopoetic genre and theological meaning, from those who seek scientific validation to those who prioritize literary and theological context.
% ABSENT_VOICES: Literalists and Young Earth Creationists are largely absent from the conversation that defines and propagates this reading. They would vehemently object, arguing that this approach compromises the inerrancy and authority of scripture, and that it is an accommodation to secular science.
% DISAPPEARANCE_RATIONALE: If this interpretive framework vanished, the perceived conflict between religious texts and scientific understanding would intensify for many, leading to increased intellectual dissonance, a hardening of literalist positions, and a more polarized public discourse on science and religion. The ability for many to hold both faith and scientific understanding would be severely challenged.
% FOUNDING_PROBLEM: The perceived and actual conflict between traditional literal interpretations of Genesis 1-2 and the findings of modern cosmology, geology, and evolutionary biology, which created intellectual dissonance and a crisis of faith for many religious adherents.
% FOUNDING_PROBLEM_CORROBORATION: Biblical scholars, theologians, scientists engaged in science-religion dialogue, and numerous lay adherents attest to the ongoing tension and the need for interpretive frameworks that bridge the gap. Academic publications, interfaith dialogues, and personal testimonies from outside the immediate beneficiaries corroborate the persistence of this problem.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__allegorical_ancient_near_east, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_narrative__allegorical_ancient_near_east, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__allegorical_ancient_near_east, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(genesis_creation_narrative__allegorical_ancient_near_east, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).
:- end_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics are low because this reading primarily functions as a coordination mechanism for intellectual coherence. Extractiveness is low as it doesn't impose significant costs but rather offers a way to resolve dissonance. Suppression is low because it's an interpretive option, not a coercive dogma, though it faces resistance from literalist camps. Theater ratio is low as its function is genuine intellectual and spiritual reconciliation. Accessibility collapse is low because it opens up, rather than closes off, avenues for understanding.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of its proponents, this reading is a 'rope' that facilitates understanding and reduces conflict. From the perspective of literalists, it might be seen as a 'snare' that undermines biblical authority, but this story focuses on the internal structure of the 'allegorical_ancient_near_east' reading itself, which is genuinely coordinative for its adherents.
 *
 * DIRECTIONALITY LOGIC:
 *   Theologians, biblical scholars, science-religion dialogue advocates, and lay adherents seeking coherence are all beneficiaries, as this reading provides intellectual tools and reduces conflict. The scientific community also benefits by having religious texts interpreted in a way that doesn't challenge scientific findings. Literalists are 'excluded' from this reading's conversation, as its core premise directly contradicts their own.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a stable, independent reading, or is its identity primarily defined by its opposition to literalist interpretations?',
    'Analysis of scholarly discourse: if the reading''s arguments are primarily constructive and internally coherent, it''s stable; if they are mostly reactive critiques of literalism, its identity is oppositional.',
    'If primarily oppositional, its ''rope'' classification might be unstable, as its function is less about coordination and more about counter-suppression against a rival reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the reading''s identity is self-standing or reactive.').

omega_variable(
    dominion_metaphor_normative_force,
    'Even when Genesis 1-2 is read mythopoetically, does the ''dominion'' metaphor still carry an implicit normative force that contributes to ecological extraction or human exceptionalism?',
    'Empirical studies of environmental ethics and theological interpretations: does this reading consistently lead to stewardship ethics, or does it implicitly permit exploitation?',
    'If the dominion metaphor retains an extractive normative force, the ''base_extractiveness'' of this reading might be subtly higher than currently assessed, particularly for ecological systems.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dominion_metaphor_normative_force, empirical, 'Implicit extractive force of the ''dominion'' metaphor in a mythopoetic reading.').

omega_variable(
    reception_by_literalists,
    'To what extent is this reading genuinely ignored by literalist communities versus actively suppressed or demonized within those communities?',
    'Sociological and theological analysis of literalist publications, sermons, and educational materials regarding their engagement with non-literal interpretations.',
    'If actively suppressed, the ''suppression'' metric for this reading (from the perspective of literalists) would be higher, indicating a more contentious and less purely coordinative landscape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reception_by_literalists, empirical, 'Nature of literalist engagement with mythopoetic interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__allegorical_ancient_near_east, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1950, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(gene_tr_t1970, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1970, 0.04).
narrative_ontology:measurement(gene_tr_t1990, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1990, 0.03).
narrative_ontology:measurement(gene_tr_t2010, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 2010, 0.04).
narrative_ontology:measurement(gene_tr_t2024, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(gene_be_t1950, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement(gene_be_t1970, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1970, 0.14).
narrative_ontology:measurement(gene_be_t1990, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1990, 0.13).
narrative_ontology:measurement(gene_be_t2010, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement(gene_be_t2024, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1950, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(gene_su_t1970, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1970, 0.09).
narrative_ontology:measurement(gene_su_t1990, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1990, 0.08).
narrative_ontology:measurement(gene_su_t2010, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 2010, 0.09).
narrative_ontology:measurement(gene_su_t2024, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__allegorical_ancient_near_east, information_standard).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, science_religion_dialogue).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, theistic_evolutionary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'genesis_creation_narrative' kernel. It focuses on the mythopoetic interpretation, which contrasts with literalist and theistic evolutionary readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
