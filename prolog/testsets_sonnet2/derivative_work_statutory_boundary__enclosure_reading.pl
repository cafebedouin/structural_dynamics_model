% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__enclosure_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__enclosure_reading
 *   human_readable: Enclosure Reading of the Derivative Work Boundary (Any Use = Preparation)
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   Under the enclosure reading, the statutory derivative-work boundary is
 *   drawn at the point of USE rather than at the point of substantial
 *   similarity or commercial exploitation: incorporating any copyrighted
 *   expression while creating a new work is itself preparation of a
 *   derivative work, triggering the exclusive right before any finished
 *   product exists and regardless of how the source material is transformed.
 *   This reading is advanced most forcefully by major rightsholders and the
 *   licensing infrastructure built around broad clearance requirements, and
 *   it structurally advantages incumbents who can afford to hold, license,
 *   and litigate over expression against independent creators, fan
 *   communities, small AI research labs, remix artists, and archives who
 *   cannot.
 *
 * KEY AGENTS:
 *   - major_rightsholder_studios: institutional beneficiary and agenda-setter; drafts and litigates the broad standard, collects licensing revenue
 *   - licensing_intermediaries: organized beneficiary; clearance-house business model depends on the boundary staying broad
 *   - independent_creators, fan_communities, remix_and_sampling_artists: powerless payers; practice is presumptively infringing preparation under this reading
 *   - ai_training_researchers: moderate-power payer; training itself becomes actionable preparation independent of output similarity
 *   - archival_institutions: moderate-power payer; preservation and cataloguing work slowed by clearance requirements
 *   - courts_and_copyright_office: institutional observer; adjudicates which reading of the kernel controls
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, 0.81).
domain_priors:suppression_score(derivative_work_statutory_boundary__enclosure_reading, 0.76).
domain_priors:theater_ratio(derivative_work_statutory_boundary__enclosure_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__enclosure_reading, snare).
narrative_ontology:human_readable(derivative_work_statutory_boundary__enclosure_reading, "Enclosure Reading of the Derivative Work Boundary (Any Use = Preparation)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__enclosure_reading, "intellectual_property_law/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__enclosure_reading, '5e6eb418-59d4-45ba-a90a-3eca2c60cd79').
narrative_ontology:cs_kernel_codification('5e6eb418-59d4-45ba-a90a-3eca2c60cd79', fixed_text).
narrative_ontology:cs_authority_grounding('5e6eb418-59d4-45ba-a90a-3eca2c60cd79', lineage).
narrative_ontology:cs_interpretation_layer_present('5e6eb418-59d4-45ba-a90a-3eca2c60cd79').
narrative_ontology:cs_reading_relation('5e6eb418-59d4-45ba-a90a-3eca2c60cd79', derivative_work_statutory_boundary__coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('5e6eb418-59d4-45ba-a90a-3eca2c60cd79', derivative_work_statutory_boundary__hybrid_carveout_reading, coexists_with).
narrative_ontology:cs_axiom('5e6eb418-59d4-45ba-a90a-3eca2c60cd79', foundational, use_of_expression_is_sufficient_trigger).
narrative_ontology:cs_axiom_status(use_of_expression_is_sufficient_trigger, holdable).
narrative_ontology:cs_axiom_grounding('5e6eb418-59d4-45ba-a90a-3eca2c60cd79', use_of_expression_is_sufficient_trigger, conventional).
narrative_ontology:cs_axiom('5e6eb418-59d4-45ba-a90a-3eca2c60cd79', secondary, transformation_degree_is_legally_irrelevant_to_threshold).
narrative_ontology:cs_axiom_status(transformation_degree_is_legally_irrelevant_to_threshold, holdable).
narrative_ontology:cs_axiom_grounding('5e6eb418-59d4-45ba-a90a-3eca2c60cd79', transformation_degree_is_legally_irrelevant_to_threshold, conventional).
narrative_ontology:cs_reference_frame('5e6eb418-59d4-45ba-a90a-3eca2c60cd79', authorial_exclusivity_over_recasting).
narrative_ontology:cs_drift_state('5e6eb418-59d4-45ba-a90a-3eca2c60cd79', post_digital_remix_and_ai_training_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5e6eb418-59d4-45ba-a90a-3eca2c60cd79', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, major_rightsholder_studios).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, licensing_intermediaries).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, ip_litigation_firms).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, independent_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, fan_communities).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, ai_training_researchers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, remix_and_sampling_artists).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, archival_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold large back catalogs of copyrighted expression and lobby for the broadest possible reading of what counts as preparing a derivative work, since that reading forces anyone touching their material into a licensing relationship. They collect licensing fees, settlement payments, and platform takedown compliance as direct revenue streams, and they draft or sponsor the litigation that establishes the broad standard in case law.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, major_rightsholder_studios, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__enclosure_reading, major_rightsholder_studios, agenda_setter).

% Operate clearance houses, rights-management platforms, and collective licensing societies that exist because the enclosure reading makes almost any incorporation of existing expression a legal risk requiring pre-clearance. Their business model depends on the boundary staying broad; a narrower coordination-style reading would shrink their necessary function.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, licensing_intermediaries, beneficiary,
    organized, generational, arbitrage, global).

% Generate billable work from the ambiguity and breadth of the enclosure standard — the broader and vaguer the rule, the more disputes require adjudication, discovery, and settlement negotiation. They benefit from the standard's enforceability without needing to hold any copyrights themselves.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, ip_litigation_firms, beneficiary,
    organized, biographical, mobile, national).

% Write fiction, make videos, or compose music that references, echoes, or builds on existing copyrighted works. Under this reading, any such use is treated as preparation of a derivative work requiring authorization before the fact, regardless of transformation or intent. They cannot afford clearance counsel or licensing fees, so most either abandon the work, publish anyway and risk takedown/litigation, or self-censor preemptively.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, independent_creators, payer,
    powerless, biographical, constrained, global).

% Produce fan fiction, fan art, and fan-edited media as an established cultural practice built directly on copyrighted source material. Under the enclosure reading their entire practice is presumptively infringing preparation, tolerated only at the rightsholder's discretion via cease-and-desist forbearance rather than legal right — a status they cannot rely on and cannot contest without resources.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, fan_communities, payer,
    powerless, biographical, trapped, global).

% Train generative models by processing large corpora that include copyrighted expression. Under this reading, the act of using that expression at all in constructing a new generative system is itself preparation of a derivative work, exposing them to licensing demands and litigation regardless of whether any specific output reproduces protected expression. Well-funded labs can absorb licensing costs; smaller research groups and open-source projects cannot.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, ai_training_researchers, payer,
    moderate, biographical, constrained, global).

% Build music, collage, and mashup works by incorporating fragments of existing recordings and compositions. The enclosure reading treats even brief or transformative incorporation as preparation requiring prior clearance, which as a practical matter is unavailable to artists without label backing — most such work is created and distributed in violation of the standard as authored, with enforcement applied selectively.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, remix_and_sampling_artists, payer,
    powerless, biographical, constrained, global).

% Libraries, museums, and preservation organizations that create annotated collections, restorations, and derivative scholarly editions from copyrighted holdings. The broad standard forces them to seek clearance for cataloguing and preservation activities that they argue serve a public function distinct from commercial exploitation, slowing or blocking preservation work pending rights clearance that is often practically impossible for orphan works.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, archival_institutions, payer,
    moderate, civilizational, constrained, national).

% Adjudicate derivative work disputes and issue rulemaking guidance. They receive competing framings from rightsholders (broad preparation standard) and defendants/researchers (narrower substantial-incorporation or transformative-use standards) and their rulings determine which reading of the kernel controls in a given jurisdiction and era.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, courts_and_copyright_office, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__enclosure_reading, major_rightsholder_studios).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, a derivative-work boundary lets creators know in advance whether they need a license, reducing disputes over new works built on existing expression and giving original authors a predictable claim on downstream commercial exploitation of their material.
% TRANSFER_FUNCTION: Moves control over the raw material of new cultural and technical production — the right to build on existing expression at all, prior to any finished work existing — from creators, researchers, and archivists to rightsholders and the clearance intermediaries who monetize the resulting licensing requirement.
% ABSENT_VOICES: Independent creators, fan communities, small AI research groups, and archival institutions rarely have a seat in the litigation or legislative drafting that sets the standard; the doctrine is shaped primarily through disputes between well-resourced rightsholders and well-resourced defendants (major platforms, major studios), leaving the diffuse population of small-scale downstream creators as rule-takers with no voice in rule-making.
% DISAPPEARANCE_RATIONALE: If the enclosure reading vanished and a narrower substantial-incorporation standard controlled instead, licensing-clearance intermediaries would lose their core reason for existing, litigation volume over incidental and transformative use would collapse, fan communities and remix artists could operate openly rather than at rightsholder sufferance, and AI training practices would be adjudicated on output similarity rather than input use — a substantial reallocation of leverage away from incumbent rightsholders.
% FOUNDING_PROBLEM: Copyright law needed a mechanism to prevent someone from taking an author's expression, making superficial changes, and selling the result as a new work without compensating the original author — the paradigm case being unauthorized translations, sequels, and adaptations that free-ride on an author's creative investment.
% FOUNDING_PROBLEM_CORROBORATION: Rightsholder trade associations and licensing bodies attest the broad preparation standard remains necessary to prevent free-riding on creative investment, particularly against large-scale commercial exploitation and AI training at scale. Independent scholarship (fair use researchers, comparative doctrine analysis from jurisdictions with narrower transformative-use standards), archivists' associations, and open-source research consortia — all outside the beneficiary set — attest that the founding problem (protecting authors from unauthorized commercial exploitation of their expression) is adequately addressed by a substantial-similarity or transformative-use standard, and that the 'any use constitutes preparation' reading exceeds the founding problem to capture territory the original doctrine never covered.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__enclosure_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81 at interval end) because the enclosure reading collapses the distinction between transformative and infringing use, making the mere act of incorporating existing expression — at the moment of creation, before any finished work is evaluated — sufficient to trigger the exclusive right. Suppression (0.76) is authored separately and structurally: enforcement operates through pre-emptive licensing demands, cease-and-desist threats, and platform takedown regimes that function whether or not litigation is ever filed, which is a raw structural fact about how the boundary is policed, not scaled by any actor's power or the scope of any single dispute. Theater ratio is comparatively low (0.28) because the enforcement apparatus (licensing clearance, litigation, takedown) is functionally doing what it claims to do — extracting payment and control — rather than performing an empty ritual; this is a genuinely operative extraction mechanism, not an atrophied one.
 *
 * DIRECTIONALITY LOGIC:
 *   Major rightsholder studios and licensing intermediaries sit at the beneficiary end: they collect licensing revenue and clearance fees that exist specifically because the boundary is drawn broadly, and they have arbitrage-grade exit (they can license, litigate, or forbear selectively as suits their interest). Independent creators, fan communities, and remix artists sit at the full-target end: trapped or constrained exit, no resources to seek clearance or contest the standard, and the practical reality that most of their output is created in violation of the standard as authored. AI training researchers and archival institutions occupy a middle-target position — moderate power gives them some capacity to negotiate licenses or litigate fair use, but the pre-creation trigger still forces costly clearance processes before any output exists.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing unauthorized commercial exploitation of an author's expression via superficial derivative recastings — remains partially live (translations, sequels, and direct commercial adaptations without authorization do still occur and do warrant a remedy). But the enclosure reading's 'any use constitutes preparation' standard extends far past that founding problem to capture transformative fan works, incidental sampling, preservation copying, and machine-learning training that do not compete with or substitute for the original commercial market. The classification prevents mislabeling this extension as pure coordination: a genuine coordination reading (the coordination_reading sibling) would confine itself to the founding problem's scope; this reading's breadth is the mandatrophy — a mandate that has outrun the problem it was built to solve, now sustained because the intermediary and rightsholder ecosystem built on top of the broad reading has an independent interest in its persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enclosure_vs_coordination_boundary_location,
    'Is the correct location of the derivative-work boundary at the point of use (this reading) or at the point of substantial incorporation into a fixed new work (the coordination_reading sibling)?',
    'Comparative doctrinal analysis across jurisdictions that have adopted narrower transformative-use or substantial-similarity standards, tracking whether original-author incentives to create measurably weaken relative to jurisdictions using the broad preparation standard.',
    'If downstream creative and research output increases without measurable harm to original authors'' incentives under the narrower standard, that would support recharacterizing this reading''s breadth as extraction rather than necessary protection; if authors'' incentives measurably weaken, it would support the enclosure reading''s necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enclosure_vs_coordination_boundary_location, conceptual, 'Where the derivative-work trigger point should structurally sit — at use versus at substantial fixed incorporation.').

omega_variable(
    commercial_carveout_relevance,
    'Does the commercial/non-commercial distinction advanced by the hybrid_carveout_reading sibling capture the morally and economically relevant line that the enclosure reading erases?',
    'Empirical study of market substitution effects: do non-commercial transformative works (fan fiction, remix, AI research prototypes) measurably substitute for or cannibalize the commercial market for the original work?',
    'If non-commercial use rarely substitutes for the original market, the enclosure reading''s refusal to carve out non-commercial use would be shown as extraction beyond the founding problem''s scope; if substitution is common even for non-commercial derivative use, the enclosure reading''s breadth would be more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_carveout_relevance, empirical, 'Whether commercial exploitation is the economically relevant boundary the enclosure reading ignores.').

omega_variable(
    ai_training_use_classification,
    'Is training a generative model on copyrighted expression structurally more like ''preparation of a derivative work'' (this reading) or more like reading/analysis that produces a categorically new kind of output (closer to the coordination reading)?',
    'Technical and legal analysis of whether trained model weights retain or reproduce protectable expression versus statistical patterns; ongoing litigation outcomes across multiple jurisdictions.',
    'Resolution in favor of the derivative-preparation framing would validate this reading''s extension to AI training as consistent rather than an overreach; resolution against would identify AI training licensing demands under this reading as extraction beyond the doctrine''s coherent scope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ai_training_use_classification, empirical, 'Whether generative-model training is structurally analogous to preparing a derivative work or to a distinct, non-infringing analytical use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__enclosure_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(deri_tr_t8, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement(deri_tr_t16, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(deri_tr_t24, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(deri_tr_t32, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(deri_tr_t40, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(deri_be_t8, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(deri_be_t16, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(deri_be_t24, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 24, 0.74).
narrative_ontology:measurement(deri_be_t32, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 32, 0.78).
narrative_ontology:measurement(deri_be_t40, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(deri_su_t8, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(deri_su_t16, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(deri_su_t24, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(deri_su_t32, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(deri_su_t40, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 40, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__coordination_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the derivative_work_statutory_boundary kernel. coordination_reading authors a low-epsilon rope/coordination structure confined to fixed recastings substantially incorporating original expression, with transformative and intermediate uses treated as non-infringing. hybrid_carveout_reading authors a middle structure conditioned on commercial exploitation. This story (enclosure_reading) authors the high-epsilon snare structure in which any use whatsoever in preparing a new work triggers the derivative-work right pre-creation. All three share the same underlying statutory text and case-law lineage but diverge sharply in epsilon, beneficiary/victim structure, and classification because they read the trigger condition differently — per the epsilon-invariance principle, this is three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
