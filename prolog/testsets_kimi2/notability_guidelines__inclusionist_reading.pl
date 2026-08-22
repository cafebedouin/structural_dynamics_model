% ============================================================================
% CONSTRAINT STORY: notability_guidelines__inclusionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__inclusionist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: notability_guidelines__inclusionist_reading
 *   human_readable: Wikipedia Notability Guidelines (Inclusionist Reading)
 *   domain: digital commons governance / knowledge infrastructure / platform constitutionalism
 *
 * SUMMARY:
 *   This constraint story instantiates the inclusionist reading of the
 *   Wikipedia Notability Guidelines (WP:N) kernel. From this reading, the
 *   guideline operates not as a neutral quality filter but as a structural
 *   snare: it systematically excludes knowledge held by marginalized
 *   communities that lack access to institutional publishing infrastructure,
 *   while legitimizing the epistemic authority of mainstream academic and
 *   journalistic sources. The claim/metric independence principle is
 *   observed: the story claims 'snare' while the metrics are authored to
 *   describe the actual operation of the guideline as experienced by excluded
 *   communities, without tuning to match any predicted engine output. The
 *   beneficiaries (institutional publishers) do not directly administer the
 *   constraint; the administrative core enforces it, creating an
 *   inter-institutional dynamic where extraction flows to one seat while
 *   enforcement is performed by another.
 *
 * KEY AGENTS:
 *   - wikipedia_administrative_core (agenda_setter, organized/constrained): enforces notability through deletion processes and policy interpretation
 *   - institutional_knowledge_producers (beneficiary, institutional/mobile): their publications are the mandatory currency of notability
 *   - marginalized_communities (payer, powerless/trapped): bear the cost of exclusion due to lack of 'reliable' institutional sources
 *   - critical_digital_scholars (observer, analytical/analytical): external analysts documenting coverage bias
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, 0.82).
domain_priors:suppression_score(notability_guidelines__inclusionist_reading, 0.78).
domain_priors:theater_ratio(notability_guidelines__inclusionist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__inclusionist_reading, snare).
narrative_ontology:human_readable(notability_guidelines__inclusionist_reading, "Wikipedia Notability Guidelines (Inclusionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__inclusionist_reading, "digital commons governance / knowledge infrastructure / platform constitutionalism").

domain_priors:requires_active_enforcement(notability_guidelines__inclusionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__inclusionist_reading, 'bbe864cc-257f-4130-abc6-867f422eae18').
narrative_ontology:cs_kernel_codification('bbe864cc-257f-4130-abc6-867f422eae18', formalized).
narrative_ontology:cs_authority_grounding('bbe864cc-257f-4130-abc6-867f422eae18', distributed).
narrative_ontology:cs_reading_relation('bbe864cc-257f-4130-abc6-867f422eae18', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('bbe864cc-257f-4130-abc6-867f422eae18', notability_guidelines__deliberative_reading, influences).
narrative_ontology:cs_axiom('bbe864cc-257f-4130-abc6-867f422eae18', foundational, marginalized_knowledge_deserves_structural_inclusion).
narrative_ontology:cs_axiom_status(marginalized_knowledge_deserves_structural_inclusion, holdable).
narrative_ontology:cs_axiom_grounding('bbe864cc-257f-4130-abc6-867f422eae18', marginalized_knowledge_deserves_structural_inclusion, deontological).
narrative_ontology:cs_axiom('bbe864cc-257f-4130-abc6-867f422eae18', foundational, reliable_source_doctrine_reifies_power).
narrative_ontology:cs_axiom_status(reliable_source_doctrine_reifies_power, holdable).
narrative_ontology:cs_axiom_grounding('bbe864cc-257f-4130-abc6-867f422eae18', reliable_source_doctrine_reifies_power, empirically_contingent).
narrative_ontology:cs_reference_frame('bbe864cc-257f-4130-abc6-867f422eae18', universal_encyclopedia_mandate).
narrative_ontology:cs_drift_state('bbe864cc-257f-4130-abc6-867f422eae18', contemporary_digital_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bbe864cc-257f-4130-abc6-867f422eae18', '').
narrative_ontology:cs_kernel_id(notability_guidelines__inclusionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, marginalized_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Volunteer administrators and senior editors who interpret notability policy, close deletion debates, and enforce sourcing standards. Their standing derives from tenure, edit counts, and community trust. Leaving the project means abandoning accumulated reputational capital and editorial influence.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikipedia_administrative_core, agenda_setter,
    organized, biographical, constrained, global).

% Mainstream academic journals, commercial presses, and legacy news organizations whose publications are codified as reliable sources under the guidelines. Their outputs become the mandatory citation currency for article existence, concentrating epistemic authority in established publishing channels.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, institutional_knowledge_producers, beneficiary,
    institutional, generational, mobile, global).

% Community groups, indigenous knowledge holders, and grassroots movements whose histories and expertise lack coverage in mainstream published sources. Their knowledge is procedurally barred from the encyclopedia not because it is false but because it is not institutionally documented. They cannot produce the required citation currency without fundamentally changing their knowledge practices.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, marginalized_communities, payer,
    powerless, biographical, trapped, local).

% Independent researchers and digital-humanities scholars who document systemic bias in Wikipedia's coverage gaps and sourcing hierarchies. They analyze deletion logs, citation networks, and editorial demographics from outside the enforcement structure.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, critical_digital_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:fixing_cost_class(notability_guidelines__inclusionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claimed to filter encyclopedic topics by verifying coverage in established secondary sources, ostensibly preventing degradation from unverifiable or promotional content in a mass-collaboration environment.
% TRANSFER_FUNCTION: Moves epistemic authority and platform visibility from communities lacking institutional publishing infrastructure to established knowledge producers, by mandating the latter's output as the sole gatekeeping currency for article existence.
% ABSENT_VOICES: Indigenous knowledge keepers, oral historians, and grassroots organizers are procedurally excluded: they are not present in notability deliberations because the guidelines define their knowledge forms as outside the evidentiary frame before debate begins.
% DISAPPEARANCE_RATIONALE: If the notability requirement vanished, Wikipedia would absorb community-held knowledge, oral histories, and locally verified expertise currently barred by sourcing thresholds; the epistemic economy would shift away from institutional publishers toward distributed knowledge networks, and the administrative apparatus devoted to deletion enforcement would become obsolete.
% FOUNDING_PROBLEM: Early open-collaboration encyclopedias faced unverifiable submissions, promotional spam, and edit conflicts over fringe topics; a rapid filter was needed to stabilize editorial quality and reader trust.
% FOUNDING_PROBLEM_CORROBORATION: Deletionist editors and the Wikimedia Foundation cite ongoing quality threats as corroboration. External digital-commons researchers and marginalized-community advocates attest that the degradation threat is manageable through less restrictive sourcing frameworks and that the current arrangement primarily reproduces existing epistemic hierarchies.
narrative_ontology:disappearance_verdict(notability_guidelines__inclusionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__inclusionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__inclusionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(notability_guidelines__inclusionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__inclusionist_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__inclusionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(notability_guidelines__inclusionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint extracts epistemic authority from excluded communities and transfers it to institutional publishers. Suppression (0.78) is high because the constraint persists only through active enforcement of sourcing standards and deletion of non-compliant articles; alternatives (other wikis, independent archives) exist but are kept marginal by Wikipedia's dominance. Theater_ratio (0.55) reflects that a substantial portion of guideline enforcement is performative quality theaterâdeletion debates cite procedural neutrality while systematically reproducing hierarchies. Accessibility_collapse (0.72) is high because once an editor internalizes the reliable-source framework, alternative epistemic standards become practically invisible. Resistance (0.45) is moderate: inclusionist campaigns and digital-commons scholarship mount ongoing critique but remain institutionally subordinate.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (administrative core) experiences the constraint as legitimate community self-governance protecting encyclopedic integrity. The payer seat (marginalized communities) experiences the identical structure as epistemic erasure. The beneficiary seat (institutional publishers) experiences it as passive prestige amplification. These divergent computations are driven by the structural dataârole, power, and exit optionsânot by the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional_knowledge_producers are declared beneficiaries with mobile exit options (they need not participate in Wikipedia to survive), yielding a low directionality near the beneficiary pole. Marginalized_communities are declared victims with trapped exit (they cannot easily adopt institutional publishing norms without assimilating out of existence), yielding a high directionality near the target pole. The administrative core is neither beneficiary nor victim in the base_properties arrays; their directionality is structurally intermediate but enforcement-biased.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids piton classification because its founding problem (quality degradation in open collaboration) is still contested as live, and the extraction is actively captured by identifiable beneficiaries rather than merely inertial. It is not a scaffold because it carries no sunset clause and no transitional justification. The inclusionist reading specifically rejects the mandatrophy frame: the guideline is not a spent coordination mechanism but an ongoing extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'This constraint is the inclusionist reading of the notability_guidelines kernel; would the classification change if the deletionist reading (rope) or deliberative reading (scaffold/rope) were adopted instead?',
    'Cross-reading structural comparison: evaluate identical editorial behavior through each reading''s beneficiary/victim frame and coordination claim.',
    'Adopting the deletionist reading would reclassify the constraint as rope or tangled_rope; adopting the deliberative reading would reclassify toward scaffold or rope, eliminating the snare signature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Uncertainty arising from kernel reading selection for the same editorial apparatus').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of marginalized knowledge structural (absence of reliable-source infrastructure in excluded communities) or internalized (editors and subjects have accepted the epistemic hierarchy as natural)?',
    'Post-intervention trajectory: if marginalized knowledge remains excluded after source infrastructure is provided, suppression is partially internalized; if inclusion rises immediately, it was structural.',
    'If internalized, effective suppression exceeds the structural measure because the excluded communities carry the constraint with them; if structural, remedies focus on source creation rather than editor attitude change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    capture_intentionality,
    'Do institutional knowledge producers actively shape notability policy to maintain epistemic dominance, or do they benefit incidentally from a quality-control rule with neutral intent?',
    'Historical trace of policy proposals, donor ties, and editorial-board composition relative to publishing-industry concentration.',
    'Active capture would confirm the snare reading and raise theater_ratio; incidental benefit would suggest tangled_rope or a false summit of naturalized quality standards.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capture_intentionality, empirical, 'Whether beneficiary gains are captured intentionally or incidentally').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__inclusionist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ng_inclusionist_tr_t0, notability_guidelines__inclusionist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ng_inclusionist_tr_t4, notability_guidelines__inclusionist_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(ng_inclusionist_tr_t8, notability_guidelines__inclusionist_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement(ng_inclusionist_tr_t12, notability_guidelines__inclusionist_reading, theater_ratio, 12, 0.45).
narrative_ontology:measurement(ng_inclusionist_tr_t16, notability_guidelines__inclusionist_reading, theater_ratio, 16, 0.5).
narrative_ontology:measurement(ng_inclusionist_tr_t20, notability_guidelines__inclusionist_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(ng_inclusionist_be_t0, notability_guidelines__inclusionist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ng_inclusionist_be_t4, notability_guidelines__inclusionist_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(ng_inclusionist_be_t8, notability_guidelines__inclusionist_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(ng_inclusionist_be_t12, notability_guidelines__inclusionist_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(ng_inclusionist_be_t16, notability_guidelines__inclusionist_reading, base_extractiveness, 16, 0.76).
narrative_ontology:measurement(ng_inclusionist_be_t20, notability_guidelines__inclusionist_reading, base_extractiveness, 20, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(ng_inclusionist_su_t0, notability_guidelines__inclusionist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ng_inclusionist_su_t4, notability_guidelines__inclusionist_reading, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(ng_inclusionist_su_t8, notability_guidelines__inclusionist_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(ng_inclusionist_su_t12, notability_guidelines__inclusionist_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(ng_inclusionist_su_t16, notability_guidelines__inclusionist_reading, suppression_requirement, 16, 0.73).
narrative_ontology:measurement(ng_inclusionist_su_t20, notability_guidelines__inclusionist_reading, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
