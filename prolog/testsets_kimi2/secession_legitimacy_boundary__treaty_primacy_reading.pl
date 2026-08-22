% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__treaty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__treaty_primacy_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: secession_legitimacy_boundary__treaty_primacy_reading
 *   human_readable: Secession Legitimacy Boundary â Treaty Primacy Reading
 *   domain: political/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint story instantiates the treaty_primacy_reading of the
 *   secession_legitimacy_boundary kernel. The standing arrangement under
 *   contest is the Canadian-style constitutional order (and comparable
 *   federal settler states) wherein secession may be claimed, debated, and
 *   pursued by provincial governments and settler majorities without the
 *   consent of Indigenous treaty holders whose territories and rights would
 *   be transferred between jurisdictions. The treaty_primacy reading assesses
 *   this arrangement as structurally extractive: it treats federal and
 *   provincial sovereignty as sufficient for territorial reconfiguration
 *   while subordinating pre-existing nation-to-nation treaty relationships.
 *   The reading asserts that Indigenous treaty rights predate and supersede
 *   both federal and provincial authority, rendering any secession without
 *   treaty-holder consent illegitimate. Indigenous nations enter the victim
 *   set because the arrangement extracts their territorial sovereignty and
 *   collapses their international status into domestic minority standing
 *   during constitutional crises.
 *
 * KEY AGENTS:
 *   - federal_authority (agenda_setter/institutional/arbitrage) â administers and enforces the constitutional framework that omits treaty consent
 *   - provincial_governments (beneficiary/institutional/constrained) â initiate secession politics within boundaries recognized without Indigenous parallel consent
 *   - settler_populations (beneficiary/organized/mobile) â exercise democratic sovereignty treated as sufficient for constitutional change
 *   - indigenous_treaty_holders (payer/organized/identity_locked) â bear sovereignty extraction; exit from treaty relationship dissolves collective identity
 *   - constitutional_courts (observer/institutional/analytical) â legitimize omission of treaty consent as procedural regularity
 *   - international_human_rights_bodies (excluded/institutional/constrained) â would object under UNDRIP but are kept outside domestic negotiation tables
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, 0.8).
domain_priors:suppression_score(secession_legitimacy_boundary__treaty_primacy_reading, 0.75).
domain_priors:theater_ratio(secession_legitimacy_boundary__treaty_primacy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__treaty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__treaty_primacy_reading, "Secession Legitimacy Boundary â Treaty Primacy Reading").
narrative_ontology:topic_domain(secession_legitimacy_boundary__treaty_primacy_reading, "political/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__treaty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__treaty_primacy_reading, 'd5473e1e-32a6-4472-8141-3948cf2afb87').
narrative_ontology:cs_kernel_codification('d5473e1e-32a6-4472-8141-3948cf2afb87', fixed_text).
narrative_ontology:cs_authority_grounding('d5473e1e-32a6-4472-8141-3948cf2afb87', lineage).
narrative_ontology:cs_interpretation_layer_present('d5473e1e-32a6-4472-8141-3948cf2afb87').
narrative_ontology:cs_reading_relation('d5473e1e-32a6-4472-8141-3948cf2afb87', secession_legitimacy_boundary__constitutional_impossibility_reading, influences).
narrative_ontology:cs_reading_relation('d5473e1e-32a6-4472-8141-3948cf2afb87', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('d5473e1e-32a6-4472-8141-3948cf2afb87', secession_legitimacy_boundary__grievance_threshold_reading, influences).
narrative_ontology:cs_axiom('d5473e1e-32a6-4472-8141-3948cf2afb87', foundational, treaty_rights_predate_supersede_constitutional_authority).
narrative_ontology:cs_axiom_status(treaty_rights_predate_supersede_constitutional_authority, holdable).
narrative_ontology:cs_axiom_grounding('d5473e1e-32a6-4472-8141-3948cf2afb87', treaty_rights_predate_supersede_constitutional_authority, conventional).
narrative_ontology:cs_axiom('d5473e1e-32a6-4472-8141-3948cf2afb87', foundational, secession_legitimacy_requires_treaty_consent).
narrative_ontology:cs_axiom_status(secession_legitimacy_requires_treaty_consent, holdable).
narrative_ontology:cs_axiom_grounding('d5473e1e-32a6-4472-8141-3948cf2afb87', secession_legitimacy_requires_treaty_consent, deontological).
narrative_ontology:cs_reference_frame('d5473e1e-32a6-4472-8141-3948cf2afb87', treaty_nation_nation_sovereignty).
narrative_ontology:cs_drift_state('d5473e1e-32a6-4472-8141-3948cf2afb87', contemporary_settler_constitutional_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('d5473e1e-32a6-4472-8141-3948cf2afb87', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, federal_authority).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, settler_populations).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the constitutional order and asserts supremacy over treaty relationships; can alter constitutional interpretation or negotiate secession frameworks without requiring Indigenous treaty-holder consent, preserving territorial integrity on settler terms.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, federal_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from a constitutional framework that recognizes their territorial claims and referendum authority without requiring parallel Indigenous consent; can initiate secession politics within provincial boundaries while treaty obligations are treated as subordinate or manageable within domestic negotiations.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_governments, beneficiary,
    institutional, generational, constrained, regional).

% Exercise democratic sovereignty through provincial and federal institutions; their majority votes are treated as sufficient for constitutional change and secession claims, while treaty-holder consent is structurally omitted from legitimacy requirements.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, settler_populations, beneficiary,
    organized, biographical, mobile, national).

% Bear the cost of territorial reconfiguration and sovereignty claims that override treaty rights; their nation-to-nation status is collapsed into domestic minority status during secession debates; exit from the treaty relationship is unthinkable as it would dissolve their sovereignty, land base, and collective identity.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders, payer,
    organized, civilizational, identity_locked, national).

% Interpret constitutional silence on treaty consent in secession contexts; their rulings establish procedural clarity for settler governments without requiring Indigenous treaty-holder consent, legitimizing the extraction as procedural regularity.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Would object to secession without Indigenous self-determination under UNDRIP and international law, but are treated as external observers rather than parties to domestic constitutional negotiation; their exclusion maintains the coherence of the settler-state coordination story.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, international_human_rights_bodies, excluded,
    institutional, generational, constrained, global).

narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__treaty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a constitutional mechanism for managing territorial breakup and secession claims within a federal state without requiring unanimous consent of all affected nations, preserving macro-stability and democratic expression for settler-majority populations.
% TRANSFER_FUNCTION: Transfers jurisdictional authority and territorial control from Indigenous treaty holders to federal or provincial governments during constitutional crises or secession campaigns, without requiring treaty-holder consent.
% ABSENT_VOICES: Indigenous treaty nations and international bodies upholding Indigenous self-determination are structurally excluded from secession negotiation tables; they would demand treaty-based veto or parallel nation-to-nation negotiation tracks but are treated as stakeholders of domestic governments rather than sovereign counterparts.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, secession negotiations would have to include Indigenous nations as equal parties, federal and provincial authority would be truncated at treaty boundaries, and the current territorial integrity assumptions of settler states would require fundamental renegotiation.
% FOUNDING_PROBLEM: How to manage territorial breakup and provincial sovereignty claims within a federal state without triggering civil conflict or endless veto points, while maintaining democratic legitimacy for settler populations.
% FOUNDING_PROBLEM_CORROBORATION: Federal and provincial constitutional scholars attest the need for a unified secession framework. Indigenous legal scholars and international human rights bodies outside the benefiting parties attest that the founding problem was framed to exclude pre-existing treaty sovereignty and continues to externalize costs onto non-consenting nations.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__treaty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__treaty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__treaty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 0.8, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.80) because the standing arrangement transfers territorial and jurisdictional authority from Indigenous treaty holders to settler governments without consent, a direct extraction of sovereignty. Suppression is high (0.75) because the constraint persists through active constitutional interpretation, judicial procedure, and political exclusion that keeps Indigenous nations off secession negotiation tables. Theater ratio is moderate (0.45) and rising: consultation processes have proliferated (duty to consult doctrine) but remain performative because they carry no veto or consent requirement, creating a widening gap between procedural theatre and substantive power. Accessibility collapse is high (0.72) because once inside the settler constitutional framework, alternatives (treaty-based sovereignty, international Indigenous self-determination) are legally collapsed or reframed as domestic interests. Resistance is high (0.70) reflecting sustained Indigenous legal and political mobilization against the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   The federal and provincial seats experience the constraint as genuine coordination: it provides clear rules for managing breakup, avoids civil war, and channels democratic expression. The Indigenous treaty-holder seat experiences the identical structure as enforced extraction: the same rules that coordinate settler governments do so by appropriating Indigenous territory and authority without consent. The engine computes this divergence from structural data â the coordination function is real for beneficiaries while the extraction is total for the identity-locked payer.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal authority, provincial governments, and settler populations are structural beneficiaries of the constitutional silence on treaty consent; their directionality sits near the beneficiary end (low d), dampening their effective extraction. Indigenous treaty holders are declared victims with identity-locked exit options, placing their directionality near the full-target end (high d), amplifying effective extraction. Constitutional courts are observers with analytical exit, directionality neutral. International bodies are excluded and constrained, directionality moderately high but excluded from the computation surface.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy guard prevents mislabeling this as pure coordination (rope) or pure extraction (snare). The arrangement was built to solve a genuine coordination problem â managing territorial breakup in a federal democracy â which prevents snare classification. However, the asymmetric extraction is structural and necessary: Indigenous nations cannot be included in the coordination without truncating settler sovereignty claims, meaning the coordination is purchased through extraction. This hybrid is the defining signature of tangled_rope. The temporal measurements show theater rising as the coordination function ages, indicating drift toward performance, but the base coordination problem (secession management) remains live for settler parties, preventing piton classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_consent_enforceability,
    'Is Indigenous treaty consent a legally enforceable constitutional requirement or a political moral claim without institutional remedy under current settler law?',
    'Supreme Court or international tribunal ruling explicitly requiring Indigenous consent for territorial severance; constitutional amendment practice establishing treaty veto.',
    'If enforceable, the constraint reclassifies toward rope or scaffold as a genuine coordination mechanism; if unenforceable, it remains a tangled_rope or snare where extraction continues under cover of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_consent_enforceability, empirical, 'Legal enforceability of treaty consent requirement').

omega_variable(
    secession_coordination_scope,
    'Does the secession framework coordinate all affected nations or only settler governments, and does the exclusion of Indigenous nations constitute incidental cost or targeted extraction?',
    'Comparative analysis of secession negotiations for Indigenous presence and veto authority; assessment of whether coordination function fails without Indigenous exclusion.',
    'If Indigenous exclusion is structural and necessary to the coordination function, the constraint is a tangled_rope; if the coordination could include them at no cost to the function, it is a snare using coordination as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secession_coordination_scope, conceptual, 'Whether secession coordination inherently requires Indigenous exclusion').

omega_variable(
    kernel_reading_interaction,
    'How does the treaty primacy reading structurally interact with competing secession legitimacy frameworks (constitutional impossibility, popular sovereignty, grievance threshold)?',
    'Corpus analysis of how treaty_primacy reading is cited in legal and political discourse relative to sibling readings; logical entailment analysis of core premises.',
    'Determines whether treaty primacy forecloses popular sovereignty or merely influences constitutional readings; affects network coupling and kernel family classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_interaction, conceptual, 'Structural relationship between treaty primacy and sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__treaty_primacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(slb_treaty_primacy_tr_t0, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(slb_treaty_primacy_tr_t5, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(slb_treaty_primacy_tr_t10, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(slb_treaty_primacy_tr_t15, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(slb_treaty_primacy_tr_t20, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(slb_treaty_primacy_tr_t25, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 25, 0.43).
narrative_ontology:measurement(slb_treaty_primacy_tr_t30, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(slb_treaty_primacy_be_t0, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(slb_treaty_primacy_be_t5, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 5, 0.63).
narrative_ontology:measurement(slb_treaty_primacy_be_t10, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement(slb_treaty_primacy_be_t15, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 15, 0.7).
narrative_ontology:measurement(slb_treaty_primacy_be_t20, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 20, 0.73).
narrative_ontology:measurement(slb_treaty_primacy_be_t25, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement(slb_treaty_primacy_be_t30, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 30, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(slb_treaty_primacy_su_t0, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(slb_treaty_primacy_su_t5, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(slb_treaty_primacy_su_t10, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(slb_treaty_primacy_su_t15, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(slb_treaty_primacy_su_t20, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(slb_treaty_primacy_su_t25, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(slb_treaty_primacy_su_t30, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, grievance_threshold_reading).

% DUAL FORMULATION NOTE:
% The secession_legitimacy_boundary kernel decomposes into four structurally distinct constraint stories because each reading posits a different ultimate source of legitimacy (treaty, constitutional text, popular vote, grievance threshold), producing different beneficiary/victim structures and epsilon values. This story (treaty_primacy_reading) links to all siblings as competing readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
