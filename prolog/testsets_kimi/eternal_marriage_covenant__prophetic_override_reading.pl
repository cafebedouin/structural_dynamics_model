% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__prophetic_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__prophetic_override_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: eternal_marriage_covenant__prophetic_override_reading
 *   human_readable: Prophetic Override of Eternal Marriage Covenant
 *   domain: religious/political_theology
 *
 * SUMMARY:
 *   In 1890, under sustained federal pressure to abolish plural marriage, the
 *   President of The Church of Jesus Christ of Latter-day Saints issued the
 *   Manifesto, suspending the practice of polygamy. The
 *   prophetic_override_reading treats this not as a mere political
 *   accommodation but as a legitimate exercise of continuing revelation: the
 *   living prophet received new divine instruction that superseded the prior
 *   eternal commandment recorded in D&C 132. This reading instantiates one
 *   structural constraint within the contested kernel of the eternal marriage
 *   covenant. It is authored as a tangled_rope because it carries a genuine
 *   coordination function (institutional survival, legal compliance,
 *   doctrinal flexibility) alongside asymmetric extraction (polygamous
 *   families destroyed, doctrinal hardliners excommunicated, prior covenants
 *   voided) and requires active enforcement to hold.
 *
 * KEY AGENTS:
 *   - church_president: Primary agenda_setter (institutional/constrained) â receives and promulgates overriding revelation
 *   - church_institution: Primary beneficiary (institutional/arbitrage) â captures survival and legitimacy
 *   - polygamous_families: Primary target (powerless/trapped) â bear the costs of family dissolution and criminalization
 *   - doctrinal_hardliners: Secondary target (powerless/identity_locked) â bear the costs of doctrinal reversal and excommunication
 *   - monogamous_membership: Secondary beneficiary (organized/constrained) â gains social legitimacy at the cost of doctrinal uncertainty
 *   - federal_government: Analytical observer (institutional/analytical) â external coercive pressure shaping the constraint's necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, 0.68).
domain_priors:suppression_score(eternal_marriage_covenant__prophetic_override_reading, 0.71).
domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__prophetic_override_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__prophetic_override_reading, "Prophetic Override of Eternal Marriage Covenant").
narrative_ontology:topic_domain(eternal_marriage_covenant__prophetic_override_reading, "religious/political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__prophetic_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__prophetic_override_reading, '768a08fa-e43f-4f13-82c5-a549035614db').
narrative_ontology:cs_kernel_codification('768a08fa-e43f-4f13-82c5-a549035614db', fixed_text).
narrative_ontology:cs_authority_grounding('768a08fa-e43f-4f13-82c5-a549035614db', lineage).
narrative_ontology:cs_interpretation_layer_present('768a08fa-e43f-4f13-82c5-a549035614db').
narrative_ontology:cs_reading_relation('768a08fa-e43f-4f13-82c5-a549035614db', eternal_marriage_covenant__immutable_commandment_reading, forecloses).
narrative_ontology:cs_reading_relation('768a08fa-e43f-4f13-82c5-a549035614db', eternal_marriage_covenant__temporal_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('768a08fa-e43f-4f13-82c5-a549035614db', foundational, living_prophet_supersedes_past_revelation).
narrative_ontology:cs_axiom_status(living_prophet_supersedes_past_revelation, holdable).
narrative_ontology:cs_axiom_grounding('768a08fa-e43f-4f13-82c5-a549035614db', living_prophet_supersedes_past_revelation, theological).
narrative_ontology:cs_axiom('768a08fa-e43f-4f13-82c5-a549035614db', foundational, ecclesiastical_preservation_authorizes_doctrinal_override).
narrative_ontology:cs_axiom_status(ecclesiastical_preservation_authorizes_doctrinal_override, holdable).
narrative_ontology:cs_axiom_grounding('768a08fa-e43f-4f13-82c5-a549035614db', ecclesiastical_preservation_authorizes_doctrinal_override, instrumental).
narrative_ontology:cs_reference_frame('768a08fa-e43f-4f13-82c5-a549035614db', prophetic_succession_framework).
narrative_ontology:cs_drift_state('768a08fa-e43f-4f13-82c5-a549035614db', post_manifesto_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('768a08fa-e43f-4f13-82c5-a549035614db', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_institution).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, monogamous_membership).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, polygamous_families).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, doctrinal_hardliners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the office of living prophet, seer, and revelator. Receives and promulgates the Manifesto and subsequent revelations that suspend plural marriage. Cannot resign the office without seismic institutional rupture; the prophetic role is binding and lifetime.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_president, agenda_setter,
    institutional, generational, constrained, global).

% Collects legal survival, federal-state legitimacy, property retention, and mainstream social acceptance from the suspension. The institutional corpus remains intact and expands after compliance. Doctrine can be reinterpreted to suit new circumstances without formal schism.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_institution, beneficiary,
    institutional, generational, arbitrage, global).

% Benefits from simplified family structure, reduced federal stigma, and continued social cohesion in Utah and surrounding regions. Accepts the doctrinal shift as evidence of living prophetic guidance. Exit means abandoning community and kinship networks.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, monogamous_membership, beneficiary,
    organized, biographical, constrained, national).

% Previously commanded to enter plural marriage under prior revelation. Now commanded to dissolve new plural unions and cease cohabitation. Face legal prosecution, ecclesiastical discipline, loss of temple standing, and family fragmentation. No legal or ecclesiastical path to maintain prior covenants.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, polygamous_families, payer,
    powerless, biographical, trapped, national).

% Believe D&C 132 establishes an immutable eternal law required for exaltation. Experience the override as doctrinal betrayal. Face excommunication, loss of community, and psychological rupture because their spiritual identity is fused with the permanence of the original commandment.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, doctrinal_hardliners, payer,
    powerless, biographical, identity_locked, national).

% Applied coercive pressure through the Poland Act, Edmunds-Tucker Act, and denial of Utah statehood. Observes the Manifesto as a political compliance outcome rather than an internal doctrinal event. Does not participate in ecclesiastical governance but shapes the constraint's environment.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, federal_government, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__prophetic_override_reading, church_institution).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__prophetic_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables the church to adapt its marriage doctrine to existential federal pressure while maintaining internal coherence, prophetic authority, and institutional survival without formal schism.
% TRANSFER_FUNCTION: Moves authority to define valid marriage practice from the fixed text of D&C 132 to the living prophet's office; moves social and legal legitimacy from polygamous families to monogamous members and the federal state.
% ABSENT_VOICES: Polygamous wives and children whose family structures were forcibly destabilized; fundamentalist dissenters excommunicated for adhering to the prior prophetic command; non-Mormon federal observers who interpreted the Manifesto as political theater rather than genuine revelation.
% DISAPPEARANCE_RATIONALE: Without the prophetic override mechanism, the church could not have suspended polygamy under federal pressure. Federal seizure of property, imprisonment of leaders, and denial of statehood would likely have intensified, fragmenting the church into competing factions or destroying its corporate existence. The doctrinal structure of continuing revelation would have been tested to breaking point.
% FOUNDING_PROBLEM: The church faced existential crisis under federal anti-polygamy enforcement, including property seizure through the Edmunds-Tucker Act, disincorporation, imprisonment of leaders, and prolonged denial of Utah statehood.
% FOUNDING_PROBLEM_CORROBORATION: Federal congressional records, the Poland Act, and Supreme Court decisions (Reynolds v. United States) attest the external pressure from outside the benefiting parties. Secular historians and political scientists corroborate the existential threat. Dissenting Mormon fundamentalistsâwho are victims of the constraint, not beneficiariesâattest the problem is dead but argue the solution was apostasy, not revelation.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__prophetic_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__prophetic_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__prophetic_override_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eternal_marriage_covenant__prophetic_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__prophetic_override_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is moderately high because the constraint transfers the costs of institutional survival onto a specific, previously-obedient population. Suppression (0.71) is higher than extraction because the constraint's persistence depends on actively prosecuting and excommunicating those who continue the prior practice. Theater_ratio (0.48) reflects the sustained performative tension of maintaining that the original revelation was 'eternal' while simultaneously treating it as supersedeable. Accessibility_collapse (0.58) captures the degree to which alternatives (fundamentalist schism, individual conscience, prior covenant continuity) were closed off by ecclesiastical and legal enforcement. Resistance (0.68) reflects the active hardliner opposition, underground plural marriage, and federal skepticism about the sincerity of the Manifesto. The measurement series share one time grid (1890â1920) to prevent misaligned temporal sampling.
 *
 * PERSPECTIVAL GAP:
 *   The institutional seat experiences the constraint as necessary adaptation and divine providence; the polygamous_family seat experiences it as betrayal and extraction; the hardliner seat experiences it as doctrinal corruption. The engine computes this divergence from the same structural data. The perspectival gap is wide because the identity-locked exit of the hardliners amplifies their effective extraction relative to the mobile, arbitrage-capable institution.
 *
 * DIRECTIONALITY LOGIC:
 *   The church_president and church_institution sit near the beneficiary end: they control the mechanism of override and collect institutional survival. Monogamous_membership sits near symmetric-to-beneficiary: they gain social legitimacy and suffer only diffuse doctrinal uncertainty. Polygamous_families and doctrinal_hardliners sit near the full-target end: they bear concentrated, non-transferable costs (family dissolution, excommunication, identity rupture) and lack exit. The federal_government is structurally external; its d is analytically derived as neutral/observer. No directionality overrides are needed because the structural derivation (beneficiary/victim + exit) captures the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâexistential federal pressureâis dead (Utah statehood was granted in 1896, federal enforcement eased). Yet the constraint persists as a doctrinal precedent establishing that the living prophet can supersede any prior revelation. This is a mandatrophy risk: the mechanism created for survival now authorizes ongoing doctrinal flexibility. However, the constraint is not yet a piton because the coordination function (prophetic adaptability) continues to be actively used and valued by the institution. The theater_ratio being below 0.5 at interval end suggests the function has not fully atrophied into pure performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_or_accommodation,
    'Is the prophetic override of the eternal marriage covenant genuine continuing revelation, or is it a political accommodation retrospectively theologized by the institutional church?',
    'Comparative analysis of prophetic statements and private correspondence around 1890 (e.g., Woodruff''s public vs. private framing), coupled with institutional behavior after the federal threat subsided.',
    'If predominantly accommodation, the constraint''s coordination function is cover for institutional extraction and the type should compute toward snare; if genuine revelation, the coordination function is authentic and tangled_rope remains appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_or_accommodation, empirical, 'Empirical ambiguity about whether the override was divine or political in origin').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the post-Manifesto suppression of polygamy driven primarily by external federal coercion or by internalized ecclesiastical loyalty?',
    'Post-exit suppression trajectory: if polygamous families continued the practice after leaving Utah or the church, the suppression was external; if they voluntarily abandoned it due to internalized belief in prophetic authority, the suppression was partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, raising extraction for identity-locked agents. If external, extraction is bounded by enforcement capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism ambiguity').

omega_variable(
    reading_relation_ambiguity,
    'Does the prophetic_override reading logically foreclose the immutable_commandment reading, or can they be held as temporally-indexed truths in a single evolving theological framework?',
    'Examine institutional discourse for whether D&C 132 is treated as permanently superseded, temporarily suspended, or still binding in principle.',
    'If permanently superseded, forecloses is correct; if suspendable or temporally indexed, the relation may be influences or coexists_with, altering the kernel''s contamination topology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_ambiguity, conceptual, 'Uncertainty about the logical relationship between prophetic override and immutable readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__prophetic_override_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prophetic_override_tr_t0, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(prophetic_override_tr_t5, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(prophetic_override_tr_t10, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(prophetic_override_tr_t15, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(prophetic_override_tr_t20, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(prophetic_override_tr_t25, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement(prophetic_override_tr_t30, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(prophetic_override_be_t0, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(prophetic_override_be_t5, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(prophetic_override_be_t10, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(prophetic_override_be_t15, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(prophetic_override_be_t20, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(prophetic_override_be_t25, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(prophetic_override_be_t30, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(prophetic_override_su_t0, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(prophetic_override_su_t5, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(prophetic_override_su_t10, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(prophetic_override_su_t15, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(prophetic_override_su_t20, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(prophetic_override_su_t25, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(prophetic_override_su_t30, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__prophetic_override_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__prophetic_override_reading, 0.08).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% The eternal_marriage_covenant kernel decomposes into three structurally distinct constraints. The immutable_commandment_reading treats D&C 132 as a fixed, unchangeable divine law with negligible extraction for the institution but high extraction for those who abandon it. The prophetic_override_reading (this file) treats the same text as supersedeable by living revelation, generating a tangled_rope with active enforcement. The temporal_accommodation_reading treats the text as still valid but temporarily suspended for political obedience. Each reading has a distinct epsilon, stakeholder set, and directionality structure. They are linked as a constraint family via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
