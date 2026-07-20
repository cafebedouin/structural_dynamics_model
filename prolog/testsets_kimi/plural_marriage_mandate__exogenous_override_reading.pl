% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__exogenous_override_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: plural_marriage_mandate__exogenous_override_reading
 *   human_readable: Federal Coercion of LDS Plural Marriage via 1890 Manifesto (Exogenous Override Reading)
 *   domain: religious/political
 *
 * SUMMARY:
 *   This constraint story instantiates the exogenous_override_reading of the
 *   plural_marriage_mandate kernel. It treats the 1890 Manifesto not as an
 *   endogenous doctrinal development but as the terminal instrument of
 *   federal coercion that forced the LDS Church to abandon a practiced divine
 *   requirement. The constraint is the coercive apparatusâimprisonment,
 *   property seizure, statehood conditionalitiesâthat extracted abandonment
 *   from practicing polygamists and the church, benefiting the federal
 *   government's project of territorial conformity. The structural data are
 *   authored independently of the sibling readings:
 *   endogenous_reinterpretation_reading and institutional_pragmatism_reading.
 *
 * KEY AGENTS:
 *   - us_federal_state: Agenda-setter and beneficiary (institutional/arbitrage) â imposes anti-polygamy enforcement to secure territorial conformity.
 *   - practicing_polygamists: Primary target (powerless/identity_locked) â bear extraction through criminalization and forced abandonment of religious practice.
 *   - lds_church_leadership: Coerced administrator (organized/constrained) â issues and enforces the manifesto under federal duress, bearing doctrinal cost.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, 0.82).
domain_priors:suppression_score(plural_marriage_mandate__exogenous_override_reading, 0.91).
domain_priors:theater_ratio(plural_marriage_mandate__exogenous_override_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__exogenous_override_reading, snare).
narrative_ontology:human_readable(plural_marriage_mandate__exogenous_override_reading, "Federal Coercion of LDS Plural Marriage via 1890 Manifesto (Exogenous Override Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__exogenous_override_reading, "religious/political").

domain_priors:requires_active_enforcement(plural_marriage_mandate__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__exogenous_override_reading, '07f3af3c-0338-4ebe-80c2-b9d91965842a').
narrative_ontology:cs_kernel_codification('07f3af3c-0338-4ebe-80c2-b9d91965842a', fixed_text).
narrative_ontology:cs_authority_grounding('07f3af3c-0338-4ebe-80c2-b9d91965842a', lineage).
narrative_ontology:cs_interpretation_layer_present('07f3af3c-0338-4ebe-80c2-b9d91965842a').
narrative_ontology:cs_reading_relation('07f3af3c-0338-4ebe-80c2-b9d91965842a', plural_marriage_mandate__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('07f3af3c-0338-4ebe-80c2-b9d91965842a', plural_marriage_mandate__institutional_pragmatism_reading, influences).
narrative_ontology:cs_axiom('07f3af3c-0338-4ebe-80c2-b9d91965842a', foundational, coerced_manifesto_lacks_doctrinal_authority).
narrative_ontology:cs_axiom_status(coerced_manifesto_lacks_doctrinal_authority, holdable).
narrative_ontology:cs_axiom_grounding('07f3af3c-0338-4ebe-80c2-b9d91965842a', coerced_manifesto_lacks_doctrinal_authority, theological).
narrative_ontology:cs_reference_frame('07f3af3c-0338-4ebe-80c2-b9d91965842a', binding_plural_marriage_mandate).
narrative_ontology:cs_drift_state('07f3af3c-0338-4ebe-80c2-b9d91965842a', post_1890_manifesto, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('07f3af3c-0338-4ebe-80c2-b9d91965842a', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, us_federal_state).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, lds_church_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces anti-polygamy legislation including the Edmunds-Tucker Act, using imprisonment, property seizure, and statehood conditionalities to compel abandonment of plural marriage. Collects territorial political conformity and elimination of a competing sovereignty in the Mountain West.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, us_federal_state, agenda_setter,
    institutional, generational, arbitrage, national).

% LDS members practicing plural marriage as a religious duty under D&C 132. After 1890 they face federal imprisonment, disenfranchisement, and property seizure, forcing abandonment of families or underground existence. Their theological identity is fused with the practice; compliance constitutes spiritual crisis.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists, payer,
    powerless, biographical, identity_locked, national).

% Issues the 1890 Manifesto under direct federal threat of church dissolution and property confiscation. Administers the abandonment of plural marriage to the membership while internally preserving the doctrine as theologically valid. Bears the cost of doctrinal contradiction and loss of theological coherence.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, lds_church_leadership, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, lds_church_leadership, payer).

% Members and leaders who reject the manifesto as coerced and continue plural marriage in hiding or schism. Excluded from post-manifesto church councils and federal amnesty negotiations; their theological objections are criminalized and dismissed as rebellion.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, fundamentalist_dissenters, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__exogenous_override_reading, us_federal_state).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the political integration of Utah Territory into the United States by eliminating the religious practice that blocked federal recognition and statehood.
% TRANSFER_FUNCTION: Moves compliance with monogamous marriage norms from the federal state to the LDS Church and its members, extracting abandonment of plural families and theological submission in exchange for cessation of property seizure and the promise of statehood.
% ABSENT_VOICES: Practicing polygamist wives, whose consent and spiritual stakes in existing plural families were erased from both federal enforcement discourse and church administrative deliberations; fundamentalist dissenters who continued to regard D&C 132 as binding and were excluded from the manifesto's drafting.
% DISAPPEARANCE_RATIONALE: If the federal coercion had vanished in 1890, the LDS Church would likely have continued solemnizing plural marriages; Utah statehood would have been delayed or conditioned on continued conflict; the theological trajectory of Mormonism would not have pivoted at this juncture and the manifesto itself would not have been issued in this form.
% FOUNDING_PROBLEM: The federal government faced a territorially concentrated religious community practicing polygamy in defiance of national law, creating a political and legal anomaly that blocked Utah's admission to the Union and challenged federal supremacy in the West.
% FOUNDING_PROBLEM_CORROBORATION: Federal legislators, territorial governors, and the Supreme Court (Reynolds v. United States, 1879) attested to the political problem from the enforcing seat. Historical scholarship outside the federal beneficiary seat corroborates that the anti-polygamy campaign was driven by national political consensus rather than by the theological needs of the LDS Church.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__exogenous_override_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(plural_marriage_mandate__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__exogenous_override_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint extracts doctrinal abandonment, family dissolution, and criminal status from its victims. Suppression (0.91) is near-maximum because persistence depends on federal imprisonment and property seizure, plus the implicit threat of church dissolution. Theater_ratio (0.70) is high because the manifesto's performative function is to present coerced capitulation as voluntary revelation; the masking is central to the constraint's operation. Accessibility_collapse (0.75) reflects that legal and ecclesiastical alternatives to compliance were effectively closed for mainstream members. Resistance (0.68) captures the ongoing underground practice and schismatic formation, which never reached levels that threatened federal control but remained visible.
 *
 * PERSPECTIVAL GAP:
 *   From the federal seat, the constraint is legitimate law enforcement resolving a territorial anomaly; computed type may approach rope or enforcement_mechanism. From the practicing_polygamist seat, the identical structure is pure extraction through state power; computed type is snare. The engine's cross-seat divergence is the measurement the corpus seeks. The church leadership seat experiences yet a third type: it is neither subsidized nor fully targeted in the base declarations, yielding a directionality that reflects its structural entrapment.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal state is the sole declared beneficiary, positioned at low directionality (subsidy side): the constraint delivers territorial political conformity to it at negligible cost. Practicing polygamists are the declared victims, positioned at high directionality (target side): they bear the full cost of extraction through imprisonment, disenfranchisement, and spiritual coercion. The lds_church_leadership is structurally ambiguousâit administers the constraint but is not a beneficiary; without a victim declaration its directionality reverts to the organized-power fallback, leaving it in the middle. This captures its dual position as both enforcer and coerced party.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâfederal supremacy over a territorially concentrated dissenting religious practiceâwas solved by 1910. The constraint's active coercive apparatus should have sunset, yet the manifesto persists as a binding doctrinal text and the church continues to discipline post-manifesto polygamists. This (founding_problem_status=dead + disappearance_verdict=world_rearranges) flags a zombie/piton trajectory: the federal snare atrophied into institutional self-policing. The temporal measurements show extraction and suppression peaking mid-interval and declining, consistent with an enforcement apparatus that completed its work and left an inertial residue.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the exogenous_override_reading of kernel plural_marriage_mandate. Its sibling endogenous_reinterpretation_reading would reclassify the manifesto as legitimate prophetic suspension with an empty victim set, while institutional_pragmatism_reading would introduce a church-survival beneficiary seat and reclassify as tangled_rope. Where is the disagreement structurally located?',
    'Triangulate federal archival coercion evidence, internal church deliberative records, and post-manifesto practice data against the three readings'' divergent predictions about beneficiary/victim structure.',
    'The disagreement is located at the locus of authorship: whether the abandonment was authored by the federal state (snare), by divine revelation (rope/scaffold), or by institutional leadership under survival pressure (tangled_rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer-frame location of the kernel contest for the 1890 manifesto.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (federal imprisonment and property seizure under the Edmunds-Tucker Act) or internalized (church disciplinary councils and member self-policing after 1904)?',
    'Post-exit suppression trajectory: compare rates of federal prosecution versus church excommunication for plural marriage across the measurement interval; if church discipline persists after federal prosecution declines, suppression is partially internalized.',
    'If internalized suppression dominates, the constraint''s effective extractiveness is higher than the structural measure suggestsâmembers carry the suppression internallyâand the church leadership seat moves toward agenda-setter extraction rather than federal victimization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in the post-manifesto period.').

omega_variable(
    church_beneficiary_ambiguity,
    'Does the LDS Church''s organizational survival and eventual statehood attainment constitute a beneficiary interest that undermines the pure victim framing of this reading?',
    'Assess whether the church as an institution received net benefits (property preservation, legal incorporation, statehood) that offset the doctrinal costs, and whether these benefits were contingent on the manifesto.',
    'If the church is a net beneficiary, the constraint reclassifies toward tangled_rope; the directionality of the church leadership seat shifts from target to beneficiary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(church_beneficiary_ambiguity, conceptual, 'Whether institutional survival creates a beneficiary interest for the church.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__exogenous_override_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t0, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0, 0.78).
narrative_ontology:measurement(plur_tr_t6, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 6, 0.72).
narrative_ontology:measurement(plur_tr_t12, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 12, 0.65).
narrative_ontology:measurement(plur_tr_t18, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 18, 0.58).
narrative_ontology:measurement(plur_tr_t24, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 24, 0.52).
narrative_ontology:measurement(plur_tr_t30, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(plur_be_t0, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(plur_be_t6, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 6, 0.75).
narrative_ontology:measurement(plur_be_t12, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 12, 0.82).
narrative_ontology:measurement(plur_be_t18, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 18, 0.8).
narrative_ontology:measurement(plur_be_t24, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 24, 0.76).
narrative_ontology:measurement(plur_be_t30, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t0, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(plur_su_t6, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 6, 0.92).
narrative_ontology:measurement(plur_su_t12, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 12, 0.9).
narrative_ontology:measurement(plur_su_t18, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 18, 0.87).
narrative_ontology:measurement(plur_su_t24, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 24, 0.82).
narrative_ontology:measurement(plur_su_t30, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
