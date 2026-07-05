% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: plural_marriage_mandate__exogenous_override_reading
 *   human_readable: 1890 Manifesto as Federally Coerced Abandonment of Divine Mandate
 *   domain: religious institutional history / political theology
 *
 * SUMMARY:
 *   This story instantiates the exogenous_override reading of the plural
 *   marriage mandate kernel: the 1890 Manifesto is treated as the terminal
 *   event in a coercive federal campaign (Morrill 1862, Poland 1874, Edmunds
 *   1882, Edmunds-Tucker 1887) that escalated from criminalization to mass
 *   imprisonment to total corporate disincorporation and property seizure,
 *   with the Manifesto issued under the direct threat of the church's
 *   complete asset forfeiture and pending before the Supreme Court. Under
 *   this reading the 'revelation' framing is not treated as the operative
 *   cause — the operative cause is the coercive apparatus, and the revelation
 *   narrative is the legitimating cover the church supplies after the fact to
 *   its own members and to history. This is a SIBLING constraint, not a
 *   competing measurement of the same one:
 *   endogenous_reinterpretation_reading treats the same 1890 event as genuine
 *   prophetic revelation (a Rope/coordination story, near-zero suppression as
 *   the operative mechanism), and institutional_pragmatism_reading treats it
 *   as strategic survival adaptation in which doctrine is instrumentalized
 *   (closer to Tangled Rope, with the church itself absorbing much of the
 *   ambiguity). Each reading carries a different epsilon because each
 *   identifies a different operative mechanism producing the same nominal
 *   outcome — that is the point of the decomposition, not an error to
 *   reconcile.
 *
 * KEY AGENTS:
 *   - federal_government: agenda_setter (institutional/analytical) — legislates and escalates until compliance is extracted
 *   - practicing_polygamist_families: payer (powerless/trapped) — bears imprisonment, property loss, forced covenant renunciation
 *   - plural_wives_and_children: payer (powerless/trapped) — bears loss of legal status and family dissolution with no voice
 *   - church_hierarchy_under_duress: payer/agenda_setter (organized/constrained) — issues the Manifesto under existential institutional threat, then enforces it downward
 *   - historians_of_mormon_institutional_change: analytical observer — assesses corroboration independent of the church's own account
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, 0.81).
domain_priors:suppression_score(plural_marriage_mandate__exogenous_override_reading, 0.9).
domain_priors:theater_ratio(plural_marriage_mandate__exogenous_override_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__exogenous_override_reading, snare).
narrative_ontology:human_readable(plural_marriage_mandate__exogenous_override_reading, "1890 Manifesto as Federally Coerced Abandonment of Divine Mandate").
narrative_ontology:topic_domain(plural_marriage_mandate__exogenous_override_reading, "religious institutional history / political theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__exogenous_override_reading, 'dd479a37-fc25-4d68-b6b0-02336994d3a3').
narrative_ontology:cs_kernel_codification('dd479a37-fc25-4d68-b6b0-02336994d3a3', formalized).
narrative_ontology:cs_authority_grounding('dd479a37-fc25-4d68-b6b0-02336994d3a3', extraction).
narrative_ontology:cs_interpretation_layer_present('dd479a37-fc25-4d68-b6b0-02336994d3a3').
narrative_ontology:cs_reading_relation('dd479a37-fc25-4d68-b6b0-02336994d3a3', plural_marriage_mandate__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('dd479a37-fc25-4d68-b6b0-02336994d3a3', plural_marriage_mandate__institutional_pragmatism_reading, influences).
narrative_ontology:cs_axiom('dd479a37-fc25-4d68-b6b0-02336994d3a3', foundational, compliance_extracted_under_coercion_is_not_doctrinal_change).
narrative_ontology:cs_axiom_status(compliance_extracted_under_coercion_is_not_doctrinal_change, holdable).
narrative_ontology:cs_axiom_grounding('dd479a37-fc25-4d68-b6b0-02336994d3a3', compliance_extracted_under_coercion_is_not_doctrinal_change, deontological).
narrative_ontology:cs_axiom('dd479a37-fc25-4d68-b6b0-02336994d3a3', secondary, revelation_narrative_is_post_hoc_legitimation_of_coerced_outcome).
narrative_ontology:cs_axiom_status(revelation_narrative_is_post_hoc_legitimation_of_coerced_outcome, holdable).
narrative_ontology:cs_axiom_grounding('dd479a37-fc25-4d68-b6b0-02336994d3a3', revelation_narrative_is_post_hoc_legitimation_of_coerced_outcome, empirically_contingent).
narrative_ontology:cs_reference_frame('dd479a37-fc25-4d68-b6b0-02336994d3a3', revealed_marriage_law_as_binding_divine_command).
narrative_ontology:cs_drift_state('dd479a37-fc25-4d68-b6b0-02336994d3a3', post_edmunds_tucker_forfeiture_deadline, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('dd479a37-fc25-4d68-b6b0-02336994d3a3', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, anti_polygamy_reform_coalitions).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, practicing_polygamist_families).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, plural_wives_and_children).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, church_hierarchy_under_duress).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__exogenous_override_reading, federal_supremacy_over_territorial_religious_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and escalates anti-polygamy legislation (Morrill, Edmunds, Edmunds-Tucker Acts), disincorporates the church, seizes church property, and imprisons practitioners until the practice is renounced. Sets the terms under which statehood and property restoration become available, converting compliance into the price of institutional survival.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% National reform and women's organizations lobby for and celebrate the escalating federal statutes, achieving their policy goal of eliminating plural marriage as a matter of national uniformity without themselves bearing any of the seizure, imprisonment, or family-dissolution costs.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, anti_polygamy_reform_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Face imprisonment of husbands and fathers, forced separation, property confiscation, and the mandated renunciation of a marriage understood as divinely commanded. Exit means either recanting a sacred covenant or absorbing continued state violence with no legal protection; there is no third option within the territory.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, practicing_polygamist_families, payer,
    powerless, biographical, trapped, regional).

% Bear the practical consequences of disincorporation and prosecution most directly: loss of legal marital status, loss of inheritance and property protections, and social and economic precarity as families are broken apart or driven into hiding. Have no voice in either the federal legislative process or the church's negotiated settlement.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, plural_wives_and_children, payer,
    powerless, biographical, trapped, regional).

% Faces institutional dissolution, total asset seizure, and the imprisonment of its leadership. Issues the 1890 Manifesto under this pressure, then administers compliance internally — occupying both the payer seat (the institution absorbs the loss) and an agenda-setting seat (it enforces the new policy on its own members going forward).
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, church_hierarchy_under_duress, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, church_hierarchy_under_duress, agenda_setter).

% Later generations who continue plural marriage after 1890 and are excommunicated by the reorganized institution. Would testify that the revelation claim was a survival narrative imposed after the fact, but are structurally excluded from both the federal record and the mainstream church's own historiography.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, excommunicated_fundamentalist_remnant, excluded,
    powerless, generational, trapped, regional).

% Examine correspondence, court records, and the timing of the Manifesto relative to the Edmunds-Tucker Act's property seizure deadlines and the pending Supreme Court case (Late Corporation of the Church of Jesus Christ of Latter-Day Saints v. United States) to assess whether the revelation account is corroborated by anything beyond the church's own institutional record.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, historians_of_mormon_institutional_change, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, there is no genuine coordination problem being solved for the payer seats — the arrangement solves a problem for the federal government (eliminating a practice it deemed incompatible with national uniformity) and for the church hierarchy as an institution (survival of corporate existence), not for the families whose marriages were dissolved.
% TRANSFER_FUNCTION: Moves assets (seized church property), liberty (imprisonment of husbands and church leaders), and legal marital status from practicing polygamist families and the church corporate body to the federal government, which recovers territorial conformity and, eventually, a path to statehood for the territory.
% ABSENT_VOICES: Plural wives and children had no standing in the federal legislative process, in the criminal prosecutions of their husbands and fathers, or in the church's internal decision to issue the Manifesto. Their objections — that a covenant they understood as divinely binding was being dissolved by state force — are recorded, if at all, only in private diaries and later fundamentalist testimony, never in the institutional or federal record that produced the settlement.
% DISAPPEARANCE_RATIONALE: Had the federal coercive apparatus (property seizure, disincorporation, mass imprisonment) not existed, the practice would very likely have continued as an internally regulated doctrine; the specific timing of the Manifesto — years after Edmunds-Tucker and immediately preceding the church's total asset forfeiture deadline — indicates the arrangement is downstream of the coercive structure, not independent of it. Remove the coercion and the documentary record gives no independent account of why the change would occur when it did.
% FOUNDING_PROBLEM: The federal government sought to eliminate plural marriage as a matter of national uniformity and as a condition of the Utah Territory's admission to statehood, using escalating criminal and property sanctions until the practicing institution renounced the doctrine.
% FOUNDING_PROBLEM_CORROBORATION: The federal government's own legislative record (Edmunds Act 1882, Edmunds-Tucker Act 1887) and the pending Supreme Court disincorporation case are documented outside the church's own accounts and corroborate that coercive pressure, not internal doctrinal reconsideration, preceded and coincided with the Manifesto's issuance; this corroboration comes from federal statutory and judicial sources, not from the beneficiary church hierarchy's own revelation narrative.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__exogenous_override_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(plural_marriage_mandate__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__exogenous_override_reading, 0.81, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.81 at 1890) and suppression higher still (0.9) because, under this reading, the operative mechanism producing compliance is the federal coercive apparatus, not internal doctrinal persuasion — property was seized, leaders were imprisoned, and the corporate church faced dissolution. Theater ratio rises steeply through the interval (0.10 to 0.72) because, under this reading, the revelation narrative surrounding the Manifesto functions increasingly as performative legitimation of a coerced outcome rather than as the actual mechanism of change; the post-1890 rise in theater_ratio reflects the institution's growing narrative investment in the revelation account even as enforcement pressure (suppression_requirement) recedes once compliance is achieved. All three tracked metrics share one time grid across 1862-1904.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government sits at the beneficiary end: it achieves territorial conformity and clears the path to statehood, and bears none of the extraction itself. Practicing polygamist families and plural wives/children sit at the full-target end: trapped exit options, powerless structural position, and the direct incidence of imprisonment, property loss, and status dissolution. The church hierarchy occupies a genuinely dual position — it pays (asset forfeiture, leadership imprisonment) but also administers the resulting policy internally, which is why it is given both payer and agenda_setter roles rather than being forced into one seat.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading treats the founding problem (federal elimination of plural marriage as a condition of statehood) as dead by 1890 in the sense that compliance was extracted, yet the revelation narrative persists institutionally long after the coercive apparatus that produced it has receded — precisely the founding_problem_status='dead' with disappearance_verdict='world_rearranges' mismatch pattern the R5 consumer is built to flag. Under this reading, that mismatch indicates a capture/legitimation dynamic: the doctrinal story continues to be asserted as the operative cause after the coercive mechanism that this reading identifies as truly operative has already achieved its result.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_causal_status,
    'Was the 1890 revelation an independent causal event that happened to coincide with maximal federal pressure, or was the coercive pressure itself the operative cause with the revelation serving as post-hoc legitimation?',
    'Comparative analysis of the private correspondence and diaries of church leadership in the months preceding the Manifesto against the public revelation narrative, cross-referenced against the Edmunds-Tucker forfeiture timeline and the pending Late Corporation Supreme Court case — looking for whether internal deliberation shows genuine theological wrestling independent of the legal deadline, or shows the deadline driving the decision with theological language supplied afterward.',
    'If internal records show the legal deadline as the dominant driver, this exogenous_override reading is strongly corroborated and the constraint remains a snare. If internal records show substantial independent theological deliberation predating or disconnected from the legal pressure, the endogenous_reinterpretation_reading gains support and this reading''s epsilon would need reassessment as a separate constraint, not a revision of this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_causal_status, empirical, 'Whether coercion or revelation is the operative causal mechanism behind the 1890 Manifesto.').

omega_variable(
    beneficiary_boundary_of_federal_government,
    'Is ''federal_government'' too coarse a beneficiary category — did the actual rents accrue to specific reform coalitions and territorial economic interests (railroad, mining, land speculation) that benefited from statehood and normalized property law, rather than to a generic national interest?',
    'Trace post-statehood land title clearances, railroad grants, and economic interests that specifically required resolution of the disincorporation and property questions the Edmunds-Tucker Act raised.',
    'A narrower, identifiable beneficiary set (specific economic actors rather than ''the federal government'' diffusely) would sharpen the snare classification into a more clearly captured extraction rather than a diffuse public-interest enforcement action.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_boundary_of_federal_government, empirical, 'Whether the beneficiary of federal action was diffuse public interest or concentrated economic actors.').

omega_variable(
    cs_framing_underdetermination,
    'Does the exogenous_override reading treat the church hierarchy purely as a victim-payer, or does its dual agenda_setter/payer role mean part of the apparent ''coercion'' was itself negotiated leverage the hierarchy used to preserve corporate continuity at the direct expense of plural families?',
    'Examine whether church leadership had negotiating channels with federal officials (amnesty negotiations, selective prosecution deals) that a purely coerced victim would not have access to, and whether the costs of compliance were distributed unevenly between leadership (who retained institutional position) and rank-and-file polygamist families (who bore the concrete losses).',
    'If the hierarchy had meaningful negotiating leverage, part of what this reading treats as pure federal-to-church extraction is better modeled as a tangled_rope internal to the church, with the hierarchy also extracting compliance costs from its own rank-and-file members — a distinct downstream constraint, not a revision of this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the church hierarchy''s dual role masks an internal extraction structure distinct from the federal-church relationship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__exogenous_override_reading, 1862, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1862, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1862, 0.1).
narrative_ontology:measurement(plur_tr_t1874, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1874, 0.18).
narrative_ontology:measurement(plur_tr_t1882, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1882, 0.3).
narrative_ontology:measurement(plur_tr_t1887, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1887, 0.45).
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1890, 0.62).
narrative_ontology:measurement(plur_tr_t1896, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1896, 0.68).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1904, 0.72).

% Extraction over time
narrative_ontology:measurement(plur_be_t1862, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1862, 0.35).
narrative_ontology:measurement(plur_be_t1874, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1874, 0.48).
narrative_ontology:measurement(plur_be_t1882, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1882, 0.6).
narrative_ontology:measurement(plur_be_t1887, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1887, 0.78).
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1890, 0.81).
narrative_ontology:measurement(plur_be_t1896, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1896, 0.7).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1904, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1862, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1862, 0.25).
narrative_ontology:measurement(plur_su_t1874, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1874, 0.4).
narrative_ontology:measurement(plur_su_t1882, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1882, 0.62).
narrative_ontology:measurement(plur_su_t1887, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1887, 0.88).
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1890, 0.9).
narrative_ontology:measurement(plur_su_t1896, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1896, 0.55).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1904, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the plural_marriage_mandate kernel. endogenous_reinterpretation_reading treats the same 1890 event as genuine prophetic revelation (near-zero suppression as operative mechanism, Rope-type). institutional_pragmatism_reading treats it as strategic survival adaptation instrumentalizing doctrine (Tangled Rope-type, church absorbing much of the internal ambiguity between coercion and agency). This exogenous_override_reading treats federal coercion as the operative mechanism and the revelation account as legitimating cover, producing a Snare-type classification with a high, actively-enforced suppression value. The three stories share the same documentary record but diverge on operative causal mechanism, and therefore on epsilon, victim/beneficiary sets, and type — this is the ε-invariance decomposition, not three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
