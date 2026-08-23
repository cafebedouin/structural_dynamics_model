% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__christian_canonical_reading, []).

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
 *   constraint_id: marriage_authority_kernel__christian_canonical_reading
 *   human_readable: Christian Canonical Marriage Authority under Indian Christian Marriage Act 1872
 *   domain: legal/religious_governance
 *
 * SUMMARY:
 *   This constraint instantiates the christian_canonical_reading of the
 *   marriage_authority_kernel: the claim that marriage and family law
 *   authority for Indian Christians derives from Christian canonical law as
 *   codified in the Indian Christian Marriage Act 1872. The reading produces
 *   a structurally specific arrangementâsacramental validity adjudicated by
 *   church tribunals, restrictive fault-based divorce grounds, and moderate
 *   gender equity norms embedded in colonial-era statute. It sits within
 *   India's plural personal-law system alongside Hindu, Muslim, Parsi, and
 *   secular civil readings of the same kernel. The colonial codification
 *   layer creates irreducible ambiguity: the Act translates canonical
 *   theology into Anglo-Indian statutory form, making the constraint
 *   simultaneously a religious commitment system and a colonial legal
 *   artifact. The engine will compute per-seat classifications; this story
 *   claims tangled_rope because the arrangement genuinely coordinates
 *   Christian communal marriage practice while asymmetrically extracting from
 *   spousesâespecially womenâthrough restricted exit from irretrievable
 *   marriages.
 *
 * KEY AGENTS:
 *   - church_tribunals: Agenda-setter (institutional/constrained) â adjudicate canonical validity and annulment under the 1872 Act
 *   - indian_church_denominations: Beneficiary (organized/identity_locked) â maintain communal boundary and religious identity through distinct personal law
 *   - christian_women_seeking_divorce: Payer (powerless/constrained) â bear the cost of fault-based barriers to marital exit
 *   - civil_courts_india: Agenda-setter (institutional/mobile) â interpret statutory divorce provisions and periodically override church tribunal claims in tension with constitutional morality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, 0.62).
domain_priors:suppression_score(marriage_authority_kernel__christian_canonical_reading, 0.58).
domain_priors:theater_ratio(marriage_authority_kernel__christian_canonical_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__christian_canonical_reading, "Christian Canonical Marriage Authority under Indian Christian Marriage Act 1872").
narrative_ontology:topic_domain(marriage_authority_kernel__christian_canonical_reading, "legal/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__christian_canonical_reading, '5cb16741-a9f9-491b-94be-f39830196b95').
narrative_ontology:cs_kernel_codification('5cb16741-a9f9-491b-94be-f39830196b95', fixed_text).
narrative_ontology:cs_authority_grounding('5cb16741-a9f9-491b-94be-f39830196b95', lineage).
narrative_ontology:cs_interpretation_layer_present('5cb16741-a9f9-491b-94be-f39830196b95').
narrative_ontology:cs_reading_relation('5cb16741-a9f9-491b-94be-f39830196b95', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('5cb16741-a9f9-491b-94be-f39830196b95', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('5cb16741-a9f9-491b-94be-f39830196b95', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('5cb16741-a9f9-491b-94be-f39830196b95', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('5cb16741-a9f9-491b-94be-f39830196b95', foundational, sacramental_indissolubility_under_canonical_law).
narrative_ontology:cs_axiom_status(sacramental_indissolubility_under_canonical_law, holdable).
narrative_ontology:cs_axiom_grounding('5cb16741-a9f9-491b-94be-f39830196b95', sacramental_indissolubility_under_canonical_law, theological).
narrative_ontology:cs_axiom('5cb16741-a9f9-491b-94be-f39830196b95', foundational, fault_based_divorce_as_necessary_guard).
narrative_ontology:cs_axiom_status(fault_based_divorce_as_necessary_guard, holdable).
narrative_ontology:cs_axiom_grounding('5cb16741-a9f9-491b-94be-f39830196b95', fault_based_divorce_as_necessary_guard, conventional).
narrative_ontology:cs_reference_frame('5cb16741-a9f9-491b-94be-f39830196b95', sacramental_indissolubility_framework).
narrative_ontology:cs_drift_state('5cb16741-a9f9-491b-94be-f39830196b95', contemporary_indian_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5cb16741-a9f9-491b-94be-f39830196b95', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, church_tribunals).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, indian_church_denominations).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_women_seeking_divorce).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate the canonical validity of Christian marriages and grant annulments under sacramental law as recognized by the Indian Christian Marriage Act 1872. Bound by canonical procedure and statutory limits; cannot unilaterally liberalize divorce grounds without doctrinal or legislative change.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, church_tribunals, agenda_setter,
    institutional, generational, constrained, national).

% Derive communal boundary and religious identity from the existence of distinct Christian personal law. The Act marks Christians as a separate legal community in India's plural system; its preservation is tied to denominational claims of distinctiveness and religious freedom.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, indian_church_denominations, beneficiary,
    organized, generational, identity_locked, national).

% Must prove statutory faultâadultery, cruelty, desertion, or conversionâto obtain divorce under the Act. Cannot claim irretrievable breakdown as a ground. Bear prolonged litigation costs, social stigma within the community, and economic vulnerability from delayed exit.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_women_seeking_divorce, payer,
    powerless, biographical, constrained, national).

% District Courts and High Courts exercise statutory jurisdiction over Christian divorce under the 1872 Act. They increasingly interpret its provisions in light of constitutional Articles 14 and 15, occasionally overriding church tribunal claims and narrowing the scope of restrictive canonical interpretation.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, civil_courts_india, agenda_setter,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__christian_canonical_reading, church_tribunals).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates marriage solemnization, registration, and validity adjudication for Indian Christians across diverse denominations under a unified statutory framework derived from canonical principles, preventing inter-denominational legal conflict and preserving communal religious identity.
% TRANSFER_FUNCTION: Transfers jurisdiction over marital validity and dissolution from individual spouses and secular civil processes to church tribunals (annulment) and fault-based civil courts (divorce), transferring the cost of exit disproportionately to women and economically weaker spouses.
% ABSENT_VOICES: Liberal clergy who would support broader annulment grounds; Christian feminists seeking no-fault divorce on irretrievable breakdown; secular uniform-civil-code advocates who reject the category of religious personal law entirely. They are excluded from church tribunal composition and underrepresented in personal-law reform debates dominated by male community leaders.
% DISAPPEARANCE_RATIONALE: If the canonical authority vanished, Christian marriages would migrate to the Special Marriage Act or a uniform civil code; church tribunals would lose jurisdiction, divorce grounds would broaden to include irretrievable breakdown, and the communal boundary maintained by distinct personal law would weaken substantially.
% FOUNDING_PROBLEM: In mid-19th century colonial India, Christian communities of diverse denominations lacked a unified statutory marriage framework recognized by the Anglo-Indian state, producing legal uncertainty over validity, succession, and legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Colonial Legislative Council records and missionary archives attest the original problem of legal uncertainty. Contemporary constitutional-law scholars, the Law Commission of India, and women's rights advocates outside the church hierarchy attest that general civil capacity and constitutional equality guarantees have superseded the original problem; the constraint now persists primarily for communal identity maintenance.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__christian_canonical_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__christian_canonical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__christian_canonical_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the fault-based framework and church tribunal gatekeeping impose high exit costs on spouses in failed marriages, with asymmetric gender impact. Suppression (0.58) is moderate: the Special Marriage Act provides a formal exit, but social and religious costs suppress its use. Theater_ratio (0.30) reflects partial performative maintenanceâchurch tribunals exercise real jurisdiction, yet a growing share of their activity defends sacramental boundary rather than pastoral function. Accessibility_collapse (0.45) is moderate: alternatives exist but are socially costly. Resistance (0.55) is significant, driven by women's groups, civil court constitutional interpretation, and periodic legislative amendment proposals. The metric series span 1872â2022 (t=0 to t=150), tracking colonial enactment, post-independence constitutional pressure, identity-politics resurgence, and contemporary gender-equity contestation.
 *
 * PERSPECTIVAL GAP:
 *   The church tribunal seat computes the constraint as genuine coordination preserving sacramental order and communal identity (low d). The women-seeking-divorce seat computes it as enforced extraction trapping them in failed marriages (high d). Civil courts occupy a middle position: they enforce the statute but have drifted toward constitutional equality norms that erode the canonical reading. The engine derives this divergence from beneficiary/victim declarations and exit modulation.
 *
 * DIRECTIONALITY LOGIC:
 *   Church tribunals and Indian church denominations are declared beneficiaries: they collect authority, jurisdiction, and communal boundary from the constraint, yielding low directionality. Christian women seeking divorce are declared victims: they bear the asymmetric cost of fault-based barriers, yielding high directionality. Civil courts are not declared as beneficiaries or victims; their structural position is agenda-setting with mobile exit options, placing them near symmetric directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâlegal uncertainty for Christian marriage in colonial Indiaâis dead. The constraint persists because it coordinates communal identity (genuine function) and because church institutions extract authority from its continuation. Mandatrophy is not fully resolved because the coordination function (sacramental registration, community boundary) is still live for denominational actors even though the original colonial legal-gap problem is gone. This prevents classification as pure snare: there is real coordination. It prevents classification as rope because the victim set is non-empty and extraction is asymmetric. Tangled rope is the structurally accurate claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colonial_codification_vs_canonical_legitimacy,
    'Does the authority of this constraint derive from authentic Christian canonical law, or from colonial statutory imposition that borrowed canonical language for imperial administrative convenience?',
    'Historical archival analysis of Legislative Council debates and missionary correspondence; theological jurisprudence comparing the Act''s provisions to Roman Catholic and Protestant canonical traditions.',
    'If primarily colonial, the constraint''s naturalized canonical framing is a false summit and extraction is layered onto an imposed legal form; if authentically canonical, the theological grounding strengthens the coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_codification_vs_canonical_legitimacy, conceptual, 'Whether the constraint''s authority is authentically canonical or a colonial legal artifact').

omega_variable(
    gender_equity_axiom_tension,
    'Does the moderate gender equity observed in the Act''s operation resolve the tension with constitutional equality, or does it mask a deeper asymmetry in fault-based divorce access?',
    'Empirical litigation studies comparing success rates, duration, and economic outcomes for Christian men and women in divorce proceedings under the Act.',
    'If asymmetry is severe despite surface equity, effective extraction is higher than the base metric suggests and the constraint leans toward snare; if equity is genuine, the tangled-rope balance shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equity_axiom_tension, empirical, 'Whether moderate gender equity is structural or superficial in fault-based divorce').

omega_variable(
    secular_exit_as_structural_pressure,
    'Does the availability of the Special Marriage Act function as a genuine alternative that reduces extraction, or as a nominal exit that is socially inaccessible to most Indian Christians?',
    'Demographic and sociological data on Christian marriage registration rates under SMA versus ICMA; qualitative studies of social sanctions for opting out of communal marriage law.',
    'If SMA is a real and used exit, suppression and effective extraction are lower than measured; if it is socially inaccessible, the constraint''s suppressive power is higher than the structural measure suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secular_exit_as_structural_pressure, empirical, 'Whether the Special Marriage Act is a practicable exit or a nominal alternative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__christian_canonical_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marriage_auth_christian_tr_t0, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(marriage_auth_christian_tr_t50, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement(marriage_auth_christian_tr_t75, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 75, 0.28).
narrative_ontology:measurement(marriage_auth_christian_tr_t100, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 100, 0.35).
narrative_ontology:measurement(marriage_auth_christian_tr_t125, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 125, 0.32).
narrative_ontology:measurement(marriage_auth_christian_tr_t150, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 150, 0.3).

% Extraction over time
narrative_ontology:measurement(marriage_auth_christian_be_t0, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(marriage_auth_christian_be_t50, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(marriage_auth_christian_be_t75, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 75, 0.55).
narrative_ontology:measurement(marriage_auth_christian_be_t100, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 100, 0.58).
narrative_ontology:measurement(marriage_auth_christian_be_t125, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 125, 0.6).
narrative_ontology:measurement(marriage_auth_christian_be_t150, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 150, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(marriage_auth_christian_su_t0, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(marriage_auth_christian_su_t50, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(marriage_auth_christian_su_t75, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 75, 0.45).
narrative_ontology:measurement(marriage_auth_christian_su_t100, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 100, 0.48).
narrative_ontology:measurement(marriage_auth_christian_su_t125, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 125, 0.5).
narrative_ontology:measurement(marriage_auth_christian_su_t150, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 150, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__christian_canonical_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_authority_kernel, decomposed from the plural personal-law system of India. The christian_canonical_reading is structurally distinct from its siblings because it grounds authority in sacramental canonical law codified under colonial statute, producing fault-based divorce and church tribunal jurisdiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
