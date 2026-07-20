% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__parsi_communal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__parsi_communal_reading, []).

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
 *   constraint_id: marriage_authority_kernel__parsi_communal_reading
 *   human_readable: Parsi Communal Marriage Authority (PMDA 1936 Reading)
 *   domain: comparative_law/constitutional_pluralism
 *
 * SUMMARY:
 *   This constraint instantiates the parsi_communal_reading of the
 *   marriage_authority_kernel. In Indian constitutional pluralism, marriage
 *   and family law authority is distributed across religious communities. The
 *   Parsi Marriage and Divorce Act 1936 delegates authority to Parsi-specific
 *   matrimonial courts and community tribunals, codifying a custom-derived
 *   endogamy norm. The arrangement is presented as preserving a
 *   demographically threatened minority; it simultaneously restricts
 *   individual marriage choice through active enforcement of communal
 *   boundaries. Structural deltas from sibling readings: use of community
 *   tribunals rather than civil courts (Hindu), qazis (Muslim), or canonical
 *   registrars (Christian); explicit statutory endogamy enforcement;
 *   relatively gender-equitable internal provisions; acute
 *   demographic-decline pressure that intensifies the coordination-extraction
 *   tension.
 *
 * KEY AGENTS:
 *   - parsi_matrimonial_judiciary: Agenda-setter (institutional/constrained) â administers the Act and enforces community-derived norms through special courts.
 *   - parsi_community_councils: Agenda-setter (organized/constrained) â regulate membership and endogamy through communal institutions.
 *   - parsi_endogamous_families: Beneficiary (moderate/identity_locked) â their social standing and community identity are preserved by the constraint.
 *   - parsi_exogamy_seekers: Payer (powerless/identity_locked) â bear the cost of marriage restrictions and risk excommunication.
 *   - uniform_civil_code_advocates: Excluded (organized/mobile) â would replace communal authority with secular civil law but are not in the room.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, 0.62).
domain_priors:suppression_score(marriage_authority_kernel__parsi_communal_reading, 0.48).
domain_priors:theater_ratio(marriage_authority_kernel__parsi_communal_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__parsi_communal_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__parsi_communal_reading, "Parsi Communal Marriage Authority (PMDA 1936 Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__parsi_communal_reading, "comparative_law/constitutional_pluralism").

domain_priors:requires_active_enforcement(marriage_authority_kernel__parsi_communal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__parsi_communal_reading, 'd7455737-88a8-45a2-9f55-a897afe452a3').
narrative_ontology:cs_kernel_codification('d7455737-88a8-45a2-9f55-a897afe452a3', fixed_text).
narrative_ontology:cs_authority_grounding('d7455737-88a8-45a2-9f55-a897afe452a3', lineage).
narrative_ontology:cs_interpretation_layer_present('d7455737-88a8-45a2-9f55-a897afe452a3').
narrative_ontology:cs_reading_relation('d7455737-88a8-45a2-9f55-a897afe452a3', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7455737-88a8-45a2-9f55-a897afe452a3', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7455737-88a8-45a2-9f55-a897afe452a3', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7455737-88a8-45a2-9f55-a897afe452a3', marriage_authority_kernel__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('d7455737-88a8-45a2-9f55-a897afe452a3', foundational, zoroastrian_endogamy_mandate).
narrative_ontology:cs_axiom_status(zoroastrian_endogamy_mandate, holdable).
narrative_ontology:cs_axiom_grounding('d7455737-88a8-45a2-9f55-a897afe452a3', zoroastrian_endogamy_mandate, conventional).
narrative_ontology:cs_axiom('d7455737-88a8-45a2-9f55-a897afe452a3', foundational, parsi_tribunal_jurisdiction).
narrative_ontology:cs_axiom_status(parsi_tribunal_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('d7455737-88a8-45a2-9f55-a897afe452a3', parsi_tribunal_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('d7455737-88a8-45a2-9f55-a897afe452a3', parsi_custom_codified_1936).
narrative_ontology:cs_drift_state('d7455737-88a8-45a2-9f55-a897afe452a3', contemporary_demographic_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d7455737-88a8-45a2-9f55-a897afe452a3', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_matrimonial_judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_community_councils).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_endogamous_families).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_exogamy_seekers).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__parsi_communal_reading, parsi_personal_law_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Special courts established under the 1936 Act, staffed by Parsi delegates, who hear matrimonial suits, grant divorces, and award alimony according to community custom and statutory procedure. Their jurisdiction is distinct from general civil courts and depends on the parties being Parsi.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_matrimonial_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Parsi Panchayats and community associations that certify religious identity, regulate admission to the faith, and exert social pressure to maintain endogamy. They influence court proceedings and control access to religious burial grounds and community trusts.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_community_councils, agenda_setter,
    organized, generational, constrained, national).

% Households that marry within the community and derive social status, religious legitimacy, and inheritance security from the Act's preservation of communal boundaries. Their children are recognized as Parsi without question and inherit community benefits.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_endogamous_families, beneficiary,
    moderate, generational, identity_locked, national).

% Parsi individuals who form relationships with non-Parsis. They face statutory barriers to having their marriages recognized under the Act, risk losing community membership and burial rights for themselves and their children, and must choose between personal relationships and communal identity.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_exogamy_seekers, payer,
    powerless, biographical, identity_locked, national).

% Legal reformers and activists who argue that religious personal laws should be replaced by a secular uniform civil code guaranteeing individual equality. They are not represented in Parsi matrimonial courts and their arguments are treated as external threats to minority autonomy.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, uniform_civil_code_advocates, excluded,
    organized, generational, mobile, national).

narrative_ontology:fixing_cost_class(marriage_authority_kernel__parsi_communal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a community-specific forum for matrimonial disputes and enforces endogamy to prevent the demographic dissolution of a tiny religious minority within India's plural legal order.
% TRANSFER_FUNCTION: Moves jurisdictional authority over Parsi marriage and divorce from general civil courts to Parsi-specialized tribunals, and moves individual marriage-choice autonomy to community-endorsed endogamy norms backed by state power.
% ABSENT_VOICES: Parsi youth seeking interfaith marriage without excommunication, and secular advocates of a uniform civil code, are structurally absent from tribunals and community councils.
% DISAPPEARANCE_RATIONALE: Without this authority, Parsi matrimonial disputes would enter general civil courts or the Special Marriage Act, endogamy norms would lose statutory enforcement, community boundary maintenance would collapse, and the already-declining population would likely assimilate rapidly.
% FOUNDING_PROBLEM: British colonial and early-independent India needed to accommodate the Parsi Zoroastrian community's distinct identity and provide a matrimonial forum suited to their customs, given their tiny demographic footprint and commercial prominence.
% FOUNDING_PROBLEM_CORROBORATION: Community institutions and demographic historians attest the extinction-risk problem remains live. Social scientists and some Parsi reformers attest the enforcement strategy has become counterproductive; the corroboration is split, with no fully external neutral party.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__parsi_communal_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__parsi_communal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__parsi_communal_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__parsi_communal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__parsi_communal_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__parsi_communal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__parsi_communal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the Act structurally restricts marriage to intra-community unions, imposing severe identity costs on exogamy. Suppression (0.48) reflects the active machinery of special courts and social ostracism. Theater ratio (0.38) captures the growing performative dimension: as the population shrinks, maintenance of elaborate matrimonial courts and endogamy norms outstrips the community's demographic weight. Accessibility collapse (0.42) is partial because the Special Marriage Act offers a legal alternative, but at the price of communal excommunication. Resistance (0.32) is modest but rising among youth and reformers. The metric profile supports tangled_rope: genuine coordination (preservation of identity and dispute-resolution forum) coexists with asymmetric extraction (autonomy restriction on exogamy-seekers).
 *
 * PERSPECTIVAL GAP:
 *   The Parsi matrimonial judiciary and community councils experience the constraint as necessary survival infrastructure for a dying community; their seat computes toward coordination. The exogamy-seeker experiences the same structure as an identity-locked barrier forcing a choice between love and community; their seat computes toward extraction. The endogamous family sits in between, deriving benefit from the boundary they do not personally enforce. The engine produces divergent per-seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary and councils are structural beneficiaries and agenda-setters: they hold jurisdictional authority and derive institutional legitimacy from the constraint (low d). Endogamous families are beneficiaries: they receive identity security and communal continuity (low-mid d). Exogamy seekers are victims: they bear the direct cost of marriage restrictions and identity loss (high d). Uniform civil code advocates are excluded from the arrangement entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â colonial and post-colonial accommodation of a tiny religious minority â is contested. If the problem were clearly live, the constraint might read as a rope or scaffold; if clearly dead, as a snare or piton. The demographic-decline reality keeps the coordination function genuinely alive for beneficiaries, while the autonomy restriction keeps extraction live for victims. Tangled_rope is the only category that respects both facts simultaneously, preventing the false dichotomy of labeling minority legal autonomy as either pure cultural preservation or pure oppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the authority derive genuinely from immutable community custom, or is the 1936 codification a colonial and post-colonial construction that freeze-framed a particular version of custom?',
    'Historical archival analysis of pre-1936 Parsi matrimonial practices compared to the Act''s provisions, and examination of legislative-debate records for contested provisions.',
    'If the codification substantially invented rather than recorded custom, the authority grounding shifts from lineage to extraction or conventional, altering the drift-state classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the 1936 Act reflects discovered custom or constructed authority.').

omega_variable(
    endogamy_enforcement_efficacy,
    'Does statutory endogamy enforcement actually preserve Parsi community identity, or does it accelerate demographic decline by excluding partial-descent children and alienating youth?',
    'Longitudinal demographic study comparing Parsi population trends under strict endogamy regimes versus liberalized membership rules, controlling for migration and fertility.',
    'If endogamy accelerates decline, the coordination story is undermined and the constraint reads more as inertial performance (piton-ward); if it genuinely preserves identity, the coordination function is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_enforcement_efficacy, empirical, 'Whether endogamy preserves or harms demographic viability.').

omega_variable(
    exogamy_barriers_structural_or_social,
    'Are barriers to exogamy primarily structural (loss of legal standing, statutory invalidity) or internalized (fear of ostracism, religious shame)?',
    'Comparative analysis of Parsi individuals marrying under Special Marriage Act versus PMDA to measure rates of community excommunication and psychological outcomes.',
    'If primarily internalized, effective extraction exceeds the structural measure; if structural, the suppression metric is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exogamy_barriers_structural_or_social, empirical, 'Structural versus internalized suppression mechanism in endogamy enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__parsi_communal_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(marr_tr_t15, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(marr_tr_t30, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(marr_tr_t45, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 45, 0.26).
narrative_ontology:measurement(marr_tr_t60, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement(marr_tr_t75, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 75, 0.38).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(marr_be_t15, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(marr_be_t30, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(marr_be_t45, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 45, 0.52).
narrative_ontology:measurement(marr_be_t60, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(marr_be_t75, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 75, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(marr_su_t15, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(marr_su_t30, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(marr_su_t45, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 45, 0.44).
narrative_ontology:measurement(marr_su_t60, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 60, 0.47).
narrative_ontology:measurement(marr_su_t75, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 75, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_authority_kernel; sibling readings instantiate other communal and secular sources of marriage-law authority in India.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
