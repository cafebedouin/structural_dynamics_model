% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__icrc_customary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__icrc_customary_reading, []).

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
 *   constraint_id: common_article_3_scope__icrc_customary_reading
 *   human_readable: Common Article 3 Scope Determined by Evolving Customary Practice (ICRC Reading)
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   A procedural method governs the reach of Common Article 3's minimum
 *   guarantees in internal armed conflict: rather than a fixed qualification
 *   threshold, applicability is determined by surveying evolving state
 *   practice and opinio juris, compiled and weighed principally by the ICRC's
 *   study apparatus and cited by criminal chambers. The method lets
 *   protections extend to new forms of internal violence without reopening
 *   treaty text, and it concentrates interpretive labor — and institutional
 *   standing — in the compiling bodies. Governments that prefer fixed
 *   thresholds absorb expanding obligations they did not author; militaries
 *   whose conduct supplies the record's raw material see their existing
 *   preferences consolidated into general law. Claim and metrics are authored
 *   independently: the story claims tangled_rope as the structurally true
 *   description (a working coordination method carrying asymmetric, actively
 *   maintained costs), while the metric values describe the arrangement's
 *   actual operation at interval end.
 *
 * KEY AGENTS:
 *   - icrc_customary_tracking_programme: agenda-setting compiler (institutional/identity_locked) — produces the practice record and convenes its weighing
 *   - international_criminal_tribunals: beneficiary and secondary agenda-setter (institutional/constrained) — cites the record and feeds judgments back into it
 *   - like_minded_practice_leading_states: beneficiary (organized/arbitrage) — authors the practice that consolidates into general law
 *   - scope_resistant_states: primary target among states (powerful/constrained) — absorbs obligations it did not author
 *   - non_state_armed_groups: primary target without voice (organized/trapped) — bound by conclusions it cannot shape
 *   - protected_persons_in_internal_conflicts: end beneficiary (powerless/trapped)
 *   - small_states_without_operational_militaries: excluded contributor (moderate/trapped)
 *   - academic_ihl_commentators: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.6).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.47).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "Common Article 3 Scope Determined by Evolving Customary Practice (ICRC Reading)").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(common_article_3_scope__icrc_customary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, 'bca4ea4a-2a9c-439c-a41a-b18909c1f023').
narrative_ontology:cs_kernel_codification('bca4ea4a-2a9c-439c-a41a-b18909c1f023', fixed_text).
narrative_ontology:cs_authority_grounding('bca4ea4a-2a9c-439c-a41a-b18909c1f023', practice).
narrative_ontology:cs_interpretation_layer_present('bca4ea4a-2a9c-439c-a41a-b18909c1f023').
narrative_ontology:cs_reading_relation('bca4ea4a-2a9c-439c-a41a-b18909c1f023', common_article_3_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('bca4ea4a-2a9c-439c-a41a-b18909c1f023', common_article_3_scope__expansive_human_rights_reading, coexists_with).
narrative_ontology:cs_axiom('bca4ea4a-2a9c-439c-a41a-b18909c1f023', foundational, custom_tracks_evolving_state_practice).
narrative_ontology:cs_axiom_status(custom_tracks_evolving_state_practice, holdable).
narrative_ontology:cs_axiom_grounding('bca4ea4a-2a9c-439c-a41a-b18909c1f023', custom_tracks_evolving_state_practice, conventional).
narrative_ontology:cs_axiom('bca4ea4a-2a9c-439c-a41a-b18909c1f023', secondary, gradual_expansion_without_treaty_amendment_is_legitimate).
narrative_ontology:cs_axiom_status(gradual_expansion_without_treaty_amendment_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('bca4ea4a-2a9c-439c-a41a-b18909c1f023', gradual_expansion_without_treaty_amendment_is_legitimate, instrumental).
narrative_ontology:cs_reference_frame('bca4ea4a-2a9c-439c-a41a-b18909c1f023', living_customary_minimum_yardstick).
narrative_ontology:cs_drift_state('bca4ea4a-2a9c-439c-a41a-b18909c1f023', contemporary, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('bca4ea4a-2a9c-439c-a41a-b18909c1f023', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, protected_persons_in_internal_conflicts).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, icrc_customary_tracking_programme).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, international_criminal_tribunals).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, like_minded_practice_leading_states).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, scope_resistant_states).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, non_state_armed_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compiles national legislation, military manuals, judicial decisions, and official statements into periodic studies and a public database of humanitarian-law practice; convenes expert meetings; issues updated assessments of which rules bind in internal conflicts. Its advisory services to armed forces and its standing in diplomatic conferences grow with each edition. Stepping back from the tracking role would hollow out the institution's core mandate, which has been built around custodianship of these materials for decades.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, icrc_customary_tracking_programme, agenda_setter,
    institutional, generational, identity_locked, global).

% Ad hoc chambers and the permanent criminal court ground charges for conduct in internal wars in rules they identify as customary, citing practice compilations and prior judgments. Each judgment enters the record as practice for future assessments. Their dockets depend on reaching conduct in conflicts that treaty texts alone address unevenly; a chamber cannot decline the method without narrowing its own reach.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, international_criminal_tribunals, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__icrc_customary_reading, international_criminal_tribunals, agenda_setter).

% Militarily active states whose field manuals, rules-of-engagement documents, and legislative implementations supply much of the raw material assessed as practice. Because their own doctrines enter the record at the drafting stage, the obligations that emerge rarely diverge from what they already intended to do; several fund and host the expert processes that weigh the evidence.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, like_minded_practice_leading_states, beneficiary,
    organized, generational, arbitrage, global).

% Governments that insist internal violence below defined intensity and organization levels stays outside the humanitarian-law frame. They file reservations, publish rebuttals to practice studies, and assert persistent objection when tribunals cite rules they rejected. Each new compilation that counts contrary practice raises their compliance and litigation exposure; maintaining the objection position consumes diplomatic capital year after year.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, scope_resistant_states, payer,
    powerful, biographical, constrained, global).

% Insurgent and rebel formations bound by whatever minimum rules the prevailing assessment says attach to their conflicts. They sign special agreements or unilateral declarations when pressed, but they have no seat in the diplomatic and expert processes where opinio juris is weighed; their conduct is recorded almost exclusively as violation rather than as contribution to the record.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, non_state_armed_groups, payer,
    organized, biographical, trapped, regional).

% Detainees, wounded fighters hors de combat, and civilians in territories controlled by parties to internal wars. When the prevailing assessment recognizes a rule as applying to a conflict, protections reach them through detention procedures, access requirements, and prosecutable prohibitions. They cannot organize, petition, or relocate out of the conflict; what reaches them arrives through intermediaries — delegations, courts, monitors.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, protected_persons_in_internal_conflicts, beneficiary,
    powerless, immediate, trapped, regional).

% Governments with no expeditionary forces and thin legal-adviser cadres. They generate almost no entries in the practice record and attend expert processes sporadically, yet the conclusions reached bind them equally. Their written objections, when filed, carry little evidentiary weight against the operational practice of larger militaries.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, small_states_without_operational_militaries, excluded,
    moderate, generational, trapped, national).

% Professors, think-tank researchers, and journal editors who audit how practice is selected and weighed, publishing methodology critiques and alternative compilations. Several advised the major studies; others documented the objections of skeptical governments. Their leverage runs through citation by chambers and delegations rather than through any decision seat.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, academic_ihl_commentators, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__icrc_customary_reading, icrc_customary_tracking_programme).
narrative_ontology:fixing_cost_class(common_article_3_scope__icrc_customary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives every party to an internal conflict — governments, armed groups, courts, humanitarian agencies — a single, continuously updated reference for which minimum protections attach, replacing case-by-case political classification fights with a shared evidentiary method.
% TRANSFER_FUNCTION: Moves obligation outward with each accepted expansion: compliance burdens shift onto parties that did not author the underlying practice, while protection and adjudicative reach move toward persons and prosecutors in conflicts previously outside the frame; interpretive authority accrues to the institutions that compile and weigh the record.
% ABSENT_VOICES: Armed groups bound by the resulting scope have no seat where opinio juris is weighed; small states without operational militaries generate no practice and are bound by conclusions drawn from others' conduct; residents of conflicts whose governments deny applicability appear mainly through intermediary reports, if at all.
% DISAPPEARANCE_RATIONALE: If the practice-tracking method vanished overnight, scope determination would revert to the bare 1949 text and to whichever classification threshold each government preferred; protections recognized since then would need fresh treaty negotiation to survive; chambers would lose the citation base under most internal-conflict prosecutions; and the compiling institutions would lose their coordinating function.
% FOUNDING_PROBLEM: The 1949 Conventions left undefined when an internal conflict qualifies for Common Article 3's minimum guarantees, leaving applicability to ad hoc political recognition that varied by government and by war.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the compiling institutions: the Tadic appeals-chamber analysis identified the classification problem as the central defect of the treaty frame; successive government delegations — including those hostile to expanded scope — continue to litigate where the line sits, attesting that the problem persists; and the continued existence of rival scope methodologies in mainstream journals attests it from the academy.
narrative_ontology:disappearance_verdict(common_article_3_scope__icrc_customary_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__icrc_customary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__icrc_customary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_article_3_scope__icrc_customary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__icrc_customary_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__icrc_customary_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__icrc_customary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.60 reflects obligations that expand past the consent of the parties bearing them: resistant governments and armed groups comply with rules whose authoritative statement they never joined, while the seats that author the record face little surprise. Suppression 0.47 is the coercive force needed to hold the method against threshold-insisting opposition — reputational pressure, citation practice, convening and funding gatekeeping — short of formal sanction; it is authored as a raw structural property and is deliberately left unscaled here (only extractiveness is scaled, by the engine, through directionality and scope). Theater_ratio 0.34: the core function is real — chambers decide cases with the record — but a growing fraction of activity is aspirational compilation that counts statements of intent as practice. Accessibility_collapse 0.42: rival scope methodologies remain arguable in principle, so alternatives are narrowed, not closed. Resistance 0.55: sustained governmental pushback — published rebuttals, persistent-objection assertions, funding fights — meets the method at every expansion. All three temporal series share one nine-point grid (1949–2026); no metric is sampled on a private schedule. Coalition note: the two targeted seats could in principle combine — joint governmental rebuttals to the major studies have occurred — but divergent interests between great-power objectors and armed groups, and the groups' lack of any forum, keep coalition power weak.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the compiling seat the method is careful, incremental law-building it has staffed for seventy years; from the resistant-government seat it is obligation arriving without signature; from the armed-group seat it is rules announced by processes it cannot enter; from the protected-person seat only outcomes register — a rule either reaches the detention site or it does not, and the method that produced it is invisible. Like-minded militaries occupy a further position: governed in form, authorial in fact. The engine derives these per-seat classifications from the declared roles, exits, and horizons; nothing in the claimed type adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Protected persons sit at the beneficiary pole: everything the method produces flows toward them and nothing is taken from them. The compiling institution holds a beneficiary-plus-agenda-setter position with identity-fused exit — its standing grows with each expansion, so its derived directionality sits deep on the subsidized side. Chambers similarly collect jurisdictional reach. Like-minded practice-leading states are declared beneficiaries because the record is made of their conduct: the derivation reads their beneficiary declaration rather than their formal subjection to the rules. Resistant states are declared victims with constrained exit — persistent objection exists but is costly — placing them near the target pole. Armed groups are victims with trapped exit and no voice, the deepest target position in the story. Small states, excluded from the conversation but bound by its output, derive near-target directionality despite holding no formal role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — when do minimum guarantees attach to internal violence — is live, so the arrangement has not outlived its mandate and no mandatrophy is declared. The classification still earns its keep by blocking two mislabels: reading the method as pure coordination ignores that its costs land on seats that never signed the underlying practice; reading it as pure extraction ignores that it solves a real classification problem every party would otherwise fight politically, war by war. The theater_ratio series is the early-warning instrument: if compilation becomes self-referential — counting assertions of intent as settled law faster than conduct confirms them — the method's function atrophies while its apparatus persists, and the story should be re-examined for inertial maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This story instantiates the icrc_customary_reading of kernel common_article_3_scope; what structural changes follow if a sibling reading displaces it?',
    'Author and classify the sibling stories separately: the threshold-based sibling freezes scope at intensity and organization levels, dissolving the practice-tracking function this story prices; the unconditional-floor sibling detaches scope from consent entirely, converting this method''s gradualism into open imposition.',
    'Under the threshold sibling, most measured extraction disappears along with the tracking function; under the floor sibling, extraction rises sharply because consent-bypass becomes explicit. This story''s classification is valid only for its own reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Reading-indexed classification: sibling readings of the CA3-scope kernel instantiate different constraints with different epsilon.').

omega_variable(
    practice_record_selection_bias,
    'Does the compiled practice record represent world state practice, or a subset weighted toward like-minded militaries, NGO documentation, and dominant-language sources?',
    'Independent replication of the major studies with pre-registered inclusion criteria, dissent-weighted counting, and multilingual source coverage.',
    'If curated, the obligations consolidated on resistant seats rest on selective evidence and effective extraction exceeds the authored measure; if representative, the measure stands as genuine convergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practice_record_selection_bias, empirical, 'Selection effects in the evidentiary base of customary assessment.').

omega_variable(
    opinio_juris_conviction_share,
    'How much of the opinio juris record reflects considered legal conviction rather than aspirational or diplomatic positioning?',
    'Longitudinal comparison of stated positions against subsequent conduct: governments whose later practice contradicts their asserted conviction reveal the aspirational share.',
    'A high aspirational share inflates the apparent expansion rate, front-loading obligations the record cannot support; a low share validates the method''s pace.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(opinio_juris_conviction_share, conceptual, 'Evidentiary weight of asserted legal conviction in the practice record.').

omega_variable(
    compiler_identity_fusion,
    'Is the compiling institution''s persistence driven by continuing demand for the function, or by institutional identity — has the organization become its custodial role?',
    'Counterfactual commissioning test: whether equivalent compilation would be entrusted to an independent body if offered; and continuity of method across leadership turnover.',
    'If identity-driven, performative maintenance is understated by the theater ratio and the arrangement drifts toward inertial persistence should the function ever atrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compiler_identity_fusion, empirical, 'Institutional identity lock on the practice-compiling seat.').

omega_variable(
    persistent_objection_viability,
    'Is persistent objection a functioning exit for resistant governments, or a dead letter that binds them more tightly than a merely limited exit implies?',
    'Survey post-1977 cases: has any government asserting persistent objection actually escaped an obligation the record consolidated against its will?',
    'If dead-letter, resistant seats are effectively trapped, deepening their target-side position and raising effective extraction above the authored measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistent_objection_viability, empirical, 'Viability of the persistent-objector exit for scope-resistant states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 1949, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__icrc_customary_reading, theater_ratio, 1949, 0.05).
narrative_ontology:measurement(comm_tr_t1965, common_article_3_scope__icrc_customary_reading, theater_ratio, 1965, 0.07).
narrative_ontology:measurement(comm_tr_t1977, common_article_3_scope__icrc_customary_reading, theater_ratio, 1977, 0.1).
narrative_ontology:measurement(comm_tr_t1990, common_article_3_scope__icrc_customary_reading, theater_ratio, 1990, 0.14).
narrative_ontology:measurement(comm_tr_t1999, common_article_3_scope__icrc_customary_reading, theater_ratio, 1999, 0.2).
narrative_ontology:measurement(comm_tr_t2005, common_article_3_scope__icrc_customary_reading, theater_ratio, 2005, 0.26).
narrative_ontology:measurement(comm_tr_t2012, common_article_3_scope__icrc_customary_reading, theater_ratio, 2012, 0.3).
narrative_ontology:measurement(comm_tr_t2019, common_article_3_scope__icrc_customary_reading, theater_ratio, 2019, 0.32).
narrative_ontology:measurement(comm_tr_t2026, common_article_3_scope__icrc_customary_reading, theater_ratio, 2026, 0.34).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1949, 0.15).
narrative_ontology:measurement(comm_be_t1965, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1965, 0.18).
narrative_ontology:measurement(comm_be_t1977, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1977, 0.28).
narrative_ontology:measurement(comm_be_t1990, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1990, 0.33).
narrative_ontology:measurement(comm_be_t1999, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1999, 0.45).
narrative_ontology:measurement(comm_be_t2005, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2005, 0.52).
narrative_ontology:measurement(comm_be_t2012, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2012, 0.56).
narrative_ontology:measurement(comm_be_t2019, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2019, 0.58).
narrative_ontology:measurement(comm_be_t2026, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2026, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1949, 0.08).
narrative_ontology:measurement(comm_su_t1965, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1965, 0.1).
narrative_ontology:measurement(comm_su_t1977, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1977, 0.18).
narrative_ontology:measurement(comm_su_t1990, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1990, 0.26).
narrative_ontology:measurement(comm_su_t1999, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1999, 0.33).
narrative_ontology:measurement(comm_su_t2005, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(comm_su_t2012, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2012, 0.44).
narrative_ontology:measurement(comm_su_t2019, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2019, 0.46).
narrative_ontology:measurement(comm_su_t2026, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2026, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__icrc_customary_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, expansive_human_rights_reading).

% DUAL FORMULATION NOTE:
% Family member of the common_article_3_scope kernel decomposition: the colloquial label 'Common Article 3 scope' covers three structurally distinct claims about what fixes applicability — fixed thresholds (state_centric_reading), evolving custom (this story), and an unconditional floor (expansive_human_rights_reading). Each carries its own epsilon, beneficiary structure, and classification; this reading's epsilon prices the practice-tracking method itself, not the scope outcomes the siblings would produce. Structurally, this reading influences both siblings because its compilations supply the evidentiary terrain they argue over, while neither sibling is logically eliminated by it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
