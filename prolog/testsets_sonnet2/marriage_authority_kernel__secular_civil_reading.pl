% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__secular_civil_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__secular_civil_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: marriage_authority_kernel__secular_civil_reading
 *   human_readable: Special Marriage Act 1954 — Secular Civil Marriage Authority
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   India's marriage and family law operates as a plural system: five
 *   parallel legal tracks (Hindu, Muslim, Christian, Parsi, and secular
 *   civil) each derive marriage authority from a different source, and
 *   individuals largely fall under whichever track corresponds to their
 *   religious identity unless they affirmatively opt into the secular civil
 *   track under the Special Marriage Act. This story concerns only the
 *   secular civil track. It functions as both an escape valve from community
 *   personal law and a genuine coordination solution for couples the
 *   community systems cannot marry at all (inter-faith couples). Its
 *   extraction is real but of a different character than the community-law
 *   readings: rather than extracting from one religious community's members
 *   on behalf of that community's authorities, it extracts social costs from
 *   individuals who exercise the exit option, imposed informally by the
 *   communities they leave rather than formally by the civil apparatus
 *   itself.
 *
 * KEY AGENTS:
 *   - civil_court_system: institutional agenda-setter administering the parallel secular track
 *   - inter_religious_couples: primary beneficiaries of a marriage path requiring no conversion
 *   - women_seeking_gender_equal_divorce_terms: beneficiaries of statutory gender parity unavailable in some community regimes
 *   - couples_exiting_community_personal_law: payers of the 30-day notice period and loss of community adjudicative access
 *   - women_facing_community_ostracism_after_civil_marriage: payers of informal social costs the statute cannot reach
 *   - religious_authorities_losing_adjudicative_jurisdiction: payers of cumulative jurisdictional erosion, excluded from civil proceedings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, 0.42).
domain_priors:suppression_score(marriage_authority_kernel__secular_civil_reading, 0.38).
domain_priors:theater_ratio(marriage_authority_kernel__secular_civil_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__secular_civil_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__secular_civil_reading, "Special Marriage Act 1954 — Secular Civil Marriage Authority").
narrative_ontology:topic_domain(marriage_authority_kernel__secular_civil_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__secular_civil_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__secular_civil_reading, 'c205baf2-a314-4703-b4f4-b924fb509fd5').
narrative_ontology:cs_kernel_codification('c205baf2-a314-4703-b4f4-b924fb509fd5', formalized).
narrative_ontology:cs_authority_grounding('c205baf2-a314-4703-b4f4-b924fb509fd5', expertise).
narrative_ontology:cs_interpretation_layer_present('c205baf2-a314-4703-b4f4-b924fb509fd5').
narrative_ontology:cs_reading_relation('c205baf2-a314-4703-b4f4-b924fb509fd5', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('c205baf2-a314-4703-b4f4-b924fb509fd5', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('c205baf2-a314-4703-b4f4-b924fb509fd5', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('c205baf2-a314-4703-b4f4-b924fb509fd5', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_axiom('c205baf2-a314-4703-b4f4-b924fb509fd5', foundational, marriage_authority_grounded_in_individual_constitutional_rights).
narrative_ontology:cs_axiom_status(marriage_authority_grounded_in_individual_constitutional_rights, holdable).
narrative_ontology:cs_axiom_grounding('c205baf2-a314-4703-b4f4-b924fb509fd5', marriage_authority_grounded_in_individual_constitutional_rights, deontological).
narrative_ontology:cs_axiom('c205baf2-a314-4703-b4f4-b924fb509fd5', foundational, civil_court_jurisdiction_available_regardless_of_religious_identity).
narrative_ontology:cs_axiom_status(civil_court_jurisdiction_available_regardless_of_religious_identity, holdable).
narrative_ontology:cs_axiom_grounding('c205baf2-a314-4703-b4f4-b924fb509fd5', civil_court_jurisdiction_available_regardless_of_religious_identity, conventional).
narrative_ontology:cs_reference_frame('c205baf2-a314-4703-b4f4-b924fb509fd5', constitutional_individual_rights_supremacy).
narrative_ontology:cs_drift_state('c205baf2-a314-4703-b4f4-b924fb509fd5', contemporary_uniform_civil_code_debate, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('c205baf2-a314-4703-b4f4-b924fb509fd5', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, inter_religious_couples).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, women_seeking_gender_equal_divorce_terms).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, civil_court_system).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, constitutional_rights_framework).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, couples_exiting_community_personal_law).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, women_facing_community_ostracism_after_civil_marriage).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, religious_authorities_losing_adjudicative_jurisdiction).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, constitutional_supremacy_over_personal_law).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, individual_rights_as_marriage_law_foundation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the Special Marriage Act as a parallel secular track: registers marriages, adjudicates divorce and succession, and applies uniform procedural standards regardless of the parties' religion. Its authority is grounded in constitutional individual-rights doctrine rather than any single community's tradition. It benefits institutionally from being the forum of last resort when community-law forums are unavailable or contested.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, civil_court_system, agenda_setter,
    institutional, civilizational, analytical, national).

% Cannot marry validly under any single community's personal law without one party formally converting. The Act gives them a registration path that does not require conversion or renunciation of either faith. This is the paradigm coordination function the Act exists to solve.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, inter_religious_couples, beneficiary,
    moderate, biographical, mobile, national).

% Under several community personal-law regimes, divorce grounds, maintenance, and custody defaults are gender-asymmetric. Marrying or transferring to the civil track under the Act (or invoking its succession provisions) gives access to more gender-symmetric statutory terms, at the cost of navigating an unfamiliar forum and losing community-recognized status.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, women_seeking_gender_equal_divorce_terms, beneficiary,
    moderate, biographical, constrained, national).

% Choosing the civil track triggers a mandatory 30-day public notice period historically used by families and community members to identify and pressure couples, particularly in inter-caste or inter-religious unions. They also lose automatic standing before community adjudicative bodies (temple committees, jamaat panchayats, church tribunals) and any social insurance those bodies provided, without a guaranteed equivalent replacement in the civil forum's informal support.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, couples_exiting_community_personal_law, payer,
    moderate, biographical, constrained, local).

% Gain formal legal equality under the civil code but frequently lose informal community support networks, inheritance goodwill, and family relationships as the price of exiting community jurisdiction. The statute cannot compel continued social inclusion; the gap between legal right and social reality is borne entirely by the individual, disproportionately women in patrilocal or caste-endogamous communities.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, women_facing_community_ostracism_after_civil_marriage, payer,
    powerless, biographical, trapped, local).

% Muslim personal law boards, Hindu religious trusts, and church tribunals lose adjudicative relevance whenever couples opt into the civil track — a visible, cumulative erosion of jurisdiction and legitimacy. They have no seat in Special Marriage Act proceedings and their objections are treated as extraneous to a purely civil registration, not as a party interest.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, religious_authorities_losing_adjudicative_jurisdiction, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__secular_civil_reading, religious_authorities_losing_adjudicative_jurisdiction, excluded).

% The doctrine of individual constitutional rights as supreme over group-based personal law is vindicated and strengthened every time the Act is successfully invoked; it is not an actor that collects anything but a proposition whose authority is reinforced by the Act's operation.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, constitutional_rights_framework, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(marriage_authority_kernel__secular_civil_reading, constitutional_rights_framework).

% Periodically reviews and amends the notice-period and registration procedures, weighs uniform civil code proposals, and adjudicates constitutional challenges from personal-law boards. Sees the full landscape across all readings of the marriage authority kernel and can alter the civil track's costs going forward.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, national_legislature_and_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__secular_civil_reading, diffuse).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__secular_civil_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a religion-neutral registration and adjudication forum so that marriage, divorce, and succession can be solved once, uniformly, for any two adults regardless of their community's personal law — solving the coordination problem that no single community's law can validly marry an inter-faith couple without conversion.
% TRANSFER_FUNCTION: Moves adjudicative jurisdiction and legitimacy from community/religious authorities to the civil court system, and moves gender-equal statutory defaults to individuals who opt in — at the cost of the social capital, community standing, and informal dispute-resolution access those individuals previously held under community law.
% ABSENT_VOICES: Religious authorities and community elders whose jurisdiction is bypassed have no formal standing in Special Marriage Act proceedings; they would argue the 30-day notice period is inadequate protection for community interests and that civil registration erodes communal self-governance, but this objection is treated as outside the civil forum's scope.
% DISAPPEARANCE_RATIONALE: If the Special Marriage Act vanished, inter-religious couples would have no non-conversion path to legal marriage, women under gender-asymmetric personal law regimes would lose the exit-and-comparison option that disciplines those regimes' worst terms, and the constitutional individual-rights doctrine would lose its primary operative instrument in family law — the personal-law systems would immediately regain monopoly jurisdiction over marriage for their respective communities.
% FOUNDING_PROBLEM: Pre-1954, no legal mechanism existed for two Indian citizens of different religions (or no religious affiliation) to marry without one party converting or renouncing faith; personal law systems were also internally non-uniform and in places starkly gender-asymmetric, with no secular alternative forum.
% FOUNDING_PROBLEM_CORROBORATION: Family law scholars and Law Commission of India reports (institutionally independent of the civil court system that administers the Act) continue to document persistent inter-caste and inter-religious marriage barriers and gender-asymmetric personal law provisions as of recent decades, corroborating that the founding problem remains active rather than resolved; women's rights NGOs outside the court system independently report continued reliance on the Act's protections.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__secular_civil_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__secular_civil_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__secular_civil_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__secular_civil_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__secular_civil_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__secular_civil_reading_tests).
:- end_tests(marriage_authority_kernel__secular_civil_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: real coordination value (enabling otherwise-impossible marriages, providing gender-equal defaults) coexists with real transfer costs (social ostracism, loss of community standing) that fall disproportionately on those who exercise the exit option, especially women. Suppression (0.38) is authored as declining slightly over the measured interval — the notice-period requirement and its historical use for community surveillance and interference have softened somewhat as civil marriage has normalized and courts have narrowed the practical effect of objections raised during the notice window, though the mechanism itself persists. Theater ratio (0.22) is low-to-moderate: the notice-period procedure retains a genuine original verification function (preventing bigamy, confirming capacity to marry) alongside an increasingly vestigial surveillance function that some courts and legislators have flagged as no longer serving its stated purpose. Accessibility collapse is moderate (0.35) — the civil track is a genuine alternative, not a closed system, but exercising it is costly enough that many couples who would prefer it remain under community law by default. Resistance (0.55) is comparatively high because religious authorities across multiple communities actively resist civil-track expansion (opposing uniform civil code proposals, contesting notice-period reform) since their jurisdictional relevance depends on the community-law tracks remaining the default.
 *
 * PERSPECTIVAL GAP:
 *   From the civil court system's seat, the Act is unambiguous coordination infrastructure solving a real legal gap. From the seat of a woman who marries under the Act and is subsequently cut off from her family and community, the same instrument delivered a formal right she cannot actually exercise without catastrophic informal cost — the engine's per-seat computation should show this divergence without either seat's reading being in error.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil court system and constitutional rights framework sit at the beneficiary end: they gain jurisdiction, legitimacy, and doctrinal reinforcement respectively, with the court system holding analytical/arbitrage-grade positioning (it isn't extracted from by its own instrument). Inter-religious couples and women seeking gender-equal terms are moderate-power beneficiaries with real but partial exit costs. Couples exiting community law and especially women facing post-marriage ostracism sit toward the target end — the latter is powerless and trapped, bearing costs the statute itself does not impose but that its exercise triggers, which is why the mapping runs through community response rather than the civil apparatus directly. Religious authorities are payers of jurisdiction rather than money — their loss is institutional relevance, and their organized power lets them resist at the legislative level even as individual couples continue to opt in.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (no non-conversion marriage path; gender-asymmetric personal law with no secular alternative) remains live by external corroboration (Law Commission reports, women's rights NGOs), which is why founding_problem_status is authored as live rather than dead — this blocks a capture/zombie mandatrophy flag. The classification as tangled_rope rather than pure rope reflects that the coordination function is real and substantial (it solves problems no other track can solve) while the extraction (social costs of exit, borne disproportionately by less powerful parties) is also real and asymmetric, and the arrangement requires active enforcement (courts must actively adjudicate against community-law claims of exclusive jurisdiction) to persist as a genuine alternative rather than a nominal one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    notice_period_function_or_vestige,
    'Does the 30-day public notice period still serve its original verification function (preventing bigamy, confirming legal capacity), or has it become primarily a vestigial mechanism that enables community surveillance and interference without commensurate benefit?',
    'Empirical study of notice-period objection outcomes: what fraction of objections filed result in a substantiated legal impediment (existing marriage, incapacity) versus what fraction are unsubstantiated community/family objections that nonetheless cause delay, harassment, or withdrawal of the application.',
    'If objections are overwhelmingly unsubstantiated and used as harassment vectors, the notice period is closer to pure suppression machinery and the theater_ratio should be revised upward; if a meaningful fraction catch genuine impediments, the current moderate theater_ratio is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notice_period_function_or_vestige, empirical, 'Whether the mandatory notice period retains genuine verification function or has become primarily surveillance theater.').

omega_variable(
    exit_cost_attribution,
    'Is the social cost borne by individuals who exit community personal law properly attributed to the secular civil reading''s extraction, or is it an externality imposed by the community-law readings that the civil track merely exposes?',
    'Comparative analysis of whether the same individuals would face equivalent ostracism for exiting community jurisdiction through other means (e.g. religious conversion, informal cohabitation) absent the Act — isolating whether the Act itself is the causal instrument or merely the visible occasion.',
    'If the ostracism cost is attributable primarily to the community-law readings'' own boundary-maintenance mechanisms rather than to anything the civil track does, this reading''s true extractiveness may be lower than authored and the corresponding beneficiary-side sibling readings'' extraction may be correspondingly understated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exit_cost_attribution, conceptual, 'Whether exit-cost extraction belongs to this reading or to the sibling community-law readings whose boundaries are being crossed.').

omega_variable(
    kernel_framing_under_determination,
    'Is the marriage_authority_kernel best framed as five parallel, co-equal legal tracks, or is the secular civil track better understood as the constitutionally supreme default that the four community tracks operate as tolerated exceptions to?',
    'Examine constitutional case law on whether personal law is subject to fundamental rights review (contested doctrine in Indian constitutional law) — if personal law is held subordinate to fundamental rights, the hierarchy framing is more accurate than the parallel-tracks framing.',
    'Under the parallel-tracks framing (adopted here), this reading coexists with siblings as one option among five. Under a hierarchy framing, this reading would sit as the supreme framework and the siblings would be reframed as exceptions requiring justification, which would likely shift several reading_relations from coexists_with toward influences or forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether the kernel is genuinely five co-equal readings or one supreme reading with four tolerated exceptions — the framing choice this story adopts versus an alternative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__secular_civil_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__secular_civil_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(marr_tr_t14, marriage_authority_kernel__secular_civil_reading, theater_ratio, 14, 0.13).
narrative_ontology:measurement(marr_tr_t28, marriage_authority_kernel__secular_civil_reading, theater_ratio, 28, 0.15).
narrative_ontology:measurement(marr_tr_t42, marriage_authority_kernel__secular_civil_reading, theater_ratio, 42, 0.18).
narrative_ontology:measurement(marr_tr_t56, marriage_authority_kernel__secular_civil_reading, theater_ratio, 56, 0.2).
narrative_ontology:measurement(marr_tr_t70, marriage_authority_kernel__secular_civil_reading, theater_ratio, 70, 0.22).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(marr_be_t14, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 14, 0.33).
narrative_ontology:measurement(marr_be_t28, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 28, 0.36).
narrative_ontology:measurement(marr_be_t42, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 42, 0.39).
narrative_ontology:measurement(marr_be_t56, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 56, 0.41).
narrative_ontology:measurement(marr_be_t70, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 70, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(marr_su_t14, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 14, 0.47).
narrative_ontology:measurement(marr_su_t28, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 28, 0.44).
narrative_ontology:measurement(marr_su_t42, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 42, 0.41).
narrative_ontology:measurement(marr_su_t56, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 56, 0.39).
narrative_ontology:measurement(marr_su_t70, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 70, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__secular_civil_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, parsi_communal_reading).

% DUAL FORMULATION NOTE:
% This story is one of five constraint stories decomposing the natural-language concept 'marriage/family law authority in India' per the ε-invariance principle. Each community-law reading (Hindu, Muslim, Christian, Parsi) and this secular civil reading ground authority in a structurally distinct source, have different beneficiary/victim sets, and — critically — have different epsilon values reflecting different extraction profiles. The secular civil reading is authored here with epsilon=0.42, moderate relative to the family: it has less concentrated extraction than readings where a single religious authority controls exit, but nonzero extraction because opting into it imposes real social costs via community response mechanisms outside the civil apparatus's direct control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
