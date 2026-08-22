% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__autonomy_primacy_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: one_country_two_systems_framework__autonomy_primacy_reading
 *   human_readable: One Country, Two Systems — Autonomy-Primacy Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   This constraint authors the autonomy-primacy reading of the One Country,
 *   Two Systems kernel: the position that the Sino-British Joint Declaration
 *   and Basic Law establish substantive, treaty-guaranteed autonomy for Hong
 *   Kong, that mainland interference with reserved domains (judicial
 *   independence, civil liberties, the electoral pathway toward universal
 *   suffrage) constitutes a breach rather than a lawful exercise of retained
 *   sovereignty, and that international treaty partners retain standing to
 *   monitor and respond to violations. On this reading the 2020 National
 *   Security Law and subsequent LegCo eligibility disqualifications are
 *   extraction events — mainland-directed enforcement capturing domains the
 *   treaty reserved to Hong Kong's autonomous institutions — not legitimate
 *   exercises of sovereign authority. The sibling readings
 *   (sovereignty_primacy_reading, balanced_coexistence_reading) are separate
 *   constraint stories with their own ε and stakeholder structure; this file
 *   does not average across them or describe their contest internally.
 *
 * KEY AGENTS:
 *   - hong_kong_residents: primary beneficiary of the autonomy guarantee while it holds
 *   - hong_kong_judiciary: agenda_setter administering the treaty-guaranteed check on executive/mainland action
 *   - pro_democracy_activists and opposition_legislators: primary targets of the extraction this reading identifies
 *   - international_treaty_partners: observers with monitoring standing but no direct enforcement power
 *   - prc_central_government: excluded from this reading's internal logic by design — its own framing is the subject of the sovereignty_primacy_reading sibling
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, 0.42).
domain_priors:suppression_score(one_country_two_systems_framework__autonomy_primacy_reading, 0.38).
domain_priors:theater_ratio(one_country_two_systems_framework__autonomy_primacy_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__autonomy_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__autonomy_primacy_reading, "One Country, Two Systems — Autonomy-Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__autonomy_primacy_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__autonomy_primacy_reading, 'e2327f80-d144-4ebe-84b8-0b1913e22e75').
narrative_ontology:cs_kernel_codification('e2327f80-d144-4ebe-84b8-0b1913e22e75', fixed_text).
narrative_ontology:cs_authority_grounding('e2327f80-d144-4ebe-84b8-0b1913e22e75', lineage).
narrative_ontology:cs_interpretation_layer_present('e2327f80-d144-4ebe-84b8-0b1913e22e75').
narrative_ontology:cs_reading_relation('e2327f80-d144-4ebe-84b8-0b1913e22e75', one_country_two_systems_framework__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('e2327f80-d144-4ebe-84b8-0b1913e22e75', one_country_two_systems_framework__balanced_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('e2327f80-d144-4ebe-84b8-0b1913e22e75', foundational, treaty_guarantee_binds_sovereign_conduct).
narrative_ontology:cs_axiom_status(treaty_guarantee_binds_sovereign_conduct, holdable).
narrative_ontology:cs_axiom_grounding('e2327f80-d144-4ebe-84b8-0b1913e22e75', treaty_guarantee_binds_sovereign_conduct, conventional).
narrative_ontology:cs_axiom('e2327f80-d144-4ebe-84b8-0b1913e22e75', foundational, judicial_review_constrains_executive_and_central_action).
narrative_ontology:cs_axiom_status(judicial_review_constrains_executive_and_central_action, holdable).
narrative_ontology:cs_axiom_grounding('e2327f80-d144-4ebe-84b8-0b1913e22e75', judicial_review_constrains_executive_and_central_action, conventional).
narrative_ontology:cs_axiom('e2327f80-d144-4ebe-84b8-0b1913e22e75', secondary, democratic_reform_pathway_remains_legally_live).
narrative_ontology:cs_axiom_status(democratic_reform_pathway_remains_legally_live, holdable).
narrative_ontology:cs_axiom_grounding('e2327f80-d144-4ebe-84b8-0b1913e22e75', democratic_reform_pathway_remains_legally_live, empirically_contingent).
narrative_ontology:cs_reference_frame('e2327f80-d144-4ebe-84b8-0b1913e22e75', joint_declaration_treaty_constrained_autonomy).
narrative_ontology:cs_drift_state('e2327f80-d144-4ebe-84b8-0b1913e22e75', post_2020_national_security_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e2327f80-d144-4ebe-84b8-0b1913e22e75', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, international_treaty_partners).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_business_sector).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, pro_democracy_activists).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_civil_society_organizations).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, opposition_legislators).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, sino_british_joint_declaration_supremacy).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, basic_law_as_binding_constitutional_instrument).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under a common-law system with (on this reading) treaty-guaranteed rights — free press, independent courts, freedom of assembly — that mainland residents do not possess. Benefit from the autonomy arrangement as long as it holds; increasingly aware that its guarantees are being tested.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents, beneficiary,
    moderate, biographical, constrained, regional).

% Administers common-law adjudication and judicial review under the Basic Law, historically checking executive overreach. On this reading its independence is the enforceable core of the treaty guarantee; it sets the practical boundary of autonomy through case law, but its authority to interpret is itself contestable and can be bypassed by NPC Standing Committee reinterpretation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary, beneficiary).

% The UK (as co-signatory of the Sino-British Joint Declaration), UN treaty bodies, and other states with stakes in Hong Kong's status as a distinct customs and legal jurisdiction. Monitor compliance, issue reports, extend visa/asylum pathways, and can impose sanctions or downgrade recognition, but hold no direct enforcement power inside Hong Kong.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_treaty_partners, observer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, international_treaty_partners, beneficiary).

% Relies on Hong Kong's separate legal and financial system, common-law contract enforcement, and capital account convertibility to function as an international financial hub distinct from the mainland. Benefits directly from the autonomy the framework guarantees; can relocate capital and operations if the guarantee erodes.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_business_sector, beneficiary,
    powerful, biographical, mobile, global).

% Sought to exercise the political reform pathway the Basic Law promises (eventual universal suffrage). On this reading, their prosecution under the National Security Law and disqualification from office are treaty violations — the mainland intervening in matters the treaty reserved to Hong Kong. Many face imprisonment, exile, or are barred from public life; exit means leaving the jurisdiction entirely.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, pro_democracy_activists, payer,
    powerless, biographical, trapped, regional).

% Unions, independent media, and advocacy groups that operated under the civil-liberties guarantee. Many have dissolved under National Security Law pressure rather than test the enforceability of the treaty guarantee in a mainland-influenced legal environment; the guarantee this reading asserts is precisely what they experienced as failing to hold.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_civil_society_organizations, payer,
    organized, biographical, trapped, regional).

% Elected or formerly-elected members disqualified or prosecuted following mainland-directed eligibility reviews. On this reading, the disqualifications are a treaty breach — mainland interference with a domain (LegCo composition via elections) the Joint Declaration and Basic Law reserved to Hong Kong's autonomous governance.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, opposition_legislators, payer,
    moderate, biographical, trapped, regional).

% Holds sovereign authority over Hong Kong and asserts the National Security Law and NPCSC interpretive power are within its reserved sovereign rights, not treaty violations. This reading treats that assertion as the disputed claim rather than as authoritative — the PRC's own framing is not adopted here, which is why it is excluded from this reading's internal logic even though it is the dominant real-world actor.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_government, excluded,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__autonomy_primacy_reading, diffuse).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__autonomy_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The framework was designed to coordinate the peaceful transfer of sovereignty over Hong Kong while preserving a distinct legal, economic, and civil system for a fixed transitional period — solving the genuine problem of integrating a capitalist, common-law territory into a socialist sovereign state without destroying either.
% TRANSFER_FUNCTION: On this reading, when the arrangement functions as designed it transfers very little — Hong Kong retains its own revenue, courts, and civil liberties regime, with only foreign affairs and defense ceded to Beijing. The extraction this story measures arises where that transfer function has been overridden: political and civic capital moved from Hong Kong's autonomous institutions to mainland-aligned enforcement apparatus (National Security Law prosecutions, LegCo eligibility reviews) in violation of the guarantee.
% ABSENT_VOICES: The PRC's own sovereignty-primacy framing is deliberately excluded from this reading's internal logic (it is the subject of the sibling constraint, sovereignty_primacy_reading) — this is not an oversight but the defining structural choice of authoring one reading cleanly. Within this reading's own terms, disqualified legislators and dissolved civil society groups are absent from the institutions that now govern them.
% DISAPPEARANCE_RATIONALE: If the autonomy guarantee were fully honored (this reading's own success condition), Hong Kong's courts, press, and elections would continue operating as designed and little would visibly change for residents day-to-day. If the guarantee is understood (as many stakeholders now argue) to have already substantially lapsed, its formal disappearance would mainly ratify a fait accompli rather than rearrange anything further — hence contested rather than a clean verdict either way.
% FOUNDING_PROBLEM: Reconciling the 1997 handover of a capitalist, common-law, internationally-integrated territory into a socialist unitary state without triggering capital flight, civil unrest, or the collapse of Hong Kong's function as a financial gateway between China and the world.
% FOUNDING_PROBLEM_CORROBORATION: Independent bodies outside Hong Kong's and Beijing's respective benefiting camps — the UN Human Rights Committee, foreign judges who resigned from the Court of Final Appeal citing the political environment, and the UK Foreign Office's semi-annual Joint Declaration compliance reports — corroborate that the founding problem (peaceful, rights-preserving integration) is treated by Beijing as resolved in its favor while Hong Kong's autonomous institutions and civil society describe it as unresolved and actively regressing. No corroborating source outside either camp affirms the founding problem is fully and uncontestedly live in its original 1997 form.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__autonomy_primacy_reading, contested).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(one_country_two_systems_framework__autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).
:- end_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) rather than low, reflecting that on this reading a genuine coordination function (peaceful transitional integration, preserved common-law commerce) coexists with real, identifiable extraction concentrated on a specific victim class (activists, disqualified legislators, dissolved civil-society organizations) since roughly 2019-2020. Suppression rose sharply through the 2019-2020 period (0.12 to 0.45) reflecting the National Security Law's enforcement apparatus, then eased slightly but remains structurally elevated relative to the pre-2019 baseline — enforcement infrastructure built during the crisis period did not fully retract. Theater ratio is comparatively low (0.25) because the courts and Basic Law institutions continue to perform substantive (not merely theatrical) functions in most ordinary civil and commercial matters — the extraction is concentrated in the political domain rather than diffused across all governance.
 *
 * DIRECTIONALITY LOGIC:
 *   Hong Kong residents broadly and the business sector are structural beneficiaries (low d) as long as the guarantee holds for their ordinary civil and commercial affairs. Pro-democracy activists, opposition legislators, and civil society organizations are structural targets (high d) precisely in the domain (political participation, press freedom, assembly) this reading identifies as breached. The hong_kong_judiciary occupies a dual position: institutionally it is the enforcement mechanism for the guarantee (agenda_setter) and its independence is itself a beneficiary interest, but its practical authority to adjudicate mainland-related matters is bounded by NPCSC interpretive override, which this reading treats as an encroachment rather than a legitimate check.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (peaceful, rights-preserving transitional integration of a distinct legal and economic system) has not fully disappeared — Hong Kong's courts, currency, and commercial law continue operating distinctly from the mainland, so this is not simple mandatrophy where the entire function has hollowed into pure theater. But the political-liberties component of the founding bargain is, on this reading, substantially captured: the coordination function (integration without disruption) persists while a specific extraction (suppression of the promised democratic reform pathway) has been layered onto it using the same institutional machinery. This is the structural signature of tangled_rope rather than either pure rope (if the guarantee held fully) or pure snare (if no coordination function remained at all).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_enforceability_after_handover_period,
    'Is the Sino-British Joint Declaration still a live, internationally enforceable instrument constraining PRC conduct in Hong Kong, or has it become a spent historical document with no binding force under PRC''s own stated position (that it has ''no practical significance'' post-handover)?',
    'International legal scholarship, UN treaty body findings, and state practice (whether foreign governments continue to treat the Joint Declaration as creating obligations, e.g. through sanctions regimes or diplomatic protests) would resolve whether the treaty retains enforceable status under this reading''s own terms.',
    'If the treaty is found to have no enforceable status, this reading''s central claim (that mainland interference is a treaty violation with external enforcement mechanisms) collapses into aspiration rather than binding constraint, which would push the classification toward snare (extraction with a defunct coordination cover) rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_enforceability_after_handover_period, conceptual, 'Whether the Joint Declaration retains binding legal force this reading depends on.').

omega_variable(
    reading_selection_and_sibling_foreclosure,
    'Is the choice to author this as the autonomy-primacy reading (rather than sovereignty-primacy or balanced-coexistence) itself doing evaluative work that should be surfaced, given that the three readings produce materially different victim sets and ε values for the same underlying events (NSL prosecutions, LegCo disqualifications)?',
    'Cross-reading comparison: hold the same event set fixed (2020-2023 NSL enforcement) and compare how each reading''s own internal logic characterizes those events — treaty violation (this reading), lawful sovereign exercise (sovereignty_primacy_reading), or contested boundary management (balanced_coexistence_reading). No external adjudicator resolves which reading is ''correct''; each is a coherent internal framework.',
    'Confirms these are genuinely three distinct constraints rather than one constraint with an observer parameter — satisfying the ε-invariance principle. Any attempt to average or synthesize across the three readings into a single ε would violate DP-001.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_selection_and_sibling_foreclosure, conceptual, 'Documents that reading selection is a committer choice, not a resolvable empirical question, and that the sibling constraints are separate files by design.').

omega_variable(
    democratic_pathway_liveness,
    'Is the Basic Law''s promised pathway toward universal suffrage (Articles 45 and 68) still a live legal commitment under this reading, or has it been definitively foreclosed by the 2021 electoral reforms and NPCSC decisions restricting the franchise and candidate eligibility?',
    'Track whether any Hong Kong court, NPCSC decision, or PRC state organ formally repudiates Articles 45/68 as spent versus merely deferring their implementation indefinitely; also track whether international treaty partners'' compliance reports characterize the pathway as foreclosed or merely delayed.',
    'If the pathway is found definitively foreclosed rather than merely delayed, the extractiveness and suppression trajectory should be revised upward and the founding_problem_status should move from contested toward dead for the political-liberties component specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_pathway_liveness, empirical, 'Whether the promised democratic reform pathway remains formally live or has been foreclosed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__autonomy_primacy_reading, 1997, 2027).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t1997, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 1997, 0.08).
narrative_ontology:measurement(one__tr_t2002, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2002, 0.1).
narrative_ontology:measurement(one__tr_t2007, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2007, 0.12).
narrative_ontology:measurement(one__tr_t2012, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2012, 0.14).
narrative_ontology:measurement(one__tr_t2017, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2017, 0.16).
narrative_ontology:measurement(one__tr_t2019, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2019, 0.18).
narrative_ontology:measurement(one__tr_t2020, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement(one__tr_t2023, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2023, 0.24).
narrative_ontology:measurement(one__tr_t2027, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2027, 0.25).

% Extraction over time
narrative_ontology:measurement(one__be_t1997, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 1997, 0.15).
narrative_ontology:measurement(one__be_t2002, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2002, 0.18).
narrative_ontology:measurement(one__be_t2007, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2007, 0.2).
narrative_ontology:measurement(one__be_t2012, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2012, 0.24).
narrative_ontology:measurement(one__be_t2017, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2017, 0.28).
narrative_ontology:measurement(one__be_t2019, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2019, 0.34).
narrative_ontology:measurement(one__be_t2020, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2020, 0.5).
narrative_ontology:measurement(one__be_t2023, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2023, 0.4).
narrative_ontology:measurement(one__be_t2027, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2027, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t1997, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 1997, 0.12).
narrative_ontology:measurement(one__su_t2002, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2002, 0.14).
narrative_ontology:measurement(one__su_t2007, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2007, 0.16).
narrative_ontology:measurement(one__su_t2012, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2012, 0.2).
narrative_ontology:measurement(one__su_t2017, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2017, 0.25).
narrative_ontology:measurement(one__su_t2019, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2019, 0.32).
narrative_ontology:measurement(one__su_t2020, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2020, 0.45).
narrative_ontology:measurement(one__su_t2023, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2023, 0.4).
narrative_ontology:measurement(one__su_t2027, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2027, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(one_country_two_systems_framework__autonomy_primacy_reading, 0.12).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework__balanced_coexistence_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_national_security_law_enforcement).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language label 'One Country, Two Systems' per the ε-invariance principle: autonomy_primacy_reading (this file, tangled_rope, moderate ε concentrated on a political-liberties victim class), sovereignty_primacy_reading (lawful sovereign delegation, expected low ε for most residents, no treaty-violation framing), and balanced_coexistence_reading (contested-boundary framing resolved through political accommodation, expected moderate ε distributed differently). Each carries its own claimed_type, stakeholder set, and ε; they are linked via affects_constraints rather than merged into one observer-parameterized story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
