% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__judicial_supremacy_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: basic_law_interpretive_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of the Basic Law Interpretive Boundary
 *   domain: constitutional/legal-political
 *
 * SUMMARY:
 *   Under the judicial supremacy reading, Israel's Basic Laws operate as
 *   entrenched higher-order law: the Supreme Court reviews ordinary Knesset
 *   legislation against Basic Law norms and its invalidations bind the
 *   legislature. The arrangement emerged from the 1995 United Mizrahi Bank
 *   decision and matured through proportionality and reasonableness
 *   doctrines. This story instantiates ONE reading of the contested kernel
 *   basic_law_interpretive_boundary; the parliamentary-sovereignty and
 *   balanced-contestation readings are separate constraints with their own
 *   epsilon values, linked only through network edges. The epsilon referent
 *   is the judicial-supremacy arrangement itself, assessed by this reading's
 *   own lights: a functioning constitutional-coordination structure that
 *   nonetheless transfers final lawmaking authority on contested value
 *   questions from the elected coalition to an unelected bench, imposing high
 *   effective barriers on legislation that threatens court-protected
 *   liberties.
 *
 * KEY AGENTS:
 *   - supreme_court_justices: agenda-setter (institutional/constrained) — administers the boundary, sets standards of review, collects interpretive authority
 *   - governing_knesset_coalition: primary target (powerful/constrained) — bears nullification of enacted legislation
 *   - knesset_backbench_legislators: secondary target (moderate/constrained) — bear retrospective voiding of legislative labor
 *   - rights_claimant_petitioners: primary beneficiary (powerless/trapped) — gain veto via litigation
 *   - public_interest_litigation_organizations: beneficiary and agenda-shaper (organized/mobile) — select and frame the constitutional docket
 *   - knesset_minority_factions: beneficiary (moderate/constrained) — use review as leverage against majorities
 *   - ordinary_voters: dual-positioned beneficiary/payer (moderate/trapped) — receive rights protection while losing direct control over contested questions
 *   - constitutional_law_professoriate: beneficiary (organized/mobile) — supplies interpretive capital
 *   - comparative_constitutional_observers: analytical observer (analytical/analytical) — watches without stakes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.64).
domain_priors:suppression_score(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.58).
domain_priors:theater_ratio(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__judicial_supremacy_reading, "Judicial Supremacy Reading of the Basic Law Interpretive Boundary").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__judicial_supremacy_reading, "constitutional/legal-political").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__judicial_supremacy_reading, '29a24f35-31f8-4319-a9a3-e4a4238f01ed').
narrative_ontology:cs_kernel_codification('29a24f35-31f8-4319-a9a3-e4a4238f01ed', fixed_text).
narrative_ontology:cs_authority_grounding('29a24f35-31f8-4319-a9a3-e4a4238f01ed', lineage).
narrative_ontology:cs_interpretation_layer_present('29a24f35-31f8-4319-a9a3-e4a4238f01ed').
narrative_ontology:cs_reading_relation('29a24f35-31f8-4319-a9a3-e4a4238f01ed', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('29a24f35-31f8-4319-a9a3-e4a4238f01ed', basic_law_interpretive_boundary__balanced_contestation_reading, forecloses).
narrative_ontology:cs_axiom('29a24f35-31f8-4319-a9a3-e4a4238f01ed', foundational, judicial_invalidation_binding_on_legislature).
narrative_ontology:cs_axiom_status(judicial_invalidation_binding_on_legislature, holdable).
narrative_ontology:cs_axiom_grounding('29a24f35-31f8-4319-a9a3-e4a4238f01ed', judicial_invalidation_binding_on_legislature, conventional).
narrative_ontology:cs_axiom('29a24f35-31f8-4319-a9a3-e4a4238f01ed', foundational, rights_require_judicial_guardianship_against_majorities).
narrative_ontology:cs_axiom_status(rights_require_judicial_guardianship_against_majorities, holdable).
narrative_ontology:cs_axiom_grounding('29a24f35-31f8-4319-a9a3-e4a4238f01ed', rights_require_judicial_guardianship_against_majorities, deontological).
narrative_ontology:cs_reference_frame('29a24f35-31f8-4319-a9a3-e4a4238f01ed', basic_laws_as_entrenched_supreme_law).
narrative_ontology:cs_drift_state('29a24f35-31f8-4319-a9a3-e4a4238f01ed', post_2023_overhaul_crisis, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('29a24f35-31f8-4319-a9a3-e4a4238f01ed', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimant_petitioners).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_minority_factions).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, public_interest_litigation_organizations).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, governing_knesset_coalition).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_backbench_legislators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, ordinary_voters).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, constitutional_law_professoriate).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, ordinary_voters).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__judicial_supremacy_reading, entrenched_higher_order_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fifteen justices hear petitions challenging Knesset statutes against Basic Law norms, sit in expanded panels for constitutional questions, and issue majority opinions that bind lower courts and, under this reading, the legislature itself. They set the standards of review — proportionality, reasonableness — that determine which statutes survive. Tenure runs to age seventy; leaving the bench means leaving the only seat from which this authority is exercised, and their professional identity is fused with the guardianship function.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices, agenda_setter,
    institutional, generational, constrained, national).

% Passes legislation by majority vote and watches portions of its program struck down or narrowed in retrospect — the 2023 reasonableness limitation was voided in full. It cannot undo adverse rulings by simple majority; its amendment path runs through Basic Law procedures that the Court has begun claiming power to review. Its realistic exits are winning subsequent elections or assembling a supermajority for constitutional restructuring, both slow and politically costly.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, governing_knesset_coalition, payer,
    powerful, biographical, constrained, national).

% Committee months and negotiated statutory text can be voided years later by retrospective application of review standards they did not control. They carry the nullification risk on their legislative labor while exercising little influence over coalition strategy or the defense of challenged statutes.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_backbench_legislators, payer,
    moderate, immediate, constrained, national).

% Individuals and groups whose interests are threatened by legislation obtain a veto point unavailable at the ballot box: file a petition, argue the statute violates Basic Law rights, and ask the Court to freeze or strike the law. Individually they hold little power and cannot exit the jurisdiction; their access runs entirely through counsel and court procedure.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimant_petitioners, beneficiary,
    powerless, biographical, trapped, national).

% Non-governmental legal organizations select which constitutional questions reach the Court, draft the framings, and supply the expertise that shapes doctrine. They collect wins when review succeeds and gain agenda-setting influence over the constitutional docket; they can redirect effort to politics or international forums if the domestic channel narrows.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, public_interest_litigation_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__judicial_supremacy_reading, public_interest_litigation_organizations, agenda_setter).

% Opposition factions lacking plenary votes use petitions to block or narrow majority legislation they could not stop in the chamber. Review is their principal leverage channel; losing it returns them to numerical inferiority on every contested bill.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_minority_factions, beneficiary,
    moderate, biographical, constrained, national).

% Elect the Knesset whose products are subject to nullification, yet receive rights protection and constitutional stability that no transient coalition could credibly offer. Their preferences on contested value questions pass through a judicial filter they cannot vote on directly; no ballot reaches the bench, and they cannot exit citizenship.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, ordinary_voters, beneficiary,
    moderate, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__judicial_supremacy_reading, ordinary_voters, payer).

% Academics supply the interpretive frameworks, clerk pipelines, and expert opinions the system runs on; professional standing tracks the centrality of constitutional interpretation. They move between academy, bench, and private practice, carrying the interpretive capital with them.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, constitutional_law_professoriate, beneficiary,
    organized, generational, mobile, national).

% Foreign courts and scholars cite and monitor the Israeli arrangement — proportionality doctrine travels through these citations. They hold no stake in domestic outcomes and occupy a purely analytical seat.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, comparative_constitutional_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court_justices).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative interpreter for conflicts between ordinary legislation and the Basic Laws, giving citizens enforceable limits on state power, minorities protection against transient majorities, and planners predictable constitutional ground rules — functions no other institution currently performs.
% TRANSFER_FUNCTION: Moves final decision authority on contested value questions from the elected Knesset to the Supreme Court; moves litigation costs onto challengers and, when invalidation succeeds, onto the enacting coalition; moves agenda-setting power to whichever actors can frame a justiciable claim.
% ABSENT_VOICES: Voters who supported the nullified statutes have no seat — their preferences enter only as reframed by counsel before the bench. Litigants without resources cannot reach the petition channel at all. Advocates of legislative supremacy argue in public but hold no position inside this reading's framework; their absence from the adjudicating table is structural.
% DISAPPEARANCE_RATIONALE: If binding invalidation vanished overnight, the Knesset would regain final authority: struck-down provisions would be re-enacted, pending challenges would collapse, rights protection would migrate to political and international channels, and the Court would revert to ordinary appellate status. The constitutional bar's practice, the petition pipeline, and the comparative citation networks built around the arrangement would all contract.
% FOUNDING_PROBLEM: Israel never adopted a formal constitution; the Basic Laws were legislated piecemeal from 1949 as intended building blocks, with no settled mechanism binding ordinary statutes. After the 1992 human-rights Basic Laws, the open problem was whether an elected simple majority could legislate against entrenched rights with no enforceable limit. The 1995 United Mizrahi Bank decision answered by establishing judicial review — the founding problem was binding a sovereign parliament to higher law without a completed written constitution.
% FOUNDING_PROBLEM_CORROBORATION: The founding gap is corroborated from outside the beneficiary set: historians, cross-spectrum constitutional scholars, and the Knesset's own Constitution Committee records attest the missing formal constitution and the Mizrahi Bank turning point. Parliamentary-sovereignty proponents acknowledge the same founding problem while disputing this solution. The narrower claim that the present arrangement is the necessary solution is attested chiefly by the legal community that staffs and benefits from it — corroboration of the problem is strong; corroboration of the solution is largely self-attested.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.64: the arrangement performs a real transfer of lawmaking authority — under this reading's own lights, legislation threatening court-protected liberties faces a high effective barrier, and the enacting coalition bears nullification of enacted work. Suppression is 0.58, authored as a raw structural property unscaled by power or scope: binding invalidation plus entrenchment close the obvious legislative exits, though not completely — the Knesset can legislate within bounds and attempt Basic Law amendments. Theater is low (0.20): invalidations actually occur and reshape statutes; the ceremonial language of supremacy inflates the ratio only modestly. Accessibility collapse is 0.60: once the boundary is understood, apparent alternatives (override clauses, simple amendments) are themselves drawn into review, collapsing the option space partially but not fully. Resistance is 0.70: the 2023 overhaul, mass protest, and proposed override clauses are organized, sustained resistance from the payer side. The claim (tangled_rope) and the metrics were authored independently: the claim asserts both a genuine coordination function and asymmetric extraction held together by active enforcement; the metrics describe observed operation. The temporal series share one grid; extraction and suppression rise monotonically with visible acceleration after 2020 as the Court's constitutional docket deepened and political contestation hardened.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute differently from identical structural data. From the governing coalition's position the arrangement operates as confiscation of an electoral mandate: statutes passed by majority are voided by an unelected panel, and the amendment exit is itself policed. From the minority-faction, petitioner, and bench positions the same structure operates as constitutional order: the only working guarantee that rights survive a hostile majority. Backbench legislators experience a third variant — retrospective destruction of legislative labor without corresponding power. The divergence is structural (role and exit differences), not informational: every seat sees the same invalidations.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (bench, petitioners, minority factions, litigation organizations) derive low directionality — the arrangement subsidizes these seats, and the litigation organizations additionally shape the docket they profit from. Victim declarations (governing coalition, backbench legislators) derive high directionality — they bear the transfer with constrained exits: no override by simple majority, amendment channel under review, electoral turnover slow. Ordinary voters sit near symmetric by declaration (beneficiary with payer secondary role): rights protection received, democratic control surrendered. Scope is national, so the engine's scope amplification applies modestly; the concentrated capture of interpretive authority by the bench seat is recorded on the receipt surface.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — binding a sovereign parliament without a completed constitution — remains live: Israel still lacks a formal constitution and the Basic Law project is unfinished, so no mandatrophy is declared. The tangled_rope classification prevents two mislabels: reading the arrangement as pure coordination ignores the measurable transfer of lawmaking authority from the elected chamber to the bench; reading it as pure usurpation ignores the rights-protection function that minorities and petitioners demonstrably collect. The mismatch consumer should watch the founding_problem_status x disappearance_verdict pair: status live plus world_rearranges is coherent here — the arrangement is load-bearing for a problem that persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_exclusivity,
    'This constraint is the judicial_supremacy_reading of kernel basic_law_interpretive_boundary; if the parliamentary_sovereignty_reading were adopted instead — via an override-clause Basic Law or an explicit Knesset-supremacy declaration — what happens to this constraint''s structure?',
    'Observe political adoption of an override clause or supremacy declaration and whether judicial invalidations cease to bind in practice.',
    'Adoption of the sibling reading collapses this constraint''s epsilon toward zero for Knesset legislation (nullification loses force) and the arrangement atrophies; the disagreement is located exclusively in which institution holds ultimate interpretive authority, so the readings cannot share a single framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_exclusivity, empirical, 'Whether the sibling parliamentary-sovereignty reading displaces this reading, and what that displacement does to the constraint''s structure.').

omega_variable(
    amendment_channel_review,
    'Does the boundary extend to Basic Law amendments themselves (super-entrenchment), or does the Knesset retain a free amendment channel as an exit?',
    'Track the January 2024 reasonableness-amendment judgment (limited review upheld 7-6) and subsequent amendment challenges; watch whether the Court articulates a general standard for reviewing constitutional amendments.',
    'If full review over amendments is established, the legislative escape route closes and the constraint approaches pure-extraction structure from the coalition''s seat; if amendments remain free, a genuine exit valve exists and the hybrid reading is stabilized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_channel_review, empirical, 'Whether the Knesset''s amendment power remains outside the boundary or is drawn inside it.').

omega_variable(
    compliance_sustainability_under_resistance,
    'Can binding invalidation persist against sustained coalition non-compliance and court-curbing legislation, or does enforcement capacity decay?',
    'Track compliance with interim orders, changes to the judicial appointment mechanism, and budget or staffing pressure across successive election cycles.',
    'If compliance decays, the arrangement drifts toward theatrical maintenance — supremacy in form without binding force; if enforcement hardens instead, suppression rises further and payer-side resistance escalates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_sustainability_under_resistance, empirical, 'Whether the enforcement infrastructure survives the current resistance cycle.').

omega_variable(
    coordination_extraction_separability,
    'Is the rights-protection coordination function separable from the transfer of lawmaking authority to the Court, or does protecting rights against majorities inherently require concentrating interpretive power in an unelected body?',
    'Comparative analysis of weak-form review jurisdictions (Canadian notwithstanding clause, UK declarations of incompatibility) achieving comparable rights outcomes with lower authority transfer.',
    'If separable, part of the measured extraction is avoidable overhead and reform designs could preserve the coordination at lower cost; if inseparable, the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the arrangement''s coordination and authority-transfer components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__judicial_supremacy_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1995, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement_basis(basi_tr_t1995, observed).
narrative_ontology:measurement(basi_tr_t2000, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2000, 0.13).
narrative_ontology:measurement_basis(basi_tr_t2000, observed).
narrative_ontology:measurement(basi_tr_t2005, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement_basis(basi_tr_t2005, observed).
narrative_ontology:measurement(basi_tr_t2010, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2010, 0.16).
narrative_ontology:measurement_basis(basi_tr_t2010, observed).
narrative_ontology:measurement(basi_tr_t2015, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2015, 0.17).
narrative_ontology:measurement_basis(basi_tr_t2015, observed).
narrative_ontology:measurement(basi_tr_t2020, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement_basis(basi_tr_t2020, observed).
narrative_ontology:measurement(basi_tr_t2025, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 2025, 0.2).
narrative_ontology:measurement_basis(basi_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(basi_be_t1995, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 1995, 0.44).
narrative_ontology:measurement_basis(basi_be_t1995, observed).
narrative_ontology:measurement(basi_be_t2000, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2000, 0.49).
narrative_ontology:measurement_basis(basi_be_t2000, observed).
narrative_ontology:measurement(basi_be_t2005, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2005, 0.53).
narrative_ontology:measurement_basis(basi_be_t2005, observed).
narrative_ontology:measurement(basi_be_t2010, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2010, 0.56).
narrative_ontology:measurement_basis(basi_be_t2010, observed).
narrative_ontology:measurement(basi_be_t2015, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2015, 0.59).
narrative_ontology:measurement_basis(basi_be_t2015, observed).
narrative_ontology:measurement(basi_be_t2020, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement_basis(basi_be_t2020, observed).
narrative_ontology:measurement(basi_be_t2025, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 2025, 0.64).
narrative_ontology:measurement_basis(basi_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1995, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement_basis(basi_su_t1995, observed).
narrative_ontology:measurement(basi_su_t2000, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2000, 0.41).
narrative_ontology:measurement_basis(basi_su_t2000, observed).
narrative_ontology:measurement(basi_su_t2005, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2005, 0.44).
narrative_ontology:measurement_basis(basi_su_t2005, observed).
narrative_ontology:measurement(basi_su_t2010, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2010, 0.47).
narrative_ontology:measurement_basis(basi_su_t2010, observed).
narrative_ontology:measurement(basi_su_t2015, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2015, 0.51).
narrative_ontology:measurement_basis(basi_su_t2015, observed).
narrative_ontology:measurement(basi_su_t2020, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement_basis(basi_su_t2020, observed).
narrative_ontology:measurement(basi_su_t2025, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 2025, 0.58).
narrative_ontology:measurement_basis(basi_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary__balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'who controls the meaning of the Basic Laws' decomposes into three structurally distinct constraints — one per reading — because assigning ultimacy to the court, the Knesset, or a bounded split yields different epsilon values, different beneficiary/victim structures, and different failure modes. This story is the judicial-supremacy member. Adoption ordering runs through political enactment: whichever reading is adopted determines whether the siblings remain live constraints or collapse into history, so each family member links the other two via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
