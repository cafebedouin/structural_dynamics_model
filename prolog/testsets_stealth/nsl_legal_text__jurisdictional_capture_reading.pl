% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__jurisdictional_capture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__jurisdictional_capture_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: nsl_legal_text__jurisdictional_capture_reading
 *   human_readable: National Security Law as Jurisdictional Capture - Mainland Legal Transplantation Reading
 *   domain: constitutional/political/international
 *
 * SUMMARY:
 *   This file instantiates the jurisdictional_capture_reading of the
 *   nsl_legal_text kernel: the Hong Kong National Security Law (enacted June
 *   2020 by attachment to Basic Law Annex III, authoritatively interpreted by
 *   the NPCSC in December 2022) as a vehicle for transplanting mainland
 *   legal-system structures - designated benches replacing juries, a reversed
 *   bail presumption, an extraterritorial enforcement organ immune from local
 *   jurisdiction, and interpretive supremacy displacing final local
 *   adjudication - into a common law order whose autonomy is thereby
 *   consumed. The epsilon referent is the standing post-2020 arrangement
 *   assessed by this reading's lights: moderate-high, because the procedural
 *   displacement is real and actively enforced, while a genuine
 *   security-coordination function (prosecution of actual violent conduct
 *   dating from the 2019 unrest) shares the same structure. Sibling readings
 *   instantiate different constraints from the same text: the
 *   sovereignty_restoration_reading reads the arrangement as legitimate
 *   sovereign instrumentation (epsilon indexed lower, benefits extending to
 *   restored public order), and the democratic_enclosure_reading reads it as
 *   permanent closure of democratic space (victims shifting to political
 *   participants, epsilon indexed higher on speech dimensions). Same
 *   referent, reading-indexed epsilon; the contest is routed to omegas, not
 *   averaged here. KEY AGENTS (by structural relationship): -
 *   npcsc_interpretive_authority: agenda-setter (institutional/arbitrage) -
 *   drafts, attaches, and interpretively controls the text -
 *   mainland_security_apparatus: primary beneficiary
 *   (institutional/arbitrage) - collects enforcement presence, immunity, and
 *   case-routing power - hong_kong_government: dual-positioned
 *   beneficiary/payer (powerful/identity_locked) - gains appointment and
 *   prosecutorial powers while spending the autonomy that constitutes its
 *   office - hk_judiciary: primary target (moderate/identity_locked) - bears
 *   bench designation, jury removal, and interpretive supersession -
 *   hk_legal_profession: secondary target (organized/constrained) - narrowed
 *   argument space, professional pressure, emigration drain - nsl_defendants:
 *   direct target (powerless/trapped) - bear the transplanted procedure
 *   directly - international_common_law_community: excluded critic
 *   (organized/mobile) - objects from outside, leverages withdrawal -
 *   rule_of_law_monitoring_bodies: analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, 0.74).
domain_priors:suppression_score(nsl_legal_text__jurisdictional_capture_reading, 0.78).
domain_priors:theater_ratio(nsl_legal_text__jurisdictional_capture_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__jurisdictional_capture_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__jurisdictional_capture_reading, "National Security Law as Jurisdictional Capture - Mainland Legal Transplantation Reading").
narrative_ontology:topic_domain(nsl_legal_text__jurisdictional_capture_reading, "constitutional/political/international").

domain_priors:requires_active_enforcement(nsl_legal_text__jurisdictional_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__jurisdictional_capture_reading, '30522b80-f6fd-401a-a794-d91d2bd814f6').
narrative_ontology:cs_kernel_codification('30522b80-f6fd-401a-a794-d91d2bd814f6', fixed_text).
narrative_ontology:cs_authority_grounding('30522b80-f6fd-401a-a794-d91d2bd814f6', extraction).
narrative_ontology:cs_interpretation_layer_present('30522b80-f6fd-401a-a794-d91d2bd814f6').
narrative_ontology:cs_reading_relation('30522b80-f6fd-401a-a794-d91d2bd814f6', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('30522b80-f6fd-401a-a794-d91d2bd814f6', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_axiom('30522b80-f6fd-401a-a794-d91d2bd814f6', foundational, common_law_autonomy_not_subordinate_to_mainland_order).
narrative_ontology:cs_axiom_status(common_law_autonomy_not_subordinate_to_mainland_order, holdable).
narrative_ontology:cs_axiom_grounding('30522b80-f6fd-401a-a794-d91d2bd814f6', common_law_autonomy_not_subordinate_to_mainland_order, conventional).
narrative_ontology:cs_axiom('30522b80-f6fd-401a-a794-d91d2bd814f6', secondary, core_procedural_rights_not_removable_by_executive_designation).
narrative_ontology:cs_axiom_status(core_procedural_rights_not_removable_by_executive_designation, holdable).
narrative_ontology:cs_axiom_grounding('30522b80-f6fd-401a-a794-d91d2bd814f6', core_procedural_rights_not_removable_by_executive_designation, deontological).
narrative_ontology:cs_reference_frame('30522b80-f6fd-401a-a794-d91d2bd814f6', pre_nsl_common_law_autonomy).
narrative_ontology:cs_drift_state('30522b80-f6fd-401a-a794-d91d2bd814f6', post_2022_interpretation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('30522b80-f6fd-401a-a794-d91d2bd814f6', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, npcsc_interpretive_authority).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, hong_kong_government).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hk_judiciary).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hk_legal_profession).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, nsl_defendants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_government).
narrative_ontology:constraint_vindicates(nsl_legal_text__jurisdictional_capture_reading, npcsc_interpretive_supremacy_doctrine).
narrative_ontology:constraint_vindicates(nsl_legal_text__jurisdictional_capture_reading, annex_iii_direct_promulgation_precedent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted the law in Beijing, attached it to Basic Law Annex III without local legislative process, and holds the power of authoritative interpretation that binds every HK court including the Court of Final Appeal. Issued the December 2022 interpretation that barred overseas counsel from national security cases after the local courts had begun hearing the question. Cannot be sued, reviewed, or overridden by any body within the HK legal system.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, npcsc_interpretive_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% Operates the Office for Safeguarding National Security inside Hong Kong; its personnel and acts fall outside local jurisdiction and answer only to mainland oversight. Gathers intelligence, directs the priorities of the local police national security unit, and may assume jurisdiction over cases involving complex foreign elements or state secrets. Collects enforcement presence, information access, and precedent-setting prosecutions while bearing no exposure to the local courts that apply the law.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, agenda_setter).

% The chief executive chairs the Committee for Safeguarding National Security, designates the judges who hear security cases, and issues implementing rules - powers the administration did not previously hold. At the same time it executes directives from the central authority, defends the arrangement in international fora, and spends the distinctiveness-based legitimacy (common law, open courts) that constitutes its office. It cannot relinquish the powers without defying the central authority, nor exercise them without consuming the autonomy that justified them.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_government, beneficiary,
    powerful, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__jurisdictional_capture_reading, hong_kong_government, payer).

% Judges swear an oath to uphold the Basic Law and adjudicate impartially. A subset is designated by the chief executive to hear national security cases, where juries are replaced by panels of designated judges, bail carries a reversed presumption, and the statute's broad definitions govern. Judges outside the designated list watch the Court of Final Appeal's finality yield to authoritative interpretation from above. Several senior figures resigned rather than continue serving; those who remain absorb criticism from both the authorities and the profession.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hk_judiciary, payer,
    moderate, biographical, identity_locked, regional).

% Barristers defend security cases within a narrowed argument space; solicitors advise clients navigating the reversed bail presumption and offense definitions that reach conduct well beyond violence. The Bar Association and Law Society faced leadership challenges and client pressure after issuing critical statements. Senior counsel have emigrated in waves; those who stay risk professional marginalization if they challenge the framework openly, and their admission, practising certificates, and courtroom access all run through institutions the framework oversees.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hk_legal_profession, payer,
    organized, biographical, constrained, regional).

% Persons charged under the four offense categories face extended pre-trial custody, trial without a jury before a designated bench, and potential transfer to mainland jurisdiction for specified case profiles. Bail is granted only if the accused can demonstrate they will not endanger national security - a showing most cannot make. Some pleaded guilty to reduce exposure; acquittals have occurred on narrow statutory grounds. They bear the transplanted procedure directly and have no forum in which to contest its terms.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, nsl_defendants, payer,
    powerless, immediate, trapped, regional).

% Foreign judiciaries, bar associations, and the overseas judges who sat on the Court of Final Appeal object to the procedural departures and say so in reports and diplomatic statements. Several overseas judges resigned from the bench; professional bodies published critical assessments; multiple states imposed sanctions on officials connected to the framework. They hold no standing in the arrangement, were excluded from its drafting, and their principal lever is withdrawal of participation and reputational cost.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, international_common_law_community, excluded,
    organized, biographical, mobile, global).

% UN treaty bodies, academic constitutionalists, and non-governmental monitors compile periodic assessments of charge composition, conviction rates, bail outcomes, and procedural change. They take input from every other seat, publish findings that feed diplomatic responses, and maintain the longitudinal record on which any resolution of the open questions in this story depends.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, rule_of_law_monitoring_bodies, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:fixing_cost_class(nsl_legal_text__jurisdictional_capture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single enforceable framework for national security across the HK-mainland boundary: defines four offense categories, creates dedicated enforcement bodies and a designated-bench procedure, and fills the gap left by twenty-three years of failed local Article 23 legislation. It coordinates threat response between mainland and HK authorities under unified command.
% TRANSFER_FUNCTION: Moves adjudicative control from HK's common law institutions to mainland-directed bodies: judge selection moves to the chief executive acting on committee recommendation, jury trial moves to three-judge panels, bail determination moves to a reversed presumption, final interpretive authority moves from the Court of Final Appeal to the NPCSC, and specified defendant categories become transferable to mainland jurisdiction. Due-process protections move from accused persons to prosecutorial discretion.
% ABSENT_VOICES: The law was drafted by mainland bodies, never introduced to the Legislative Council, and promulgated by annexation without local debate. HK's bar leadership, senior judiciary, civil liberties practitioners, and affected residents had no seat in its authorship; UN treaty bodies and foreign governments commented only after enactment. Their objections survive in bar submissions and diplomatic notes but entered no deliberative record.
% DISAPPEARANCE_RATIONALE: If the law vanished overnight, the designated-judge list would lapse, pending security cases would revert to ordinary criminal procedure with juries and normal bail rules, the mainland security office's presence would lose any legal basis in HK, and the 2022 interpretation's effects would dissolve back into ordinary adjudication. Prosecutorial strategy, defence practice, and the administration's appointment powers would all reorganize around the pre-2020 framework.
% FOUNDING_PROBLEM: The 2019 unrest produced months of street confrontation the HK government could neither resolve politically nor fully police, culminating in university sieges and an opposition electoral landslide; separately, Article 23 local legislation had failed for twenty-three years, leaving national security offenses unenforceable locally. Beijing concluded that HK's own institutions would neither restore order nor legislate the needed framework.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: contemporaneous international reporting and academic studies corroborate the 2019 unrest as real and severe; UN Human Rights Committee concluding observations, international bar association reports, and public statements of former HK judges and officials attest that the acute unrest ended by mid-2020 and dispute that current enforcement tracks any continuing equivalent. No source outside the beneficiary set attests the founding problem persists in its original form; the live disagreement is over whether the response required permanent restructuring of HK's legal order.
narrative_ontology:disappearance_verdict(nsl_legal_text__jurisdictional_capture_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__jurisdictional_capture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__jurisdictional_capture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nsl_legal_text__jurisdictional_capture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__jurisdictional_capture_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.74 (interval end): the procedural changes are decoupled from demonstrated security necessity - jury removal, reversed bail, and interpretive supersession operate across the whole security docket regardless of case character, and the 2024 local ordinance widened scope beyond the original four categories. Suppression is authored at 0.78 as a raw structural property (unscaled; the engine scales only extractiveness, by directionality and scope): persistence depends on enforcement machinery - a dedicated police unit, a dedicated prosecution division, designated-bench assignment, and overseas bounty notices - not on participant preference. Theater ratio 0.32: most activity is functionally consequential (real charges, real sentences), but a rising share is symbolic maintenance (security education campaigns, loyalty ceremonies, bounty announcements aimed at exiles beyond reach). Accessibility collapse 0.62: within the security docket, alternatives (jury trial, ordinary bail, final local adjudication) are foreclosed by fiat and cannot be reconstructed by any local actor; outside the docket, ordinary common law practice continues, bounding the collapse. Resistance 0.55: external resistance is real (multi-state sanctions, resignation of overseas Court of Final Appeal judges, bar association reports, emigration of senior counsel) while internal open resistance is effectively priced out. All three tracked series run on one shared seven-point annual grid (2020-2026); end-state values equal the base_properties scalars. The rising suppression_requirement series traces the enforcement ratchet deliberately - machinery built up year over year - rather than a static enforcement picture.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (NPCSC, mainland security apparatus) should compute a coordination-dominant type: from inside, the arrangement is a security framework they built, fund, and direct, with costs borne elsewhere. The payer seats (judiciary, legal profession, defendants) should compute extraction-dominant types: from inside, the same structure consumes adjudicative control, argument space, and liberty. The hong_kong_government seat is genuinely split - it exercises new powers while its constitutive legitimacy (common law, open courts) is the resource being spent - and should compute differently from both pure seats. The engine derives this divergence from the power and exit data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d for the NPCSC and the mainland security apparatus, both holding arbitrage-grade exit: they wrote the rules and are not subject to them. Victim declarations drive high d for the judiciary and legal profession, amplified by identity_locked and constrained exits - a judge's professional identity is fused with the common law oath, so exit means self-erasure rather than relocation. Defendants derive near-full-target d from trapped exit. One override is recorded: the derivation would place hong_kong_government near the beneficiary end (roughly 0.2) from its beneficiary declaration, but its net structural position is approximately symmetric (0.5) - the powers it gains are exercised at the price of the institutional autonomy that constitutes the office, and it cannot shed either side of the trade. The override keys to its power atom ('powerful'), which no other stakeholder in this story holds, so the correction lands on exactly one seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (2019 unrest plus the twenty-three-year Article 23 vacuum) is contested rather than dead: the acute unrest verifiably ended by mid-2020, but the arrangement not only persisted - it expanded, with the 2024 local ordinance legislating atop the imposed text and broadening offenses. Tangled_rope classification prevents mislabeling in both directions: a pure-snare verdict would erase the genuine coordination component (violent conduct from the unrest period is really prosecuted under it), and a pure-rope verdict would whitewash the asymmetric institutional cost the same structure imposes. On the mismatch consumer: founding_problem_status=contested paired with disappearance_verdict=world_rearranges raises no dead-mandate flag yet, but the trajectory - rising theater_ratio, widening scope beyond the founding trigger - is the accumulating signature that would date a mandate-outlived-function transition if scope growth continues while the founding problem stays resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Which reading of the nsl_legal_text kernel correctly identifies the operative constraint structure - jurisdictional capture, sovereign restoration, or democratic enclosure?',
    'Comparative enforcement analysis: distribution of prosecutions across violent-conduct versus expression and association charges; whether transplanted procedures remain confined to the security docket; longitudinal assessment of legal-order continuity by HK professionals and monitoring bodies.',
    'If the restoration reading is correct, epsilon falls toward coordination-cost levels and the beneficiary set widens to include restored public order for the HK general public; if the enclosure reading is correct, the victim set shifts toward political participants and epsilon rises further on speech dimensions; the present capture reading stands only if procedural displacement outpaces demonstrated security necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Indexical uncertainty over which reading of the NSL text is operative.').

omega_variable(
    transplantation_diffusion_extent,
    'Is the transplanted mainland procedure confined to the national-security docket, or is it diffusing into ordinary criminal and civil administration?',
    'Track ordinary-case indicators after 2022: jury usage rates outside security cases, bail grant rates in comparable non-NSL offenses, and any extension of designated-bench or closed-proceedings mechanisms beyond the statute''s four offense categories.',
    'Confinement supports tangled_rope with bounded extraction; diffusion indicates the coordination shell is being overtaken by system-level replacement, pushing toward snare and raising epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transplantation_diffusion_extent, empirical, 'Whether mainland procedural forms stay sequestered in the security docket or spread through the legal system.').

omega_variable(
    designated_bench_compliance_selection,
    'Do designated judges retain case-level decisional independence, or does the designation mechanism select for compliance?',
    'Compare acquittal rates, bail decisions, and sentence distributions between designated and non-designated benches in matched ordinary criminal matters; examine the career trajectories of judges who ruled against prosecutorial positions in security cases.',
    'Compliance selection would locate the deepest institutional cost inside the judiciary itself and mark the suppression as internalized into career incentives; demonstrated independence would relocate the operative cost to the interpretive layer and lower the judiciary-seat burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(designated_bench_compliance_selection, empirical, 'Whether judge designation filters for compliance or leaves adjudication substantively intact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__jurisdictional_capture_reading, 2020, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t2020, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2020, 0.18).
narrative_ontology:measurement(nsl__tr_t2021, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2021, 0.21).
narrative_ontology:measurement(nsl__tr_t2022, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2022, 0.24).
narrative_ontology:measurement(nsl__tr_t2023, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2023, 0.27).
narrative_ontology:measurement(nsl__tr_t2024, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2024, 0.29).
narrative_ontology:measurement(nsl__tr_t2025, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2025, 0.31).
narrative_ontology:measurement(nsl__tr_t2026, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 2026, 0.32).

% Extraction over time
narrative_ontology:measurement(nsl__be_t2020, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(nsl__be_t2021, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2021, 0.61).
narrative_ontology:measurement(nsl__be_t2022, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2022, 0.66).
narrative_ontology:measurement(nsl__be_t2023, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2023, 0.7).
narrative_ontology:measurement(nsl__be_t2024, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2024, 0.72).
narrative_ontology:measurement(nsl__be_t2025, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2025, 0.73).
narrative_ontology:measurement(nsl__be_t2026, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 2026, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t2020, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement(nsl__su_t2021, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2021, 0.6).
narrative_ontology:measurement(nsl__su_t2022, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2022, 0.65).
narrative_ontology:measurement(nsl__su_t2023, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2023, 0.7).
narrative_ontology:measurement(nsl__su_t2024, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2024, 0.74).
narrative_ontology:measurement(nsl__su_t2025, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2025, 0.76).
narrative_ontology:measurement(nsl__su_t2026, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 2026, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__jurisdictional_capture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, democratic_enclosure_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Hong Kong National Security Law' covers at least three structurally distinct claims about what the text does. Per the epsilon-invariance principle the label is decomposed into a three-story family sharing one referent (the standing post-2020 arrangement) with reading-indexed epsilon: this story (jurisdictional capture - the parties consumed are the common law institutions themselves), sovereignty_restoration_reading (beneficiary-weighted, epsilon indexed to public-order restoration), and democratic_enclosure_reading (victims are political participants, epsilon indexed to civic-space closure). Family members link via affects_constraints; the imposed text sits upstream of each reading's downstream enforcement practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nsl_legal_text__jurisdictional_capture_reading, powerful, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
