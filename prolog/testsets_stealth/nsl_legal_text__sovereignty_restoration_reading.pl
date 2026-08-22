% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__sovereignty_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__sovereignty_restoration_reading, []).

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
 *   constraint_id: nsl_legal_text__sovereignty_restoration_reading
 *   human_readable: National Security Law as Sovereign Order-Restoration Instrument (Sovereignty-Restoration Reading)
 *   domain: constitutional/political/international
 *
 * SUMMARY:
 *   This story instantiates the sovereignty_restoration_reading of the
 *   nsl_legal_text kernel: the Hong Kong National Security Law as a
 *   legitimate sovereign security instrument that restored constitutional
 *   order after the 2019 unrest. Authored from that seat, the arrangement
 *   solves a real coordination problem — a national security legislation gap
 *   open since 1997, closed centrally after a year of escalating street
 *   conflict — while imposing concentrated costs on a defined class: protest
 *   participants, opposition politicians, and independent media, whom the
 *   reading classifies as security threats rather than as a
 *   general-population target set. Extraction is therefore moderate and
 *   selective rather than diffuse. The claim/metric gap is deliberate: the
 *   reading CLAIMS tangled_rope (genuine coordination with accepted, targeted
 *   costs), while the authored metrics describe rising application breadth
 *   and a growing ritual layer — the engine measures that divergence; the
 *   claim is not reconciled to the metrics. Committer structure (which
 *   kernel, which reading, what siblings would change) is carried in the
 *   omega variables and kernel_context, not in the constraint body.
 *
 * KEY AGENTS:
 *   - - cpg_authority: Primary beneficiary/agenda-setter (institutional/arbitrage) — drafted and promulgated the statute centrally, collects consolidated authority and deterrence value
 *   - - hksar_government: Enforcing beneficiary (institutional/constrained) — administers the law day to day, regained street-level governability
 *   - - designated_ns_judges: Administrative enforcer with sanction exposure (institutional/constrained) — hears cases on hand-picked panels without juries
 *   - - protesters_activists: Primary target (moderate/constrained) — bears arrests, long pretrial custody, and heavy sentences
 *   - - opposition_politicians: Target (moderate/constrained) — disqualified, jailed, or oath-bound out of meaningful competition
 *   - - independent_press: Target (moderate/trapped) — flagship outlet closed, editors detained, criminal exposure for security-adjacent reporting
 *   - - general_public_hk: Near-symmetric seat (moderate/constrained) — order and safety gained, expressive space narrowed
 *   - - pro_establishment_elites: Secondary beneficiary (powerful/mobile) — cleared electoral field and integration agenda access
 *   - - foreign_governments: Excluded challenger (powerful/mobile) — sanctions and treaty responses from outside the arrangement
 *   - - international_treaty_observers: Analytical observer (analytical/analytical) — tracks prosecution patterns and treaty compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, 0.58).
domain_priors:suppression_score(nsl_legal_text__sovereignty_restoration_reading, 0.78).
domain_priors:theater_ratio(nsl_legal_text__sovereignty_restoration_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__sovereignty_restoration_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__sovereignty_restoration_reading, "National Security Law as Sovereign Order-Restoration Instrument (Sovereignty-Restoration Reading)").
narrative_ontology:topic_domain(nsl_legal_text__sovereignty_restoration_reading, "constitutional/political/international").

domain_priors:requires_active_enforcement(nsl_legal_text__sovereignty_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__sovereignty_restoration_reading, 'b1a8d3c2-0cbd-4fd5-a608-d650a2ef530c').
narrative_ontology:cs_kernel_codification('b1a8d3c2-0cbd-4fd5-a608-d650a2ef530c', fixed_text).
narrative_ontology:cs_authority_grounding('b1a8d3c2-0cbd-4fd5-a608-d650a2ef530c', lineage).
narrative_ontology:cs_interpretation_layer_present('b1a8d3c2-0cbd-4fd5-a608-d650a2ef530c').
narrative_ontology:cs_reading_relation('b1a8d3c2-0cbd-4fd5-a608-d650a2ef530c', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1a8d3c2-0cbd-4fd5-a608-d650a2ef530c', nsl_legal_text__jurisdictional_capture_reading, coexists_with).
narrative_ontology:cs_axiom('b1a8d3c2-0cbd-4fd5-a608-d650a2ef530c', foundational, sovereign_defense_prerogative_is_paramount).
narrative_ontology:cs_axiom_status(sovereign_defense_prerogative_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('b1a8d3c2-0cbd-4fd5-a608-d650a2ef530c', sovereign_defense_prerogative_is_paramount, deontological).
narrative_ontology:cs_axiom('b1a8d3c2-0cbd-4fd5-a608-d650a2ef530c', foundational, unrest_constituted_existential_sovereign_threat).
narrative_ontology:cs_axiom_status(unrest_constituted_existential_sovereign_threat, holdable).
narrative_ontology:cs_axiom_grounding('b1a8d3c2-0cbd-4fd5-a608-d650a2ef530c', unrest_constituted_existential_sovereign_threat, empirically_contingent).
narrative_ontology:cs_axiom('b1a8d3c2-0cbd-4fd5-a608-d650a2ef530c', secondary, common_law_survives_within_statutory_carveouts).
narrative_ontology:cs_axiom_status(common_law_survives_within_statutory_carveouts, holdable).
narrative_ontology:cs_axiom_grounding('b1a8d3c2-0cbd-4fd5-a608-d650a2ef530c', common_law_survives_within_statutory_carveouts, conventional).
narrative_ontology:cs_reference_frame('b1a8d3c2-0cbd-4fd5-a608-d650a2ef530c', sovereign_supremacy_post_unrest_normalization).
narrative_ontology:cs_drift_state('b1a8d3c2-0cbd-4fd5-a608-d650a2ef530c', contemporary_post_article23_layering, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('b1a8d3c2-0cbd-4fd5-a608-d650a2ef530c', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, cpg_authority).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, hksar_government).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, pro_establishment_elites).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, protesters_activists).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, opposition_politicians).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, independent_press).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, general_public_hk).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, designated_ns_judges).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, general_public_hk).
narrative_ontology:constraint_vindicates(nsl_legal_text__sovereignty_restoration_reading, sovereign_supremacy_doctrine).
narrative_ontology:constraint_vindicates(nsl_legal_text__sovereignty_restoration_reading, patriots_administering_hong_kong_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and promulgated the statute through the NPC Standing Committee in June 2020 after a year of escalating unrest, bypassing the local legislature entirely. Maintains the Office for Safeguarding National Security in Hong Kong with jurisdiction over the gravest cases, defines the boundaries of secession, subversion, terrorism, and collusion, applies the law extraterritorially, and issues bounty notices for figures abroad. Collects restored uncontested authority over the territory and a demonstrated precedent of sovereign resolve.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, cpg_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% Administers the statute day to day: chairs the Committee for Safeguarding National Security, selects the designated judges, directs the dedicated prosecution division, and ordered civic groups and a major newspaper to disband. Regained street-level governability after 2019 and can pass budgets and policy without filibuster; its latitude to deviate from the central security line has narrowed in the same measure.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, hksar_government, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__sovereignty_restoration_reading, hksar_government, beneficiary).

% Career judges hand-picked by the Chief Executive to hear national security cases on three-member panels without juries in serious matters. Work within reversed bail presumptions and closed-hearing provisions while publicly maintaining that common law method survives inside the statute's bounds. Several serving and former judges carry foreign sanctions, a personal cost of the designation they cannot decline without ending their judicial careers.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, designated_ns_judges, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__sovereignty_restoration_reading, designated_ns_judges, payer).

% Organized the 2019 mobilizations and continue advocacy from inside and outside the territory. Face arrest under the new offenses for slogans, fundraising, and organizing unofficial primaries; endure long pretrial custody and sentences reaching life imprisonment. Dozens have emigrated mid-prosecution or accepted exile; those who remain operate under surveillance and cannot assemble as before.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, protesters_activists, payer,
    moderate, biographical, constrained, regional).

% Lost effectively all elected representation after the electoral overhaul tied to the patriots-administering-Hong-Kong principle. Dozens were disqualified or jailed following the unofficial primary prosecution; continued participation requires oath-taking and loyalty review, so a political career inside the system now presupposes accepting the sovereignty framework the statute encodes.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, opposition_politicians, payer,
    moderate, biographical, constrained, national).

% The largest pro-democracy newspaper was forced to close after arrests under the collusion offense and asset freezes under the statute's enforcement powers; senior editors remain detained. Reporting that touches security topics now carries personal criminal exposure, so remaining outlets self-censor, relocate operations abroad, or fold.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, independent_press, payer,
    moderate, biographical, trapped, national).

% Received restored public order: transport runs, businesses reopened, street violence ended, and early surveys showed marked gains in perceived safety. Pays in narrowed expression — library holdings pulled, films vetted, curricula revised — and in the ambient fact that speech offenses exist which most residents never approach but everyone must now account for. Emigration offers an exit taken by tens of thousands at substantial personal cost.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, general_public_hk, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__sovereignty_restoration_reading, general_public_hk, payer).

% Business and political figures aligned with Beijing gained a cleared electoral field, reliable legislative majorities, and privileged access to Greater Bay Area integration opportunities. Their positions depend on demonstrating loyalty and delivering stability; their capital and family arrangements retain offshore options that soften any downside they personally bear.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, pro_establishment_elites, beneficiary,
    powerful, generational, mobile, national).

% Imposed sanctions on officials and judges, suspended extradition treaties, expanded visa pathways for residents, and formally dispute the statute's compatibility with the Sino-British Joint Declaration. They had no seat in the law's drafting and their objections carry no procedural weight inside the system, though their measures raise the personal and institutional costs paid by the enforcers.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, foreign_governments, excluded,
    powerful, generational, mobile, global).

% UN human rights mechanisms, bar associations, and academic monitors track prosecution patterns, trial fairness, and treaty obligations, publishing findings and recommendations. They hold no enforcement lever inside the jurisdiction; their product is the evidentiary record on which outside assessments of the statute rest.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, international_treaty_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__sovereignty_restoration_reading, cpg_authority).
narrative_ontology:fixing_cost_class(nsl_legal_text__sovereignty_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fills the national security legislation gap that Article 23 of the Basic Law left unenacted for twenty-three years, centralizes threat response in a single chain of command, and restores baseline public order after a year of escalating unrest — policing, prosecution, and deterrence provided once, centrally, rather than through fragmented local ordinances that repeatedly failed passage.
% TRANSFER_FUNCTION: Moves procedural protections (jury trial, bail presumption, judge selection) and political capacities (candidacy, association, speech on sovereignty topics) away from accused persons and opposition actors toward the sovereign center; moves sentencing severity upward for defined conduct classes; moves enforcement discretion to dedicated police and prosecutorial units answerable to the central government.
% ABSENT_VOICES: The pan-democratic legislators who resigned rather than serve under the restructured council, the arrested primary organizers, the shuttered newsroom's staff, and the bar members who objected to the designated-judge arrangement — none had a vote on the text, which the NPC Standing Committee promulgated directly. Their objections register only outside the system or through controlled consultation channels.
% DISAPPEARANCE_RATIONALE: If the statute vanished overnight, prosecutions would halt pending appeals, disbanded organizations would reform, the designated-judge list and specialized police unit would dissolve, and electoral competition would reopen on pre-2021 terms. The entire security architecture built on the text would unwind within months — and this reading's own account predicts that renewed disorder would follow, which is precisely why it holds the arrangement indispensable.
% FOUNDING_PROBLEM: Months of escalating 2019 protests — airport shutdowns, sustained roadblocks, petrol bomb attacks, the storming of the Legislative Council — read by the central government as a foreign-backed attempt at subversion, compounded by the Basic Law's Article 23 remaining unlegislated since 1997 despite a constitutional obligation to enact it.
% FOUNDING_PROBLEM_CORROBORATION: Partial and asymmetric. Commercial chambers, 2021-era resident safety surveys, and credit and insurance assessments corroborate the order-restoration half from outside the governing coalition. No source outside the benefiting parties corroborates the persisting-existential-threat half: UN treaty bodies, foreign governments, and the exiled opposition actively dispute it, and the targeted classes plainly do not attest it. The reading therefore stands on contested corroboration — order restored is attested; threat persisting is asserted.
narrative_ontology:disappearance_verdict(nsl_legal_text__sovereignty_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__sovereignty_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__sovereignty_restoration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nsl_legal_text__sovereignty_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__sovereignty_restoration_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored moderate (0.58 at interval end) because costs concentrate on a defined conduct-and-affiliation class rather than the general population, but the series rises across the grid as application broadens: from protest-era conduct (2020) through the primary-election and media cases (2021-2022), exile bounties and passport measures (2023), and the 2024 layering of the local Safeguarding National Security Ordinance on top of the central statute. Suppression (0.78) is high because persistence depends on dedicated machinery — a specialized police unit, a designated-judge list replacing juries in serious cases, a dedicated prosecution division, reversed bail presumptions — and the suppression_requirement series traces that machinery's build-out, which is why it is authored despite the static-scalar default. Theater (0.32) starts low because enforcement is substantively real, then climbs as ritualized loyalty performance grows: oath ceremonies, national security education days, library and film purges, and patriotic curricula that maintain the appearance of consensus more than they change behavior. Accessibility_collapse (0.62) reflects that the opposition-party route has largely collapsed while costly exit by emigration remains open. Resistance (0.60) is sustained externally — sanctions, suspended treaties, exile advocacy — with little open internal resistance remaining. All three metric series share one seven-point annual grid (2020-2026); the 2026 points are marked projected.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats and the payer seats compute differently from the same statute. From the cpg_authority and hksar_government positions the arrangement is restored order, filled legislative gap, and recovered governability — coordination they built and operate. From the protesters_activists, opposition_politicians, and independent_press positions the same structure is criminalized political existence: assembly, candidacy, and reporting each carry personal custodial risk. The general_public_hk seat sits near symmetric — visible safety gains against ambient expressive narrowing. Designated judges occupy an intermediate institutional position: administering the framework while personally absorbing foreign sanction costs. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The cpg_authority sits nearest the beneficiary end: it wrote the rules, applies them extraterritorially, and holds arbitrage-grade exit from any local backlash. The hksar_government is a beneficiary with lower autonomy — it collects governability but executes a line it did not fully author, placing it slightly further from the pure-beneficiary pole. Pro-establishment elites collect the cleared political field at low d. The three declared victim groups sit near the full-target end, with constrained or trapped exit amplifying their effective burden: press seats are most trapped (asset freezes and licensing leverage), politicians and activists constrained (jail or exile as the remaining exits). The general public derives near-symmetric d from its dual beneficiary/payer position. Foreign governments are excluded rather than coordinated — their exclusion is part of what the enforcement structure maintains — and treaty observers hold the analytical seat.
 *
 * MANDATROPHY ANALYSIS:
 *   Under this reading's own accounting the founding problem remains live: the 2019 unrest is treated as an attempted subversion backed by foreign forces, and the threat environment (exiled organizing, foreign sanctions regimes, alleged interference) is held to persist, so the mandate renews itself rather than atrophying. The 2024 local ordinance extends the mandate into new offense classes — mandate extension, not mandate decay. The mismatch consumer (founding_problem_status x disappearance_verdict) finds live + world_rearranges, so no zombie flag fires on this reading's books. The flag's firing condition is exactly what omega threat_assessment_validity tests: if the existential-threat premise fails, status flips to dead while the arrangement persists, and the piton-or-worse cross-check engages. Mandatrophy mislabeling is prevented in both directions here: the real enforcement work (prosecutions, order maintenance) keeps theater_ratio below the atrophy band, while the rising theater series marks where ritual is beginning to substitute for function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Which reading of the nsl_legal_text kernel correctly characterizes the statute''s operative logic — targeted sovereign defense (this reading), permanent democratic closure (democratic_enclosure_reading), or mainland legal transplantation (jurisdictional_capture_reading)?',
    'Longitudinal prosecution-pattern analysis: whether charges track violent conduct and evidenced foreign collusion (supporting this reading) or peaceful expression and ordinary opposition activity (supporting the enclosure reading), combined with doctrinal analysis of how much common law procedure survives in designated-judge practice.',
    'Resolution reassigns the victim and beneficiary sets and moves epsilon across a wide range; if the enclosure reading prevails, this story''s moderate-epsilon profile converts toward a high-epsilon profile with the general population entering the target set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, empirical, 'Reading indexicality of the NSL kernel: this file instantiates one of three rival readings.').

omega_variable(
    threat_assessment_validity,
    'Was the 2019 unrest an existential threat to sovereign control justifying an extraordinary centralized instrument, or a severe but manageable civil disturbance whose threat framing served as retrospective justification?',
    'Declassified threat assessments, contemporaneous comparison of unrest scale against state coercive capacity, and the evidentiary record underlying the NPCSC decision of May 2020.',
    'If the threat framing fails, this reading''s foundational empirical axiom is defeated, the founding problem flips to dead, and the arrangement''s persistence demands the enclosure or capture explanation instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_assessment_validity, empirical, 'Validity of the existential-threat premise on which this reading rests.').

omega_variable(
    proportionality_scope_drift,
    'Does application remain confined to the narrow threat class this reading licenses, or does it expand into ordinary political competition and administrative routine?',
    'Annual tracking of offense-type and defendant-profile distributions in national security prosecutions, including Safeguarding National Security Ordinance cases from 2024 onward, measured against the reading''s stated target class.',
    'Continued broadening pushes effective extraction beyond the moderate band and erodes the tangled_rope claim toward snare; confinement stabilizes the current profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_scope_drift, empirical, 'Whether the law''s application scope matches the reading''s licensed target class.').

omega_variable(
    kernel_boundary_framing,
    'Is the stabilized kernel the statutory text alone, or the NPCSC decision-plus-interpretation practice that authoritatively extends it?',
    'Doctrinal analysis of whether designated bodies treat the text as controlling or the central authorities'' interpretive pronouncements as effectively co-equal sources of the kernel.',
    'If the broader kernel is adopted, the codification assessment shifts from fixed_text stability toward erosion of the written text''s primacy, changing the drift diagnosis for this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_boundary_framing, conceptual, 'Under-determination in what counts as the kernel this reading reads.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__sovereignty_restoration_reading, 2020, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t2020, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2020, 0.14).
narrative_ontology:measurement_basis(nsl__tr_t2020, observed).
narrative_ontology:measurement(nsl__tr_t2021, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2021, 0.18).
narrative_ontology:measurement_basis(nsl__tr_t2021, observed).
narrative_ontology:measurement(nsl__tr_t2022, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2022, 0.22).
narrative_ontology:measurement_basis(nsl__tr_t2022, observed).
narrative_ontology:measurement(nsl__tr_t2023, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2023, 0.26).
narrative_ontology:measurement_basis(nsl__tr_t2023, observed).
narrative_ontology:measurement(nsl__tr_t2024, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2024, 0.29).
narrative_ontology:measurement_basis(nsl__tr_t2024, observed).
narrative_ontology:measurement(nsl__tr_t2025, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2025, 0.31).
narrative_ontology:measurement_basis(nsl__tr_t2025, observed).
narrative_ontology:measurement(nsl__tr_t2026, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2026, 0.32).
narrative_ontology:measurement_basis(nsl__tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(nsl__be_t2020, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement_basis(nsl__be_t2020, observed).
narrative_ontology:measurement(nsl__be_t2021, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2021, 0.48).
narrative_ontology:measurement_basis(nsl__be_t2021, observed).
narrative_ontology:measurement(nsl__be_t2022, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2022, 0.52).
narrative_ontology:measurement_basis(nsl__be_t2022, observed).
narrative_ontology:measurement(nsl__be_t2023, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2023, 0.54).
narrative_ontology:measurement_basis(nsl__be_t2023, observed).
narrative_ontology:measurement(nsl__be_t2024, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2024, 0.56).
narrative_ontology:measurement_basis(nsl__be_t2024, observed).
narrative_ontology:measurement(nsl__be_t2025, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2025, 0.57).
narrative_ontology:measurement_basis(nsl__be_t2025, observed).
narrative_ontology:measurement(nsl__be_t2026, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2026, 0.58).
narrative_ontology:measurement_basis(nsl__be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t2020, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement_basis(nsl__su_t2020, observed).
narrative_ontology:measurement(nsl__su_t2021, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2021, 0.62).
narrative_ontology:measurement_basis(nsl__su_t2021, observed).
narrative_ontology:measurement(nsl__su_t2022, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2022, 0.66).
narrative_ontology:measurement_basis(nsl__su_t2022, observed).
narrative_ontology:measurement(nsl__su_t2023, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2023, 0.7).
narrative_ontology:measurement_basis(nsl__su_t2023, observed).
narrative_ontology:measurement(nsl__su_t2024, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2024, 0.74).
narrative_ontology:measurement_basis(nsl__su_t2024, observed).
narrative_ontology:measurement(nsl__su_t2025, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2025, 0.76).
narrative_ontology:measurement_basis(nsl__su_t2025, observed).
narrative_ontology:measurement(nsl__su_t2026, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2026, 0.78).
narrative_ontology:measurement_basis(nsl__su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__sovereignty_restoration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, jurisdictional_capture_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the NSL' decomposes into three structurally distinct readings of one kernel text, per the epsilon-invariance principle. The sovereignty_restoration_reading (this file) is the officially instantiated reading and supplies the institutional environment — designated benches, dedicated prosecution, electoral restructuring — that the other two readings describe and contest; it is upstream in the sense that its operation generates the empirical record each sibling interprets. Each sibling file links back here and carries its own epsilon: the enclosure reading authors high extraction over the whole political class, the capture reading authors extraction measured in institutional autonomy lost, and this reading authors moderate extraction over a defined threat class. The epsilon values differ because the readings differ, not because the text differs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
