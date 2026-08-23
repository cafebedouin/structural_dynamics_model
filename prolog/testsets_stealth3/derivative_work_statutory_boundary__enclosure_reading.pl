% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__enclosure_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__enclosure_reading
 *   human_readable: Derivative-Work Boundary: Enclosure Reading (Any-Use Preparation Rule)
 *   domain: legal/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   The standing arrangement this story captures: the statutory
 *   derivative-work boundary is administered so that any incorporation of
 *   copyrighted expression into a new work - quotation, sampling, visual
 *   reference, intermediate copying, translation, transformation -
 *   constitutes preparation of a derivative work, and preparation requires
 *   authorization before creation begins. Clearance desks, blanket licenses,
 *   and per-use negotiations mediate access; enforcement runs through
 *   infringement litigation, statutory notice-and-takedown regimes, and
 *   platform-scale automated fingerprint matching that flags unlicensed
 *   material at upload. The arrangement's public justification is authorial
 *   incentive: creators will only adapt, translate, and build upon works if
 *   every such adaptation is theirs to authorize and price. Its operative
 *   effect is that the protected cultural corpus is available to new creators
 *   as inventory to be licensed, not as raw material to be engaged.
 *   Extraction concentrates in catalog-owning incumbents and the
 *   intermediaries who commission clearance; costs fall on
 *   transformation-based creators, small developers, commons projects, and
 *   memory institutions. Claim/metric independence: claimed_type snare is
 *   asserted from the structural reading (identifiable victims,
 *   enforcement-dependent persistence, suppressed exits); the metrics are
 *   authored independently as descriptive estimates of actual operation - the
 *   engine computes per-seat types, and divergence between claim and computed
 *   output is signal, not error.
 *
 * KEY AGENTS:
 *   - incumbent_rights_holders: agenda-setting beneficiary (institutional/arbitrage) - writes and enforces the boundary's terms, collects clearance and settlement revenue
 *   - licensing_intermediaries: pure beneficiary (organized/mobile) - commissions on mandatory permission transactions
 *   - platform_enforcement_operators: administering intermediary (institutional/constrained) - runs enforcement at scale, pays for it, buys legal peace
 *   - remix_creators: primary target (powerless/identity_locked) - pre-creation demands, no pricing power, practice fused with existing works
 *   - independent_developers: target (moderate/constrained) - clearance-or-rebuild choice
 *   - open_source_communities: organized target (organized/constrained) - clean-room defense under a licensable-event boundary
 *   - libraries_archives: institutional target (organized/constrained) - preservation gated case-by-case
 *   - fan_communities_noncommercial: excluded voice (powerless/trapped) - absorbs enforcement, holds no seat
 *   - courts_and_legislatures: analytical observer (institutional/analytical) - sets how far the boundary reaches
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, 0.82).
domain_priors:suppression_score(derivative_work_statutory_boundary__enclosure_reading, 0.8).
domain_priors:theater_ratio(derivative_work_statutory_boundary__enclosure_reading, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0.54).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__enclosure_reading, snare).
narrative_ontology:human_readable(derivative_work_statutory_boundary__enclosure_reading, "Derivative-Work Boundary: Enclosure Reading (Any-Use Preparation Rule)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__enclosure_reading, "legal/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__enclosure_reading, 'fe8f09d7-3c19-4fc3-be31-eda2fbf5b029').
narrative_ontology:cs_kernel_codification('fe8f09d7-3c19-4fc3-be31-eda2fbf5b029', fixed_text).
narrative_ontology:cs_authority_grounding('fe8f09d7-3c19-4fc3-be31-eda2fbf5b029', extraction).
narrative_ontology:cs_interpretation_layer_present('fe8f09d7-3c19-4fc3-be31-eda2fbf5b029').
narrative_ontology:cs_reading_relation('fe8f09d7-3c19-4fc3-be31-eda2fbf5b029', derivative_work_statutory_boundary__coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('fe8f09d7-3c19-4fc3-be31-eda2fbf5b029', derivative_work_statutory_boundary__hybrid_carveout_reading, forecloses).
narrative_ontology:cs_axiom('fe8f09d7-3c19-4fc3-be31-eda2fbf5b029', foundational, any_expression_use_is_derivative_preparation).
narrative_ontology:cs_axiom_status(any_expression_use_is_derivative_preparation, holdable).
narrative_ontology:cs_axiom_grounding('fe8f09d7-3c19-4fc3-be31-eda2fbf5b029', any_expression_use_is_derivative_preparation, conventional).
narrative_ontology:cs_axiom('fe8f09d7-3c19-4fc3-be31-eda2fbf5b029', foundational, authorial_dominion_over_all_transformations).
narrative_ontology:cs_axiom_status(authorial_dominion_over_all_transformations, holdable).
narrative_ontology:cs_axiom_grounding('fe8f09d7-3c19-4fc3-be31-eda2fbf5b029', authorial_dominion_over_all_transformations, deontological).
narrative_ontology:cs_reference_frame('fe8f09d7-3c19-4fc3-be31-eda2fbf5b029', literal_statutory_authorial_dominion).
narrative_ontology:cs_drift_state('fe8f09d7-3c19-4fc3-be31-eda2fbf5b029', contemporary_transformative_use_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fe8f09d7-3c19-4fc3-be31-eda2fbf5b029', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, incumbent_rights_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, licensing_intermediaries).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, remix_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, independent_developers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, open_source_communities).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, libraries_archives).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, fan_communities_noncommercial).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, platform_enforcement_operators).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__enclosure_reading, total_control_incentive_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major studios, record labels, publishers, and legacy media groups holding large catalogs of protected works. They draft model license terms, bring infringement actions, send takedown notices, and operate licensing desks that price access to their catalogs. Clearance and settlement revenue flows to them, and they can shift catalogs between business models - theatrical, broadcast, streaming, licensing - when any single channel degrades.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, incumbent_rights_holders, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__enclosure_reading, incumbent_rights_holders, beneficiary).

% Performance rights organizations, reproduction rights organizations, and stock-content agencies standing between rights holders and anyone seeking clearance. They take a commission on every permission transaction and maintain the databases and standard forms that make case-by-case clearance possible at scale. Their transaction volume exists because authorization is mandatory; if whole classes of use became permission-free, their business would need to restructure.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, licensing_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% Large user-generated-content platforms running fingerprint-matching, hash-blocking, and notice-handling systems at upload scale. They finance and operate the day-to-day enforcement machinery, share advertising revenue with rights holders under licensing agreements, and purchase legal predictability by cooperating. Opting out would mean renegotiating with every rights holder at once while their core product depends on hosting user uploads.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, platform_enforcement_operators, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__enclosure_reading, platform_enforcement_operators, payer).

% Video essayists, sample-based musicians, mashup and collage artists, fan translators, and reaction-format creators whose working method begins from existing recorded works. Authorization demands arrive before publication, priced by parties with no relationship to them, for uses they often cannot fully describe in advance. Leaving the practice means leaving the medium they trained in and are known for; staying means negotiating from zero each time or publishing unlicensed and absorbing takedowns.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, remix_creators, payer,
    powerless, biographical, identity_locked, global).

% Small game studios, tool makers, and app developers whose products incorporate audio, art styles, interface conventions, or engine behaviors traceable to protected works. They must audit dependencies, buy clearances priced for large-studio budgets, or rebuild assets from scratch. Clean-room reimplementation is available in principle but consumes schedule and capital they do not have.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, independent_developers, payer,
    moderate, biographical, constrained, global).

% Free-software projects, interoperability efforts, and documentation initiatives that reverse-engineer formats, reimplement protocols, and publish compatibility layers. Under a boundary that treats any contact with protected expression as a licensable event, clean-room discipline and copyleft licensing are defensive routines rather than solutions; enforcement actions against documentation and protocol reimplementations land on volunteer labor.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, open_source_communities, payer,
    organized, generational, constrained, global).

% National and research libraries, broadcast archives, and museum conservation labs that digitize, format-shift, index, and exhibit holdings containing protected expression. Each project touches rights whose holders may be defunct, untraceable, or aggregated behind intermediaries; clearance timelines routinely exceed grant periods, and orphan-work material simply waits.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, libraries_archives, payer,
    organized, civilizational, constrained, national).

% Amateur communities producing noncommercial transformative works - fiction, art, video, translation - circulated freely among themselves. They hold no seat in any licensing negotiation, retain no counsel, and learn the boundary's position only through takedown notices and account loss. Their preference, that noncommercial community circulation be left alone, is voiced nowhere the terms are set.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, fan_communities_noncommercial, excluded,
    powerless, biographical, trapped, global).

% Appellate courts applying the statutory definitions case by case, and the legislature that wrote them. They hear contests between rights holders and reuse communities, weigh evidence about market substitution, and periodically reshape doctrine through landmark rulings or amendments. Their determinations set how far the boundary reaches, though they neither collect from nor pay into the licensing economy.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__enclosure_reading, incumbent_rights_holders).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, standardized answer to the question every reuser faces - may I build on this work, and on what terms - by making authorization a uniform precondition for any incorporation of protected expression and by concentrating permission decisions in identifiable rights-holder offices rather than leaving each dispute to after-the-fact litigation.
% TRANSFER_FUNCTION: Moves money (clearance fees, settlement payments, revenue shares) and decision rights (which transformations may exist, in what form, on what timeline) from downstream creators and memory institutions to catalog-owning incumbents and their commissioned intermediaries.
% ABSENT_VOICES: Noncommercial fan communities, classroom educators, and accessibility advocates (captioning, audio description, translation for disabled audiences) would object that their uses are non-substituting and socially valuable, but they appear nowhere in licensing negotiations; terms are set among large rights holders, large commercial licensees, and platform intermediaries.
% DISAPPEARANCE_RATIONALE: If the any-use preparation rule and its enforcement vanished overnight, transformation-based creation would resume without pre-clearance, platform filtering would lose its legal predicate, clearance intermediaries would lose their transaction base, and incumbents would lose both a revenue line and the ability to veto unwanted adaptations; sampling, fan works, preservation digitization, and interoperable reimplementation would reorganize around the vacated space within years.
% FOUNDING_PROBLEM: Unauthorized recastings - translations, dramatizations, abridgments, sequels - that could substitute for the original work in its market and strip authors of the value of their own creations; the derivative-work right was written to give authors control over adaptations of their works.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent rights holders attest the problem is live and expanding with digital reproduction. Corroboration from outside the benefiting parties cuts the other way: the 1976 Act's legislative history frames the right narrowly around market-substituting recastings, the transformative-use line of case law holds that many incorporations do not usurp the original's market, and independent legal-economic scholarship documents negligible substitution for most reuse classes. No party outside the beneficiary set attests that the any-use formulation itself was ever the founding problem.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__enclosure_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because authorization is demanded before creation, rates are decoupled from demonstrated substitution harm, and refusal means the work may not exist at all - the strongest extraction position a rule can occupy. Suppression (0.80) reflects that persistence depends on active machinery - litigation threat, takedown regimes, automated filtering - rather than participant preference; suppression is authored as a raw structural property and enters the engine's computation unscaled. Theater ratio (0.54) is rising: the incentive-to-author rationale still organizes the rhetoric, but a growing share of enforcement activity defends catalog control and clearance revenue rather than any demonstrable incentive function. Accessibility collapse (0.58): public-domain and wholly original creation remain available, but for practices whose medium is transformation the alternative set collapses to license-or-abstain. Resistance (0.66): fair-use advocacy, copyleft discipline, filmmaker best-practice codes, and platform pushback are sustained but fragmented. The measurement series share one time grid (points 0, 10, 20, 30, 40, 50 on a 1976-2026 mapping) so every metric is authored at every examined point; the suppression_requirement series is included because this story specifically traces enforcement-capacity buildup (litigation-only, to statutory takedown, to automated filtering), not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seats the same structure reads as stewardship: catalogs defended, adaptation quality controlled, piracy deterred. From the payer seats it reads as a gate erected before creation, priced by counterparties holding veto power over whether the work may exist. The platform seat straddles: it experiences the machinery as an operating cost it accepted in order to keep hosting. The engine computes these divergent per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent rights holders sit at the beneficiary pole (d near 0.0): the rule subsidizes their catalogs, and arbitrage-grade exit (repurposing catalogs across channels) damps residual exposure further. Licensing intermediaries sit just off the pole (d roughly 0.05-0.10): pure commission collectors with mobile exit. Platform operators derive near-symmetric position (d roughly 0.5): they pay for the enforcement infrastructure yet monetize the licensed-content economy it secures. Remix creators approach the full-target pole (d roughly 0.95): identity-locked exit amplifies exposure past their nominal bargaining weakness. Independent developers (roughly 0.85) and open-source communities (roughly 0.8) are constrained rather than locked; libraries and archives (roughly 0.75) bear diffuse institutional costs on civilizational horizons. Fan communities are excluded rather than seated - structurally targeted, derivationally silent. Global spatial scope amplifies effective extraction modestly for the payer seats, since verification across jurisdictions is harder; no directionality overrides were needed because the beneficiary/victim declarations plus exit options already place every seat correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The mislabel risk runs in both directions. As its beneficiaries tell it, the arrangement is pure incentive coordination: without control over adaptations, no one would invest in translating, dramatizing, or preserving works, and the permission system is the price of that coordination. As its payers tell it, the coordination story is cover for pre-creation toll-taking on uses that rarely substitute. Classification keeps the two separable: a genuine coordination residue (a standardized permission point, uniform answers to reuse questions) coexists with asymmetric extraction (rates decoupled from demonstrated harm, enforcement arriving before creation). On the genealogy interview, the founding problem - market-substituting unauthorized recastings - is disputed: incumbents attest it live; the transformative-use evidentiary record attests the any-use mandate outruns it. A finding that the founding problem is dead, combined with the world-rearranging verdict, is precisely the capture signature the mismatch consumer cross-checks against the rising theater and enforcement trajectories.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_delta,
    'This constraint instantiates the enclosure_reading of the derivative_work_statutory_boundary kernel; how would adopting a sibling reading change the structural classification?',
    'Compile the sibling stories (coordination_reading, hybrid_carveout_reading) and compare computed classifications across the kernel family; the disagreement is located in the boundary''s quantifier - all uses versus a defined subset versus a commercially-conditioned subset.',
    'Under coordination_reading the victim set shrinks to unauthorized fixed recastings and epsilon falls sharply toward rope/tangled_rope territory; under hybrid_carveout_reading the victim set splits by commercial status and enforcement narrows to commercial actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_delta, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    substitution_harm_premise,
    'Does unlicensed incorporation of protected expression actually displace demand for the original work, as the pre-creation licensing requirement presupposes?',
    'Econometric displacement studies comparing sales of originals before and after widespread transformative reuse (sampling, video essays, fan translation), controlling for marketing cycles and catalog age.',
    'If displacement is negligible or negative for most reuse classes, the harm premise beneath the any-use rule fails, the clearance economy loses its justification structure, and the arrangement''s persistence becomes purely enforcement-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_harm_premise, empirical, 'Whether the market-substitution premise supporting universal pre-creation licensing holds empirically.').

omega_variable(
    suppression_structural_vs_chilling,
    'Is the measured suppression carried by the enforcement machinery (takedowns, litigation threat, automated filtering) or internalized as creator self-censorship that anticipates demands never made?',
    'Post-reform trajectory: if a jurisdiction narrows the boundary and self-censorship among transformation-based creators persists for years afterward, the internalized component dominates.',
    'If internalized, effective suppression exceeds the structural measure and outlives formal reform; removing enforcement would not restore the pre-arrangement option set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_chilling, empirical, 'Structural versus internalized suppression mechanism in creator behavior.').

omega_variable(
    identity_lock_extent_among_remixers,
    'How many transformation-based creators are genuinely identity-locked (their artistic practice constitutively involves existing works) versus merely economically constrained?',
    'Longitudinal career data: track creators who faced licensing demands and measure whether they switch mediums (constrained) or abandon practice entirely and report identity-level loss (locked).',
    'Identity-locked targets amplify effective extraction toward the full-target end; if most are merely constrained, aggregate effective extraction for the payer seat falls and the seat computes less severely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_extent_among_remixers, empirical, 'Extent of identity fusion binding transformation-based creators to the licensed-use path.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__enclosure_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(deri_tr_t30, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(deri_tr_t40, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(deri_tr_t50, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 50, 0.54).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(deri_be_t30, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(deri_be_t40, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 40, 0.76).
narrative_ontology:measurement(deri_be_t50, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(deri_su_t10, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(deri_su_t30, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(deri_su_t40, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(deri_su_t50, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 50, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, coordination_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, hybrid_carveout_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the derivative work right' spans three structurally distinct claims about one statutory kernel; per the epsilon-invariance principle they are authored as three linked stories rather than one observable-dependent story. This member carries the strongest quantifier over uses and therefore the highest epsilon and the largest victim set; coordination_reading carries the narrowest boundary; hybrid_carveout_reading conditions the boundary on commercial status. Edges run from this story to both siblings because maximalist administration (automated filtering, blanket licensing, pre-creation clearance norms) changes the legitimacy conditions and enforcement-resource environment under which the narrower readings are argued.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
