% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__marketplace_pidgin_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__marketplace_pidgin_reading, []).

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
 *   constraint_id: hebrew_linguistic_life__marketplace_pidgin_reading
 *   human_readable: Inter-Communal Function Standard for Linguistic Life (Marketplace-Pidgin Reading of Hebrew Continuity)
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   The kernel 'Hebrew linguistic life' asks what it means for a language to
 *   be alive, and the Hebrew case is its hardest instance: a language with
 *   unbroken liturgical transmission, documented commercial use in Ottoman
 *   Jerusalem's markets, and no native speakers for roughly seventeen
 *   centuries before the 1880s. This file instantiates ONE reading of that
 *   kernel — the marketplace-pidgin reading — whose criterion is that a
 *   language is alive when it functions as an inter-communal medium for
 *   practical coordination, regardless of native-speaker status or sacred
 *   function. The expected structural delta under this reading: Hebrew was
 *   continuously alive before the revival as a modified Medieval Hebrew
 *   pidgin of the Jerusalem markets — neither purely preserved nor purely
 *   revived but continuously adapted. The sibling readings (liturgical
 *   preservation; native generational) are separate constraints with their
 *   own epsilon, beneficiaries, and victims, linked through the network
 *   section; they are not averaged into this file. Epsilon's referent is the
 *   standing arrangement under contest — the functional criterion as an
 *   operative standard in the relevant literatures — assessed by this
 *   reading's own lights: the reading endorses the criterion and still
 *   authors the costs it imposes on rival camps. Assumptions recorded:
 *   interval 0-45 maps to approximately 1980-2025; sibling constraint IDs
 *   follow this file's naming convention; provenance commit hashes are
 *   pipeline placeholders pending stamping.
 *
 * KEY AGENTS:
 *   - functional_vitality_sociolinguists: Primary beneficiary (organized/constrained) — their evidence base becomes constitutive of linguistic-life verdicts; citations, posts, and grant lines follow
 *   - pidgin_and_koine_specialists: Secondary beneficiary (moderate/mobile) — supply the comparative contact-language cases; most mobile of the specialist seats
 *   - old_yishuv_merchant_communities: Vindicated historical party (powerless/trapped) — recognized as continuous speakers without voice to claim or contest it
 *   - hebrew_language_academy: Dual-positioned (institutional/identity_locked) — collects continuity legitimation while paying quiet demotion of its revival charter
 *   - liturgical_transmission_authorities: Primary target (institutional/identity_locked) — fifteen centuries of transmission scored irrelevant to the verdict they exist to guard
 *   - revival_narrative_establishment: Secondary target (institutional/identity_locked) — the death-and-resurrection arc demoted to elevation of a persisting medium
 *   - disciplinary_canon_boards: Agenda setter (institutional/arbitrage) — adjudicates which vitality criteria count as rigorous and hedges across readings
 *   - arab_trading_partners: Excluded voice (powerless/trapped) — half the pidgin's user base, erased by both national historiographies
 *   - language_vitality_assessors: Analytical observer (institutional/analytical) — must operationalize 'alive' for hundreds of languages and watches the definitional contest instrumentally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, 0.58).
domain_priors:suppression_score(hebrew_linguistic_life__marketplace_pidgin_reading, 0.48).
domain_priors:theater_ratio(hebrew_linguistic_life__marketplace_pidgin_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__marketplace_pidgin_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__marketplace_pidgin_reading, "Inter-Communal Function Standard for Linguistic Life (Marketplace-Pidgin Reading of Hebrew Continuity)").
narrative_ontology:topic_domain(hebrew_linguistic_life__marketplace_pidgin_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__marketplace_pidgin_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__marketplace_pidgin_reading, 'e624e005-ee11-4893-a581-2aeafa1b8187').
narrative_ontology:cs_kernel_codification('e624e005-ee11-4893-a581-2aeafa1b8187', distributed).
narrative_ontology:cs_authority_grounding('e624e005-ee11-4893-a581-2aeafa1b8187', expertise).
narrative_ontology:cs_interpretation_layer_present('e624e005-ee11-4893-a581-2aeafa1b8187').
narrative_ontology:cs_reading_relation('e624e005-ee11-4893-a581-2aeafa1b8187', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('e624e005-ee11-4893-a581-2aeafa1b8187', hebrew_linguistic_life__native_generational_reading, coexists_with).
narrative_ontology:cs_axiom('e624e005-ee11-4893-a581-2aeafa1b8187', foundational, communicative_function_constitutes_aliveness).
narrative_ontology:cs_axiom_status(communicative_function_constitutes_aliveness, holdable).
narrative_ontology:cs_axiom_grounding('e624e005-ee11-4893-a581-2aeafa1b8187', communicative_function_constitutes_aliveness, instrumental).
narrative_ontology:cs_axiom('e624e005-ee11-4893-a581-2aeafa1b8187', secondary, status_markers_excluded_from_vitality_verdicts).
narrative_ontology:cs_axiom_status(status_markers_excluded_from_vitality_verdicts, holdable).
narrative_ontology:cs_axiom_grounding('e624e005-ee11-4893-a581-2aeafa1b8187', status_markers_excluded_from_vitality_verdicts, conventional).
narrative_ontology:cs_reference_frame('e624e005-ee11-4893-a581-2aeafa1b8187', practical_intercommunal_medium_standard).
narrative_ontology:cs_drift_state('e624e005-ee11-4893-a581-2aeafa1b8187', contemporary_vitality_assessment_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e624e005-ee11-4893-a581-2aeafa1b8187', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, functional_vitality_sociolinguists).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, pidgin_and_koine_specialists).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, old_yishuv_merchant_communities).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_language_academy).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, liturgical_transmission_authorities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, revival_narrative_establishment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_language_academy).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, pre_revival_hebrew_continuity_thesis).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, functional_vitality_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Researchers who document how languages do communicative work between communities — trade, administration, intermarriage — without requiring native acquisition. Their evidence base (marketplace records, responsa, travelers' accounts) becomes central to linguistic-life verdicts under this standard, and standing, citations, and grant lines follow across the specialty as a whole. Leaving the field means abandoning an accumulated archive and toolkit they are trained in, at real mid-career cost.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, functional_vitality_sociolinguists, beneficiary,
    organized, biographical, constrained, global).

% Scholars of contact languages and koines who supply the comparative cases — Jerusalem before 1880, but also trade pidgins elsewhere. The standard makes their cases load-bearing for major historical verdicts rather than curiosities. They retain the most mobility of the specialist seats: the same skills transfer to creole and contact-linguistics venues outside the Hebrew debate.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, pidgin_and_koine_specialists, beneficiary,
    moderate, biographical, mobile, global).

% The Sephardi, Ashkenazi, Maghrebi, and Musta'arabi Jews of Ottoman-era Jerusalem who used a modified Medieval Hebrew to bargain, invoice, and correspond across communal lines. Most left no archives in their own voice; the standard vindicates their everyday practice as continuous linguistic life — a recognition they cannot themselves claim or contest, since later historians and descendants speak for them.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, old_yishuv_merchant_communities, beneficiary,
    powerless, generational, trapped, regional).

% The statutory body stewarding Hebrew language planning in Israel. Continuity narratives feed its authority: a language that adapted continuously rather than resurrecting validates a mandate of guiding organic development. At the same time its founding charter speaks the language of revival, and a standard under which the revival was elevation rather than resurrection quietly rewrites the institution's origin story. Its identity is bound up with Hebrew's fate; it cannot stand apart from any verdict on the language's life.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_language_academy, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_language_academy, payer).

% Rabbinic academies, yeshiva networks, and masoretic philologists for whom Hebrew's uninterrupted life is carried by prayer, study, and textual transmission across fifteen centuries of exile. A standard that counts only practical inter-communal use renders their chain of transmission invisible to linguistic-life verdicts — the very practice they hold constituted the language's survival scores as beside the point. Their self-understanding is constituted by the transmission chain; abandoning the claim is not a position they can take.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, liturgical_transmission_authorities, payer,
    institutional, civilizational, identity_locked, global).

% National historiography, school curricula, museums, and commemorative institutions built around the death-and-resurrection arc: a dead holy tongue restored by pioneers and their children. Under a functional standard the arc becomes elevation of a persisting market medium — less miraculous, less discontinuous — and the 'first Hebrew child' symbolism loses its uniqueness. The narrative is fused with national identity; revising it reads as betrayal from inside the institutions that tell it.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, revival_narrative_establishment, payer,
    institutional, generational, identity_locked, national).

% Journal editors, handbook authors, hiring committees, and curriculum boards that decide which vitality criteria count as rigorous. They adjudicate between the rival standards, increasingly weighting observable inter-communal function, and can hedge — accepting functional evidence while leaving room for transmission-based and nativist arguments — because their position lets them shift emphasis without personal exposure.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, disciplinary_canon_boards, agenda_setter,
    institutional, generational, arbitrage, global).

% The Arabic-speaking merchants, artisans, and neighbors who met Jewish traders in the same markets and used the shared commercial Hebrew alongside Arabic. Both Zionist and Arab-nationalist historiographies wrote them out — the former centering Hebrew alone, the latter minimizing Jewish-Arab linguistic intimacy. They would testify to how much of the market's 'Hebrew' was hybrid, negotiated, and bidirectional; they are unarchived, largely unlettered in the relevant registers, and dead.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, arab_trading_partners, excluded,
    powerless, generational, trapped, regional).

% Endangered-language programs, UNESCO-style vitality frameworks, and survey teams that must operationalize 'alive' for hundreds of languages. They watch the definitional contest closely because whichever criterion wins changes their classifications, funding triage, and documentation priorities; they take positions instrumentally rather than as constituents of the Hebrew debate.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, language_vitality_assessors, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__marketplace_pidgin_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__marketplace_pidgin_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an observable, teachable test for linguistic life — does the language do communicative work between communities — that scholars, vitality assessors, and funders can apply uniformly across unrelated cases without first settling disputes about speaker status or religious function.
% TRANSFER_FUNCTION: Moves classification legitimacy, citations, grant lines, and curricular authority toward functional-use evidence and the specialists who produce it, and away from transmission-chain and nativization evidence and their institutional custodians.
% ABSENT_VOICES: The Arabic-speaking trading partners of the Jerusalem markets — half the pidgin's user base — are absent: unarchived, mostly unlettered in the relevant registers, and written out of both Zionist and Arab-nationalist historiographies. They would complicate the clean 'Hebrew was alive' verdict with testimony about hybridity and bidirectional borrowing. Also absent: ordinary monolingual vernacular users of the period, for whom the question never arose.
% DISAPPEARANCE_RATIONALE: If the functional criterion vanished overnight, vitality frameworks would revert to transmission-based or nativist tests, hundreds of language classifications would flip, the Old Yishuv literature would lose its organizing frame, and the Hebrew continuity dispute would re-center on the rival readings — the arrangements of every named seat depend on this standard's currency.
% FOUNDING_PROBLEM: The pre-1880 status of Hebrew: traditional accounts held the sacred tongue was kept alive by unbroken prayer and study; nationalist accounts celebrated a dead language's miraculous revival; both needed a non-theological test of linguistic life that could arbitrate. This reading was articulated to give the continuity answer such a test: aliveness is doing communicative work, whatever the speakers' status — neither pure preservation nor pure revival but continuous adaptation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: liturgical-tradition philologists who reject the criterion nonetheless concede the marketplace evidence exists (commercial Hebrew in responsa, travelers' accounts, and consular records), and Arabist historians of Ottoman Jerusalem independently document the shared market medium; the surviving dispute is interpretive — what the evidence shows — not whether the practice occurred. No party outside the beneficiary coalition attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__marketplace_pidgin_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__marketplace_pidgin_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_linguistic_life__marketplace_pidgin_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.58 because the criterion, once operative, channels classification legitimacy, citations, and grant lines toward functional-use evidence and its specialists while demoting transmission-chain and nativization evidence and their custodians — an asymmetric transfer riding on a real classificatory service. Suppression is 0.48: enforcement runs through peer review, handbook canon, and vitality-rubric adoption, and graduate training internalizes the functional default, but rival criteria remain publishable and held — penalties are reputational, not existential. Theater is 0.28: the criterion does genuine classificatory work across many cases, with a minority of invocations citing 'functional vitality' without operationalizing it. Accessibility collapse is 0.42 — alternatives (transmission-based, nativist, pluralist-multidimensional definitions) remain coherently holdable; the criterion argues rather than forecloses. Resistance is 0.62: liturgical authorities and revival-narrative institutions actively contest it in print. Claimed type is tangled_rope, stated independently of the metrics: the criterion solves a real coordination problem (an observable, teachable test of linguistic life) AND carries asymmetric extraction through the same structure, held in place by active enforcement. The temporal series share one grid (six points, all three metrics at each) so no metric inherits another's end-state values; the rising suppression_requirement series is authored because this story specifically tracks enforcement build-up (rubric adoption, canon hardening), and the rising extractiveness series may trip the T17 abductive trigger — expected, and welcome as an investigation hypothesis rather than a reclassification.
 *
 * PERSPECTIVAL GAP:
 *   From the liturgical and revival-establishment seats the criterion operates as demotion: practices that constitute their identities are scored irrelevant to the verdict they care most about. From the functional-vitality and pidgin-specialist seats the same criterion operates as overdue recognition: evidence they spent careers assembling becomes load-bearing. The canon-board seat experiences neither cost nor benefit symmetrically — it adjudicates and hedges. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: functional-vitality sociolinguists and pidgin specialists collect standing and resources; the Old Yishuv communities receive vindication they cannot themselves claim; the Academy collects continuity legitimation while paying narrative demotion (dual position, hence the secondary role). Victim declarations map to the demoted camps: transmission authorities and the revival establishment bear the criterion's costs with identity-locked exit, placing them near the full-target end of directionality. The excluded seat (Arab trading partners) sits outside the derivation — authored absence is commentary-grade and drives no correction. Receipt check for gain_flow: each named seat's situation was re-read; gains spread across the beneficiary coalition (two scholar seats, one vindicated community, one dual-positioned academy) with no single seat capturing the extraction's proceeds, hence the affirmative 'diffuse' rather than a named capturer. Fixing cost is authored independently: displacing an operative disciplinary standard requires dismantling accumulated literature, funded programs, and training pipelines, which is prohibitive relative to any single fixer's benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — settling the pre-1880 status of Hebrew on non-theological grounds — remains live: vitality assessment for hundreds of languages still needs a criterion, and the Hebrew verdict remains disputed. Founding-problem status 'live' pairs with disappearance verdict 'world_rearranges': no mismatch flag arises, correctly, because the arrangement has not outlived its function. Mandatrophy analysis guards the opposite error here: reading the criterion's enforcement and career stakes as pure extraction would erase its genuine coordination service (an observable, generalizable test of linguistic life), while reading its coordination service as innocence would erase the demotion it imposes on transmission-custodian and revival-narrative seats. The tangled-rope claim keeps both halves visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This constraint is one reading of the kernel hebrew_linguistic_life — the marketplace_pidgin_reading. What would the sibling readings (liturgical_preservation_reading, native_generational_reading) change structurally if adopted instead?',
    'Generate the sibling stories as separate epsilon-invariant constraints and compare victim sets, epsilon, and computed types across the kernel family.',
    'Under the liturgical sibling the payer and beneficiary sets invert (transmission custodians become beneficiaries; marketplace users become footnotes); under the native sibling the pre-1880 verdict flips to ''dead,'' making this reading''s continuity claim the disputed element. The disagreement is located in the aliveness criterion itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Committer structure: one reading of the hebrew_linguistic_life kernel; siblings are separate constraints.').

omega_variable(
    marketplace_evidence_density,
    'How dense and how Hebrew was pre-1880 commercial Hebrew use in Jerusalem — continuous working medium, occasional crutch, or scattered formulae?',
    'Systematic re-survey of responsa queries, consular and travelers'' accounts, maskilic correspondence, and court records, quantifying frequency, register, and degradation of the commercial Hebrew, with code-switching ratios against Judeo-Arabic, Yiddish, and Ladino.',
    'If use was thin or mostly formulaic, the continuity claim weakens toward the native-generational verdict and this reading''s empirical base erodes; if dense and adaptive, both the liturgical and nativist accounts lose their monopoly on ''life.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marketplace_evidence_density, empirical, 'Density and character of the pre-1880 marketplace Hebrew evidence.').

omega_variable(
    criterion_generalizability,
    'Does ''aliveness equals inter-communal practical coordination'' generalize beyond the Hebrew case, or is it fitted to win this one?',
    'Apply the criterion unchanged to post-Carolingian Latin, Ge''ez, Church Slavonic, and other liturgical languages with commercial residues; check whether the resulting verdicts are accepted by the respective scholarly communities or rejected as absurd.',
    'If the criterion is case-fitted, part of its persuasive force is rhetorical rather than classificatory and its coordination function shrinks; if it generalizes, it stands as a genuine standard and rival criteria must answer for their divergent verdicts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criterion_generalizability, conceptual, 'Whether the reading''s criterion is a general standard or a case-fitted instrument.').

omega_variable(
    arab_participation_erasure,
    'Was the market medium specifically Hebrew, or a shared Jewish-Arab commercial pidgin in which Arabic was an equal constituent — and does the ''Hebrew aliveness'' framing misdescribe a bilingual phenomenon?',
    'Reconstruct the market pidgin''s lexicon and syntax from the same sources with attention to Arabic-derived material and bidirectional borrowing; test whether the medium functions as Hebrew-with-additives or as a distinct intercommunal code.',
    'If the medium was substantially shared, the beneficiary structure widens beyond Hebrew-partisan seats and the reading shades into a shared-heritage account that both national narratives resist; the kernel family''s victim structure rearranges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arab_participation_erasure, empirical, 'Whether the pre-1880 medium was Hebrew proper or a shared Jewish-Arab pidgin.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__marketplace_pidgin_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hll_mkt_pidgin_tr_t0, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(hll_mkt_pidgin_tr_t9, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 9, 0.17).
narrative_ontology:measurement(hll_mkt_pidgin_tr_t18, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 18, 0.2).
narrative_ontology:measurement(hll_mkt_pidgin_tr_t27, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 27, 0.23).
narrative_ontology:measurement(hll_mkt_pidgin_tr_t36, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 36, 0.26).
narrative_ontology:measurement(hll_mkt_pidgin_tr_t45, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 45, 0.28).

% Extraction over time
narrative_ontology:measurement(hll_mkt_pidgin_be_t0, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(hll_mkt_pidgin_be_t9, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 9, 0.42).
narrative_ontology:measurement(hll_mkt_pidgin_be_t18, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 18, 0.47).
narrative_ontology:measurement(hll_mkt_pidgin_be_t27, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 27, 0.52).
narrative_ontology:measurement(hll_mkt_pidgin_be_t36, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 36, 0.56).
narrative_ontology:measurement(hll_mkt_pidgin_be_t45, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 45, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hll_mkt_pidgin_su_t0, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(hll_mkt_pidgin_su_t9, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 9, 0.34).
narrative_ontology:measurement(hll_mkt_pidgin_su_t18, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 18, 0.38).
narrative_ontology:measurement(hll_mkt_pidgin_su_t27, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 27, 0.42).
narrative_ontology:measurement(hll_mkt_pidgin_su_t36, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 36, 0.45).
narrative_ontology:measurement(hll_mkt_pidgin_su_t45, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 45, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__marketplace_pidgin_reading, information_standard).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__native_generational_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'Hebrew was linguistically alive before the revival' decomposes into three epsilon-invariant readings of one kernel. This member (marketplace_pidgin_reading) authors epsilon for the functional-criterion arrangement as this reading sees it: moderate-high, because the standard's operation transfers legitimacy toward functional evidence and away from transmission and nativist custodians. The liturgical sibling authors epsilon for the transmission-chain arrangement (low extraction from its own seat; the marketplace evidence is a footnote); the native-generational sibling authors epsilon for the nativist arrangement and returns a 'dead until 1880' verdict. Upstream/downstream: the liturgical reading is the established tradition this reading argues against; the native-generational reading is the position this reading's evidence pressures. Each file links the others via network.affects_constraints; no reading averages across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
