% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__hybrid_pragmatic_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__hybrid_pragmatic_reading
 *   human_readable: Woodruff Manifesto Settlement — Hybrid Pragmatic Reading
 *   domain: religious/political/legal
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kernel
 *   marriage_commitment_legitimacy: the hybrid pragmatic reading of the 1890
 *   Woodruff Manifesto settlement. On this reading, prophetic authority was
 *   deployed strategically — the declaration managed an exogenous federal
 *   crisis while preserving core theological commitments through deliberate
 *   scope ambiguity about what exactly had been suspended. The doctrine of
 *   plural marriage remained canonically intact; the practice ceased; the gap
 *   between them was administered rather than resolved. The ε referent is the
 *   standing Manifesto arrangement as this reading assesses it — NOT the
 *   arrangement any sibling reading would endorse. The sibling readings
 *   (endogenous_reinterpretation_reading, exogenous_override_reading) are
 *   separate constraints in separate files; the contest between them is
 *   routed to omega variables, not averaged into this one. KEY AGENTS (by
 *   structural relationship): - first_presidency_leadership: agenda-setter
 *   and primary beneficiary (institutional/identity_locked) — authors and
 *   administers the settlement, collects doctrinal flexibility -
 *   quorum_of_twelve_apostles: collective beneficiary with embedded payer
 *   faction (institutional/identity_locked) - rank_and_file_believers:
 *   primary target (moderate/trapped) — bears interpretive uncertainty -
 *   post_manifesto_plural_families: concentrated target (powerless/trapped) —
 *   bear recognition loss - federal_government: exogenous agenda-setter
 *   (institutional/arbitrage) — sets coercive terms, collects compliance -
 *   fundamentalist_dissenters: excluded voice (powerless/identity_locked) -
 *   lds_historians: analytical observer — sees the full authorization chain
 *
 * KEY AGENTS:
 *   - first_presidency_leadership: agenda-setter and primary beneficiary (institutional/identity_locked) — controls interpretation, timing, and enforcement; collects preserved doctrinal flexibility
 *   - quorum_of_twelve_apostles: beneficiary with secondary payer position (institutional/identity_locked) — administered both the permissive and prohibitive phases; two members paid with their offices
 *   - rank_and_file_believers: primary target (moderate/trapped) — bear permanent interpretive uncertainty on a salvation-relevant question
 *   - post_manifesto_plural_families: concentrated target (powerless/trapped) — sealed inside the ambiguity, denied after it closed
 *   - federal_government: exogenous agenda-setter (institutional/arbitrage) — sets the coercive terms, collects compliance, bears no internal costs
 *   - fundamentalist_dissenters: excluded voice (powerless/identity_locked) — their exploitation objection is what the settlement excludes
 *   - lds_historians: analytical observer (analytical/analytical) — reconstructs the authorization chain from archives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.62).
domain_priors:suppression_score(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.68).
domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "Woodruff Manifesto Settlement — Hybrid Pragmatic Reading").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "religious/political/legal").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '20ca1dc4-0cc3-4eed-adae-c3cc317122c3').
narrative_ontology:cs_kernel_codification('20ca1dc4-0cc3-4eed-adae-c3cc317122c3', fixed_text).
narrative_ontology:cs_authority_grounding('20ca1dc4-0cc3-4eed-adae-c3cc317122c3', lineage).
narrative_ontology:cs_interpretation_layer_present('20ca1dc4-0cc3-4eed-adae-c3cc317122c3').
narrative_ontology:cs_reading_relation('20ca1dc4-0cc3-4eed-adae-c3cc317122c3', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('20ca1dc4-0cc3-4eed-adae-c3cc317122c3', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, influences).
narrative_ontology:cs_axiom('20ca1dc4-0cc3-4eed-adae-c3cc317122c3', foundational, prophetic_authority_instrumentally_deployed).
narrative_ontology:cs_axiom_status(prophetic_authority_instrumentally_deployed, holdable).
narrative_ontology:cs_axiom_grounding('20ca1dc4-0cc3-4eed-adae-c3cc317122c3', prophetic_authority_instrumentally_deployed, instrumental).
narrative_ontology:cs_axiom('20ca1dc4-0cc3-4eed-adae-c3cc317122c3', secondary, doctrinal_continuity_through_practice_suspension).
narrative_ontology:cs_axiom_status(doctrinal_continuity_through_practice_suspension, holdable).
narrative_ontology:cs_axiom_grounding('20ca1dc4-0cc3-4eed-adae-c3cc317122c3', doctrinal_continuity_through_practice_suspension, conventional).
narrative_ontology:cs_reference_frame('20ca1dc4-0cc3-4eed-adae-c3cc317122c3', strategic_institutional_adaptation).
narrative_ontology:cs_drift_state('20ca1dc4-0cc3-4eed-adae-c3cc317122c3', post_second_manifesto_consolidation, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('20ca1dc4-0cc3-4eed-adae-c3cc317122c3', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, first_presidency_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, quorum_of_twelve_apostles).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_believers).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, post_manifesto_plural_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, quorum_of_twelve_apostles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1890 declaration, controlled its interpretation, and decided which sealings proceeded and which explanations circulated during the following decade. The presidency's authority over doctrine, timing, and enforcement is the arrangement's operating mechanism: the same office that pronounced the suspension declined to pronounce a doctrinal reversal, retaining discretion over both. Exiting the arrangement would mean dissolving the office's claim to speak authoritatively for God, since the settlement was issued in the prophetic voice.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, first_presidency_leadership, agenda_setter,
    institutional, generational, identity_locked, national).

% Collectively ratified and administered the settlement: some apostles performed post-Manifesto sealings in the colonies and abroad under understood sanction, and after 1904 the same body enforced the tightened prohibition. Two members resigned from the quorum rather than repudiate marriages they had performed or defended, bearing the payer side from inside the beneficiary body. Apostolic office is constitutive of personal identity; resignation was experienced as self-liquidation rather than exit.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, quorum_of_twelve_apostles, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__hybrid_pragmatic_reading, quorum_of_twelve_apostles, payer).

% Were taught for two generations that plural marriage was required for the highest degree of salvation, then told new plural marriages cease without a doctrinal explanation of why. They cannot know whether obedience to the declaration or continued practice serves their eternal standing, and the settlement's ambiguity leaves that question permanently open. Leaving the community carries total social cost in the Intermountain settlements where employment, kinship, and worship are inseparable.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_believers, payer,
    moderate, biographical, trapped, regional).

% Couples sealed between 1890 and 1904, many in the Mexican and Canadian colonies or aboard international vessels, under what participants understood as continuing leadership sanction. After 1904 their marriages were publicly denied, wives were left without recognized marital status, some husbands faced discipline, and children carried contested legitimacy. No exit restores recognition: the sealings happened inside the ambiguity the settlement maintained.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, post_manifesto_plural_families, payer,
    powerless, biographical, trapped, regional).

% Set the coercive terms through the Edmunds and Edmunds-Tucker Acts: criminalization of plural marriage, escheatment of church property, disenfranchisement, and the threat of disincorporation. Once the declaration satisfied enforcement, federal attention moved on — statehood followed, amnesties issued, and the machinery lapsed. The federal seat bears none of the arrangement's internal costs and collects compliance without further expenditure.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Believers who judged the declaration non-binding because it lacked revelatory character, and who sought or performed post-Manifesto sealings outside sanction. They were disciplined, excommunicated, and driven from the councils where the settlement's meaning was fixed. Their objection — that the ambiguity was being exploited to authorize marriages and then deny them — is precisely the voice the arrangement excludes, and their identity fuses with the principle they were expelled for holding.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, fundamentalist_dissenters, excluded,
    powerless, generational, identity_locked, regional).

% Later scholars reconstructing the authorization chain behind post-Manifesto sealings and the drafting history of the declaration from diaries, temple records, and colony archives. They take no part in enforcement or belief, and their findings are the principal external check on every seat's self-description.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, lds_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__hybrid_pragmatic_reading, first_presidency_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a single authoritative settlement of the marriage-practice question at the moment of existential legal crisis: the institution could present one face to federal power, preserve corporate property and statehood prospects, and keep the community intact, solving centrally what would otherwise have forced each family and congregation to negotiate federal criminal law alone.
% TRANSFER_FUNCTION: Moves interpretive certainty and doctrinal closure from rank-and-file believers to institutional leadership: members surrender the ability to know whether plural marriage remains commanded and, for post-Manifesto families, the recognition of their sealings, while leadership retains exclusive discretion over doctrine, timing, and enforcement.
% ABSENT_VOICES: Dissenting believers who held the declaration non-binding and post-Manifesto plural wives whose status depended on its meaning were disciplined out of the conversation or never admitted to it; the councils that fixed the settlement's interpretation contained no seat speaking for the families the ambiguity had created.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, property seizure and leadership incarceration resume under the Edmunds-Tucker machinery, the statehood campaign collapses, and the community splits between compliance and dissent factions — while thousands of existing plural families pass into unresolved legal and sacramental limbo. Every seat's arrangements depend on the settlement having been made.
% FOUNDING_PROBLEM: Federal criminalization of plural marriage under the Edmunds and Edmunds-Tucker Acts threatened confiscation of temples and corporate assets, imprisonment of the leadership, disenfranchisement of members, and destruction of the religious community as a going concern.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the federal enforcement machinery lapsed, Utah statehood in 1896 removed the territorial lever, presidential amnesty proclamations restored civil rights, and congressional debate together with later independent historical scholarship attests the existential crisis ended by the early 1900s. No source outside the beneficiary set claims the founding problem persists.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__hybrid_pragmatic_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.62 at interval end) because the settlement's costs fall asymmetrically: leadership retains discretion while members absorb uncertainty and post-Manifesto families absorb recognition loss. Suppression (0.68) is higher than extraction because persistence after 1904 required active machinery — disciplinary councils, resignation-forced apostles, denial of sealings — not participant preference; suppression is authored as a raw structural property and is NOT scaled by power or scope, whereas extractiveness is scaled by directionality and scope in the engine's computation. Theater (0.46) tracks the gap between public reaffirmation and private flexibility: the declaration's public performance grew more ornamental as its operative meaning migrated into unpublished administrative practice, peaking around the Smoot hearing era before settling once the ambiguity closed. Accessibility collapse is moderate (0.52): exit existed (fundamentalist schisms eventually formed) but at catastrophic social cost, so alternatives were suppressed rather than impossible. Resistance (0.55) is real: post-Manifesto marriages continued for fourteen years, two apostles refused compliance, and dissent communities persisted. The measurement series run on ONE shared time grid (points 0, 4, 8, 12, 16, 20 of a 1890–1910 span) with every tracked metric authored at every point; the trajectory rises monotonically to the Second Manifesto enforcement peak (point 16) then partially settles as the ambiguity that generated extraction was itself closed. Identity-lock operates at three depths: institutional (the presidency cannot exit without dissolving its own authority claim), professional-official (apostolic office constitutive of personhood), and soteriological (members' identity fused with the promise that plural marriage secured highest exaltation). Coalition potential among the powerless victims is neutralized by dispersed grievance — post-Manifesto families did not know of one another, and each household's injury was privately shameful rather than publicly actionable.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the first_presidency_leadership seat the arrangement is successful crisis management — a rope the institution built and rode through an existential storm. From the rank_and_file_believers and post_manifesto_plural_families seats the same structure operates as enforced extraction: a question central to their salvation was answered ambiguously on purpose, and the families created inside the ambiguity were disowned when it closed. Inside the beneficiary body itself, the quorum_of_twelve_apostles splits: the administering majority collects, while the dissenting pair pays with their offices — same power atom, opposite directionalities, differentiated only by what each knew and did. The federal_government seat experiences neither coordination nor extraction, only satisfied demand. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   first_presidency_leadership sits near the beneficiary pole: the settlement subsidizes the office with retained discretion (d low). federal_government sits nearest the subsidy end — the arrangement delivers compliance to it at zero further cost, and its arbitrage-grade exit means nothing traps it into the arrangement's internal costs. quorum_of_twelve_apostles derives low d from beneficiary listing, with the payer residue carried in the secondary role and situation rather than an override, since the split is intra-group and overrides key on power atoms the whole group shares. rank_and_file_believers derive high d (victims, trapped exit amplifies toward full target). post_manifesto_plural_families sit at the extreme target end: powerless, trapped, and injured by the arrangement's specific mechanism rather than incidentally. fundamentalist_dissenters derive high d but collect nothing — their exclusion is the enforcement object. lds_historians are analytical and directionality-neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading predicts mandatrophy outright: the founding problem — existential federal assault — died by 1896–1907 (statehood, amnesty, lapsed enforcement), yet the arrangement persists because dissolution is prohibitively costly for the only actor who could effect it. Full repudiation of the underlying revelation collapses the prophetic-continuity claim on which the office rests; full revival of practice invites renewed prosecution. The cost to fix exceeds what the agenda-setter bears from not fixing, and the gains accrue to a named seat — the classic captured-mandate signature. The tangled_rope classification prevents mislabeling in both directions: a pure-rope reading ignores that after 1904 the settlement transferred real costs onto members who had relied on its ambiguity; a pure-snare reading ignores the genuine survival coordination all seats consumed during 1890–96, when the settlement was the price of the community's continued existence. Both functions ran through the same structure; the ratio between them shifted over the interval, which the measurement series records. The R5 mismatch (founding_problem_status dead × disappearance_verdict world_rearranges) is authored honestly and should fire the capture/zombie flag — under this reading, that flag is correct.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Which of the three readings of the marriage_commitment_legitimacy kernel correctly instantiates the constraint — genuine revelation (endogenous), coerced suspension (exogenous_override), or strategic adaptation (this file)?',
    'Archival reconstruction of the authorization chain behind post-Manifesto sealings combined with contemporaneous private statements of the First Presidency about the declaration''s intended scope and ontological status.',
    'Under the endogenous reading epsilon collapses toward the coordination-cost floor (no extraction if members accept the reversal as commanded); under the exogenous_override reading agenda-setting shifts to the federal seat and leadership becomes a transmission conduit with sharply reduced discretionary gain; under this reading the structure stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Committer-frame omega: this constraint is one reading of the kernel; sibling readings instantiate structurally different constraints with different epsilon, beneficiary structures, and classifications.').

omega_variable(
    post_manifesto_authorization_chain,
    'Were the plural marriages sealed between 1890 and 1904 authorized or tolerated by institutional leadership, or performed without sanction?',
    'Temple and colony sealing records, apostolic journals, and correspondence establishing who authorized each post-Manifesto ceremony and under what understanding.',
    'If authorized, the settlement operated a deceive-then-disown mechanism and epsilon rises toward the snare boundary; if unauthorized, members misled themselves within tolerated ambiguity and measured extraction falls materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_manifesto_authorization_chain, empirical, 'Whether post-Manifesto sealings carried leadership sanction — the factual hinge on which the victim structure''s severity turns.').

omega_variable(
    scope_ambiguity_intentionality,
    'Was the declaration''s scope ambiguity deliberately engineered as strategy, or an emergent artifact of drafting under duress?',
    'Drafting history of the declaration text, contemporaneous council minutes, and the sequence of public versus private instructions issued in 1890–91.',
    'Engineered ambiguity confirms the strategic-extraction structure this reading claims; emergent ambiguity reclassifies leadership from strategic beneficiary to improvising steward and lowers effective extraction accordingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_ambiguity_intentionality, conceptual, 'Whether the ambiguity at the heart of this reading was design or drift.').

omega_variable(
    member_compliance_internalization,
    'Does rank-and-file acceptance of the settlement reflect conviction that the declaration was inspired, or structural coercion that left no affordable alternative?',
    'Post-exit trajectory comparison: members who left for fundamentalist communities versus secular leavers, and whether reported suppression symptoms persist after physical exit from Intermountain communities.',
    'If internalized, effective suppression exceeds the structural measure — members carry the constraint with them after exit; if structural, removal of social barriers would release compliance quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(member_compliance_internalization, empirical, 'Structural versus internalized suppression mechanism in member compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(marr_tr_t4, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 4, 0.34).
narrative_ontology:measurement(marr_tr_t8, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement(marr_tr_t12, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 12, 0.5).
narrative_ontology:measurement(marr_tr_t16, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 16, 0.55).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 20, 0.46).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(marr_be_t4, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(marr_be_t8, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(marr_be_t12, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(marr_be_t16, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(marr_su_t4, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(marr_su_t8, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(marr_su_t12, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(marr_su_t16, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 16, 0.72).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__hybrid_pragmatic_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The kernel marriage_commitment_legitimacy decomposes into three epsilon-distinct readings, each a separate constraint story: endogenous_reinterpretation_reading (revealed reversal — low extraction, coordination framing, members as voluntary beneficiaries), exogenous_override_reading (coerced suspension — extraction located in federally imposed costs, leadership as conduit rather than capturer), and this file, hybrid_pragmatic_reading (moderate extraction, leadership-captured flexibility, members and post-Manifesto families as targets). The upstream/downstream edge runs from this reading toward the endogenous reading: the settlement's textual ambiguity is the enabling condition for the later retroactive narration of the Manifesto as revelation. Family linkage is declared here per the epsilon-invariance principle; no single story averages across the readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
