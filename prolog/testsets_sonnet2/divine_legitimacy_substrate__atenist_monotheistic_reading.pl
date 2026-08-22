% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__atenist_monotheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__atenist_monotheistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__atenist_monotheistic_reading
 *   human_readable: Atenist Reading of Divine Legitimacy: Pharaonic Monopoly on Revelation
 *   domain: religious/political economy of belief systems
 *
 * SUMMARY:
 *   This story instantiates the Atenist reading of the divine legitimacy
 *   kernel: the claim that legitimate divine authority flows exclusively
 *   through pharaonic revelation of Aten, with all other cults rendered
 *   illegitimate. This is a distinct constraint from the Amun-priestly
 *   reading (which locates legitimacy in an established interpretive
 *   priesthood serving a multi-deity cosmology) and the folk-syncretistic
 *   reading (which locates legitimacy in household and village practice
 *   incorporating multiple deities pragmatically). The three readings are not
 *   the same constraint measured differently — they have different
 *   beneficiaries, different victims, different enforcement mechanisms, and
 *   different persistence profiles. This story's epsilon is authored for the
 *   Atenist arrangement as it actually operated: a centralizing seizure of
 *   interpretive and economic authority, assessed by the reading's own
 *   internal logic (revelation as legitimation), not by whether Aten worship
 *   is 'true.'
 *
 * KEY AGENTS:
 *   - pharaoh_akhenaten: sole interpretive authority, primary beneficiary of centralized control
 *   - amun_priesthood: primary institutional victim, stripped of economic and doctrinal base
 *   - temple_dependent_artisans and provincial_temple_towns: diffuse economic victims of the reorganization
 *   - rural_household_worshippers: excluded from the reorganization's reach entirely, neither targeted nor served
 *   - later_restoration_priesthood: analytical/retrospective observer whose actions corroborate the arrangement's dependence on active enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.81).
domain_priors:suppression_score(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.88).
domain_priors:theater_ratio(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__atenist_monotheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__atenist_monotheistic_reading, "Atenist Reading of Divine Legitimacy: Pharaonic Monopoly on Revelation").
narrative_ontology:topic_domain(divine_legitimacy_substrate__atenist_monotheistic_reading, "religious/political economy of belief systems").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__atenist_monotheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__atenist_monotheistic_reading, 'cdd8547b-2fa4-4a09-84a5-bd24c964dd34').
narrative_ontology:cs_kernel_codification('cdd8547b-2fa4-4a09-84a5-bd24c964dd34', formalized).
narrative_ontology:cs_authority_grounding('cdd8547b-2fa4-4a09-84a5-bd24c964dd34', extraction).
narrative_ontology:cs_reading_relation('cdd8547b-2fa4-4a09-84a5-bd24c964dd34', divine_legitimacy_substrate__amun_polytheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('cdd8547b-2fa4-4a09-84a5-bd24c964dd34', divine_legitimacy_substrate__folk_syncretistic_reading, influences).
narrative_ontology:cs_axiom('cdd8547b-2fa4-4a09-84a5-bd24c964dd34', foundational, aten_sole_true_deity).
narrative_ontology:cs_axiom_status(aten_sole_true_deity, holdable).
narrative_ontology:cs_axiom_grounding('cdd8547b-2fa4-4a09-84a5-bd24c964dd34', aten_sole_true_deity, theological).
narrative_ontology:cs_axiom('cdd8547b-2fa4-4a09-84a5-bd24c964dd34', foundational, pharaoh_sole_legitimate_interpreter).
narrative_ontology:cs_axiom_status(pharaoh_sole_legitimate_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('cdd8547b-2fa4-4a09-84a5-bd24c964dd34', pharaoh_sole_legitimate_interpreter, conventional).
narrative_ontology:cs_reference_frame('cdd8547b-2fa4-4a09-84a5-bd24c964dd34', pre_amarna_multideity_temple_order).
narrative_ontology:cs_drift_state('cdd8547b-2fa4-4a09-84a5-bd24c964dd34', post_akhenaten_restoration, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('cdd8547b-2fa4-4a09-84a5-bd24c964dd34', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, new_royal_administrative_cadre).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, amarna_court_officials).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, temple_dependent_artisans).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, provincial_temple_towns).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, rural_household_worshippers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares himself the sole legitimate recipient and transmitter of Aten's revelation, dismantles rival temple administrations, redirects their landholdings and labor to the new Amarna cult apparatus, and centralizes both religious and political authority in his own person. He alone can authorize doctrine; there is no priestly intermediary layer he does not control.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten, beneficiary).

% A newly elevated class of officials owes its position entirely to the Atenist reorganization; they administer redirected temple estates and revenues on the pharaoh's behalf. Their advancement is contingent on the new orthodoxy holding, so their interests are bound to its continuation even though they did not originate it.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, new_royal_administrative_cadre, beneficiary,
    powerful, biographical, constrained, national).

% Relocated to the new capital at Akhetaten, their social and material standing depends on proximity to the pharaoh and the new cult. They benefit from patronage flows but hold no independent doctrinal authority — everything routes through the throne.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, amarna_court_officials, beneficiary,
    powerful, biographical, constrained, regional).

% Loses temple estates, personnel, and the entire economic and interpretive apparatus built over centuries around Amun-Ra worship. Their institutional exit options are foreclosed by decree — they cannot practice, teach, or administer their former cult openly, and the confiscation of temple wealth strips their material base along with their theological standing.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood, payer,
    organized, generational, trapped, national).

% Sculptors, scribes, weavers, and laborers whose livelihoods were organized around temple commissions and temple-linked redistribution lose their economic base when temple income is redirected. They have no alternative patron network of comparable scale and cannot easily relocate their skills or households.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, temple_dependent_artisans, payer,
    powerless, biographical, trapped, regional).

% Entire towns whose economies were built around temple pilgrimage, festival commerce, and temple employment see that economic circulation collapse as the new cult concentrates activity at Akhetaten. They have no say in the reorganization and cannot relocate an entire town's economic base.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, provincial_temple_towns, payer,
    powerless, generational, trapped, regional).

% Continue quiet household devotion to traditional deities out of official view, since the new orthodoxy has no meaningful presence or enforcement reach into everyday village practice. They are not consulted in the theological reorganization and their continued practice is neither sanctioned nor fully suppressed at the local level — it simply exists outside the new doctrine's actual reach, unlike the temple institutions which are directly targeted.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, rural_household_worshippers, excluded,
    powerless, biographical, constrained, local).

% The post-Amarna restoration administration (Tutankhamun-era and after) that reverses the Atenist reorganization, reopens temples, and reinstates the prior priestly economy. Their subsequent actions and the restoration stela's own language serve as retrospective testimony about what the Atenist period did to the prior arrangement.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, later_restoration_priesthood, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__atenist_monotheistic_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates religious and political authority in a single interpretive channel (the pharaoh alone), which in principle could resolve doctrinal fragmentation and unify royal and religious authority under one command structure, eliminating friction between throne and competing priestly power centers.
% TRANSFER_FUNCTION: Moves land, labor, grain revenues, and interpretive authority away from the established Amun temple network and its dependent towns and artisans, concentrating them in the new royal cult apparatus at Akhetaten and in the officials who administer it.
% ABSENT_VOICES: The Amun priesthood and the towns dependent on temple economies are given no voice in the reorganization — their objections, insofar as they exist in the record, surface only retrospectively through the restoration period's own repudiation of the Atenist arrangement. Rural worshippers are simply outside the frame entirely, neither consulted nor targeted.
% DISAPPEARANCE_RATIONALE: The historical record shows exactly this: upon Akhenaten's death the arrangement was reversed almost immediately — temples reopened, priesthoods reinstated, the capital abandoned, and the pharaoh's name subsequently erased from king lists. The speed and completeness of the reversal demonstrates the arrangement depended entirely on active enforcement by a single ruler, not on any self-sustaining coordination logic.
% FOUNDING_PROBLEM: Ostensibly built to solve a claimed problem of theological confusion or corruption in the traditional multi-deity cult administration, and to unify divine and political authority against a priesthood whose accumulated temple wealth and administrative independence had become a rival power center to the throne.
% FOUNDING_PROBLEM_CORROBORATION: The pharaoh's own inscriptions attest the founding problem as theological (false gods, corrupted worship) and civilizational renewal. The restoration-era Tutankhamun restoration stela, issued by the successor administration and explicitly reversing the arrangement, attests instead that the prior temple system had been in good order and that its dismantling had caused disorder and divine displeasure — corroboration from outside the Atenist court that frames the 'founding problem' as pretextual rather than genuine, though as with any successor propaganda this account is itself interested.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__atenist_monotheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__atenist_monotheistic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__atenist_monotheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises sharply over the interval (0.35 to 0.81) tracking the historically attested pace of temple estate confiscation and administrative relocation to Akhetaten. Suppression tracks closely alongside (0.45 to 0.88) because maintaining a single legitimate interpretive channel required active dismantling of the existing priestly apparatus, not merely its supersession — old temples were closed, personnel reassigned or displaced, and the traditional pantheon's names in some cases physically excised from monuments. Theater ratio rises moderately (0.2 to 0.42): a substantial share of activity, especially later in the reign, involved monumental inscription and ritual performance asserting exclusive legitimacy rather than functional administration, consistent with a regime whose primary vulnerability was the absence of a broad, voluntary base of doctrinal consent.
 *
 * PERSPECTIVAL GAP:
 *   From the pharaoh's seat, this is coordination: unifying divine and political authority to end doctrinal fragmentation. From the Amun priesthood's seat, the same structure is naked expropriation dressed in theological language. The engine should compute a strong seat divergence here — agenda_setter and beneficiary seats read low effective extraction (arbitrage/institutional exit); the payer seats (Amun priesthood, artisans, temple towns) read high effective extraction with no meaningful exit. This divergence is exactly what the classification apparatus should surface, not something to reconcile away.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh Akhenaten sits at the full-beneficiary end: he is simultaneously the sole legitimate interpreter and the direct recipient of the redirected economic base. The new administrative cadre and Amarna court officials are secondary beneficiaries whose positions are contingent on the new order holding — their exit options are constrained because their advancement has no meaning outside this arrangement, yet they did not originate it, so they carry some derivative directionality risk if the arrangement collapses (as it in fact did). The Amun priesthood and temple-dependent populations sit at the full-target end: trapped, organized in the priesthood's case but powerless institutionally against a monarch who controls both political and (newly) sole religious legitimacy. Rural household worshippers are treated as excluded rather than targeted: the story's own evidence suggests village-level practice persisted largely undisturbed because enforcement capacity did not extend that far, which is a materially different structural position from the temple institutions that were the direct object of dismantling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — theological corruption / need for unified divine-political authority — is contested precisely because it never outlives its author: the reversal was immediate and near-total upon Akhenaten's death, which is strong evidence the founding problem was pretextual for a court-specific centralization project rather than a genuine, broadly-shared crisis requiring resolution. Reading this as tangled_rope rather than pure snare acknowledges that a real coordination function existed in principle (unified authority resolving doctrinal fragmentation) even though its actual operation was overwhelmingly extractive and required continuous active enforcement that collapsed the moment enforcement ceased.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_revelation_vs_political_instrument,
    'Was the Atenist doctrine a sincerely held theological conviction on Akhenaten''s part, or primarily an instrument for breaking the Amun priesthood''s accumulated political and economic power?',
    'Comparative analysis of the timing and sequencing of doctrinal proclamation versus temple asset confiscation; correspondence and administrative records (where they survive) indicating whether economic reorganization preceded or followed theological justification.',
    'If primarily instrumental, the coordination-function claim (unifying divine and political authority to resolve doctrinal fragmentation) is largely cover, pushing the classification toward snare; if genuinely theological with extraction as a side effect of implementation, the tangled_rope reading (real coordination intent, asymmetric extraction in practice) is better supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_revelation_vs_political_instrument, conceptual, 'Whether Atenist monotheism was sincere theology or an instrument of political-economic consolidation.').

omega_variable(
    kernel_framing_reveal_vs_institution,
    'Should the atenist reading''s kernel be framed as ''the claim of exclusive revelation'' (a legitimacy narrative) or as ''the pharaonic office as sole interpretive institution'' (a structural arrangement)? These two framings could yield different cs_pattern classifications — the former centers a contestable truth-claim, the latter centers an administrative monopoly regardless of the claim''s content.',
    'Compare classification outcomes under both framings using the same underlying evidence (temple confiscation records, inscriptional exclusivity claims, restoration-era reversal actions) to see whether the coordination/extraction balance shifts.',
    'If the institutional framing is adopted, the constraint may read closer to pure extraction (an administrative seizure regardless of theological content); if the legitimacy-narrative framing is adopted, the coordination story (unifying fragmented worship) retains more independent weight, supporting the tangled_rope reading chosen here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_reveal_vs_institution, conceptual, 'Alternative framings of the kernel — legitimacy claim versus institutional monopoly — that could shift the classification.').

omega_variable(
    reach_of_suppression_beyond_temples,
    'How far did Atenist enforcement actually extend into household and village-level worship, versus remaining concentrated on state temple institutions?',
    'Archaeological survey of household shrine artifacts and amulets from the Amarna period at non-elite sites, compared against pre- and post-Amarna baselines.',
    'If household practice was substantially undisturbed (as this story assumes, treating rural worshippers as excluded rather than targeted), the suppression metric is accurately scoped to institutional actors; if household practice was also actively suppressed, the suppression score and victim set would need to expand to include rural worshippers directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reach_of_suppression_beyond_temples, empirical, 'Whether suppression was confined to state temple institutions or extended into everyday household practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__atenist_monotheistic_reading, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(divi_tr_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement(divi_tr_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 6, 0.33).
narrative_ontology:measurement(divi_tr_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 9, 0.38).
narrative_ontology:measurement(divi_tr_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(divi_tr_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 17, 0.42).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(divi_be_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(divi_be_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(divi_be_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 9, 0.75).
narrative_ontology:measurement(divi_be_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 12, 0.79).
narrative_ontology:measurement(divi_be_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 17, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(divi_su_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(divi_su_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 6, 0.74).
narrative_ontology:measurement(divi_su_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 9, 0.82).
narrative_ontology:measurement(divi_su_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 12, 0.86).
narrative_ontology:measurement(divi_su_t17, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 17, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__atenist_monotheistic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.08).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints in the divine_legitimacy_substrate kernel family. amun_polytheistic_reading describes the prior (and subsequently restored) arrangement in which legitimacy flows through an established priestly interpretive body serving a multi-deity cosmology — likely a lower-suppression, higher-legitimacy-cost-to-challenge tangled_rope or rope depending on how entrenched priestly extraction was independently assessed. folk_syncretistic_reading describes a largely informal, low-enforcement arrangement at the household/village level that this story treats as mostly outside the Atenist reorganization's reach. This story (atenist_monotheistic_reading) is the most acutely extractive and coercively enforced of the three, precisely because it required an abrupt, centralized seizure of an existing institutional base rather than evolving within an existing coordination structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
