% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__syncretic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__syncretic_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__syncretic_reading
 *   human_readable: Honji-Suijaku Syncretic Reading: Kami-Buddha Unified Cosmological Order
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   This story instantiates the SYNCRETIC READING of the shinbutsu
 *   ontological commitment kernel: the claim that kami and buddhas are
 *   aspects of one unified cosmological order, with kami as suijaku (trace
 *   manifestations) of Buddhist honji (original grounds). This reading treats
 *   the honji-suijaku metaphysics as genuine doctrinal integration, not mere
 *   institutional accommodation (the partition reading) or tolerated
 *   incoherence (the incoherence reading — see sibling stories). Under this
 *   reading, the coordination function (a single interpretive grammar for
 *   combinatory shrine-temple complexes) is real, but it is bought at the
 *   cost of subordinating Shinto's independent cosmological standing to a
 *   hierarchy in which Buddhist doctrine supplies the 'real' account of what
 *   kami are. The extraction is doctrinal and institutional: interpretive
 *   authority, land, and patronage flow toward Buddhist schools and jingu-ji
 *   administration, while local kami cults lose the standing to say their
 *   deities are not derivative of anything else.
 *
 * KEY AGENTS:
 *   - buddhist_temple_hierarchy: primary beneficiary and agenda_setter (institutional/arbitrage) — administers the syncretic doctrine and the jingu-ji institutions built on it
 *   - shingon_tendai_doctrinal_schools: primary beneficiary (institutional/arbitrage) — supplies the cosmological architecture that makes kami legible as buddhas
 *   - shinto_shrine_priesthoods: primary payer (moderate/constrained) — retains local ritual role but loses cosmological authorship
 *   - local_kami_cults: primary target (powerless/trapped) — loses independent ontological standing entirely
 *   - imperial_court_patrons: secondary beneficiary (institutional/arbitrage) — uses doctrinal coherence as a tool of political consolidation
 *   - historians_of_religion: analytical observer — reconstructs which reading the historical record actually supports, contested across sites and periods
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, 0.62).
domain_priors:suppression_score(shinbutsu_ontological_commitment__syncretic_reading, 0.58).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__syncretic_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__syncretic_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__syncretic_reading, "Honji-Suijaku Syncretic Reading: Kami-Buddha Unified Cosmological Order").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__syncretic_reading, "religious_studies/japanese_history/ontology_of_practice").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__syncretic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__syncretic_reading, '413c4c0d-9840-493c-b429-8ab643b40aff').
narrative_ontology:cs_kernel_codification('413c4c0d-9840-493c-b429-8ab643b40aff', distributed).
narrative_ontology:cs_authority_grounding('413c4c0d-9840-493c-b429-8ab643b40aff', lineage).
narrative_ontology:cs_interpretation_layer_present('413c4c0d-9840-493c-b429-8ab643b40aff').
narrative_ontology:cs_reading_relation('413c4c0d-9840-493c-b429-8ab643b40aff', shinbutsu_ontological_commitment__partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('413c4c0d-9840-493c-b429-8ab643b40aff', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('413c4c0d-9840-493c-b429-8ab643b40aff', foundational, kami_are_provisional_buddha_manifestations).
narrative_ontology:cs_axiom_status(kami_are_provisional_buddha_manifestations, overridden).
narrative_ontology:cs_axiom_grounding('413c4c0d-9840-493c-b429-8ab643b40aff', kami_are_provisional_buddha_manifestations, theological).
narrative_ontology:cs_axiom('413c4c0d-9840-493c-b429-8ab643b40aff', secondary, single_cosmological_order_underlies_all_ritual_practice).
narrative_ontology:cs_axiom_status(single_cosmological_order_underlies_all_ritual_practice, overridden).
narrative_ontology:cs_axiom_grounding('413c4c0d-9840-493c-b429-8ab643b40aff', single_cosmological_order_underlies_all_ritual_practice, conventional).
narrative_ontology:cs_reference_frame('413c4c0d-9840-493c-b429-8ab643b40aff', esoteric_buddhist_cosmological_primacy).
narrative_ontology:cs_drift_state('413c4c0d-9840-493c-b429-8ab643b40aff', meiji_shinbutsu_bunri_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('413c4c0d-9840-493c-b429-8ab643b40aff', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, buddhist_temple_hierarchy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, shingon_tendai_doctrinal_schools).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, shinto_shrine_priesthoods).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, local_kami_cults).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, imperial_court_patrons).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__syncretic_reading, honji_suijaku_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__syncretic_reading, buddhist_cosmological_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the honji-suijaku framework through temple-shrine complexes (jingu-ji), designating kami as provisional local manifestations (suijaku) of Buddhist original grounds (honji). Controls doctrinal interpretation, ritual calendars, and often the land and revenue attached to combined shrine-temple institutions. Sets the terms under which kami cults are permitted to persist — as subordinate, explained phenomena within a Buddhist cosmological hierarchy.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, buddhist_temple_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, buddhist_temple_hierarchy, beneficiary).

% Supply the metaphysical architecture (esoteric Buddhist cosmology, mandala systems) that renders kami legible as buddhas-in-disguise. Gains court patronage, land grants, and interpretive authority over shrine practice by being the schools whose doctrine explains what kami 'really are.' Their institutional standing depends on the honji-suijaku mapping remaining the accepted account.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, shingon_tendai_doctrinal_schools, beneficiary,
    institutional, civilizational, arbitrage, national).

% Preside over kami worship at shrines increasingly absorbed into jingu-ji complexes where Buddhist clergy hold interpretive and often administrative seniority. Cannot articulate kami's significance in terms outside the honji-suijaku frame without risking institutional marginalization or loss of patronage. Some shrine lineages retain local prestige, but the cosmological account of what they serve is authored elsewhere.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, shinto_shrine_priesthoods, payer,
    moderate, generational, constrained, regional).

% Village- and clan-level kami veneration predating and structurally independent of Buddhist cosmology gets reframed as an immature or provisional expression of a deeper Buddhist truth. Practitioners have no institutional channel to contest the reframing; their kami's autonomy as an independent object of devotion is dissolved into a subordinate position within someone else's cosmological order.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, local_kami_cults, payer,
    powerless, generational, trapped, local).

% Sponsors combinatory shrine-temple institutions and legitimates the syncretic reading because it unifies a fragmentary religious landscape into a single cosmology that mirrors and supports centralized rule (the state as microcosm of an integrated sacred order). Benefits from doctrinal coherence as a tool of political consolidation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, imperial_court_patrons, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, imperial_court_patrons, agenda_setter).

% Examine temple records, ritual manuals, and shrine chronicles across centuries to assess whether the honji-suijaku framework represented genuine doctrinal integration, a tolerated institutional fiction, or a mapping that varied by site and era. Their reconstructions are contested and shape which reading of the kernel gets treated as historically dominant.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, historians_of_religion, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, teachable cosmological grammar that lets diverse local kami cults and translocal Buddhist institutions operate within one interpretive system — pilgrims, patrons, and administrators can navigate shrine-temple complexes using one shared framework instead of negotiating incommensurable religious claims site by site.
% TRANSFER_FUNCTION: Moves interpretive authority, land revenue, and cosmological primacy from autonomous kami cults and shrine priesthoods to Buddhist doctrinal schools and the jingu-ji administrative apparatus, in exchange for continued (subordinated) ritual legitimacy for the kami side.
% ABSENT_VOICES: Local kami-cult practitioners whose traditions predate and are structurally independent of Buddhist cosmology have no doctrinal seat from which to insist their kami are not provisional manifestations of anything else; their objection, where it exists, survives mainly in localized ritual practice rather than in the textual record that the syncretic reading is built from.
% DISAPPEARANCE_RATIONALE: If the syncretic honji-suijaku framework vanished overnight, Buddhist institutions would lose a major interpretive claim over shrine administration and associated revenue, and jingu-ji complexes would face pressure to separate into independent Buddhist and Shinto institutions (as the later shinbutsu bunri policy actually forced). Whether the 'world' — meaning everyday ritual practice at the local level — would rearrange much is disputed: some shrine-temple sites might have continued functionally similar practice under a different or no unifying cosmological label, which is part of why sibling readings (partition, incoherence) exist.
% FOUNDING_PROBLEM: Early esoteric Buddhist missionizing in Japan faced an existing, entrenched, locally legitimate kami cult system it could not simply displace; honji-suijaku metaphysics was constructed to explain kami as compatible with (and subordinate to) Buddhist cosmology rather than requiring their elimination or leaving the traditions doctrinally unreconciled.
% FOUNDING_PROBLEM_CORROBORATION: Meiji-era shinbutsu bunri (separation) policymakers and modern historians of religion outside both the Buddhist and Shinto institutional lineages attest that the missionizing-era problem of doctrinal accommodation no longer applies to a Shinto establishment now legally and institutionally independent of Buddhist hierarchy; the syncretic framework's continued invocation in some traditionalist and academic contexts persists as an interpretive heritage claim rather than as a live solution to an active coexistence problem. No corroboration from a source independent of Buddhist doctrinal interest fully endorses the ongoing metaphysical necessity of the unified reading.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__syncretic_reading, contested).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__syncretic_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__syncretic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__syncretic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__syncretic_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects a substantial but not total transfer: the syncretic reading does deliver a genuine coordination good (a workable shared framework for combinatory institutions across a fragmented religious landscape), but the terms of that coordination consistently place Buddhist cosmology as explanatorily prior, which is a real asymmetry, not a neutral synthesis. Suppression (0.58) is moderate-high: kami cults are not violently suppressed, but their capacity to assert independent ontological standing is structurally foreclosed by which doctrinal apparatus controls textual production, temple administration, and court patronage. Theater ratio (0.42) rises over the measured interval as the coordination function (explaining religious diversity) increasingly serves an administrative/legitimating function (justifying jingu-ji land and revenue arrangements) rather than doing live theological work — by the later interval, the doctrine is as much institutional load-bearing structure as it is believed cosmology. Accessibility collapse (0.5) and resistance (0.45) sit at moderate values because, unlike a mountain, alternative framings (partition, incoherence) remained live throughout the historical record — the syncretic reading never fully closed off rival accounts, which is precisely why this kernel supports three distinct sibling readings rather than one settled truth.
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist doctrinal seat, the syncretic reading is simply correct cosmology, coordinating an otherwise incoherent religious landscape into one intelligible order. From the local kami cult seat, the same doctrine is experienced as an imposed hierarchy that erases the independence of what was previously a self-standing devotional object. The engine's per-seat computation should reflect this: agenda_setter/beneficiary seats compute low effective extraction from their own structural position even as payer/target seats compute high effective extraction from the identical arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist temple hierarchy and doctrinal schools sit near the full-beneficiary end: they administer the framework, collect the associated revenue and prestige, and face no exit cost since the framework is their own construction. Imperial patrons similarly benefit, using cosmological coherence instrumentally for political consolidation, with institutional exit options (arbitrage) since their stake is political rather than devotional. Shinto shrine priesthoods are constrained payers — real losses in interpretive authority, but not trapped, since some negotiate continued local standing. Local kami cults are the clearest targets: powerless, trapped by lack of institutional voice, bearing the full cost of losing independent ontological standing with no arbitrage option available to them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling an entrenched local kami cult system with an incoming translocal Buddhist missionary project — is historically dead: Meiji-era shinbutsu bunri legally and institutionally separated the two traditions, and no coexistence crisis currently requires a unifying cosmology. Yet the syncretic reading persists in traditionalist and some academic contexts as inherited doctrinal architecture. Classifying this as tangled_rope rather than snare or mountain prevents two mislabelings: it is not pure extraction (there was and to some degree remains a genuine coordination function — a shared interpretive grammar for combinatory religious sites), and it is not natural cosmological fact (the metaphysics is a historically specific doctrinal construction with identifiable authors, beneficiaries, and a documented alternative history in which it did not hold everywhere or at all times, per the sibling readings).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    syncretic_vs_partition_ontological_status,
    'Did premodern institutional actors actually hold the honji-suijaku mapping as a genuine metaphysical claim (kami ARE buddha-manifestations), or did shrine-temple complexes operate it as a practical administrative convenience while treating Shinto and Buddhist domains as functionally separate (the partition reading)?',
    'Close textual analysis of ritual manuals, doctrinal treatises, and shrine-temple administrative records across different regions and periods, distinguishing genuinely held cosmological claims from pragmatic institutional arrangements described in cosmological language.',
    'If the record supports partition rather than integration, this story''s claimed extraction (subordination of kami to Buddhist cosmological hierarchy) overstates what actually occurred, and the correct reading for most sites/periods would be partition_reading rather than syncretic_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syncretic_vs_partition_ontological_status, conceptual, 'Whether historical practice supports genuine ontological integration versus functional domain separation.').

omega_variable(
    kernel_framing_under_determination,
    'Is honji-suijaku better modeled as a single contested kernel with three ontologically distinct readings (as this story assumes), or as a spectrum of site-specific and period-specific practices that resist reduction to any of the three canonical readings?',
    'Comparative survey of jingu-ji institutional records across multiple centuries and regions, checking whether individual sites cluster cleanly into one of the three readings or exhibit hybrid/shifting patterns that no single reading captures.',
    'If practice is genuinely heterogeneous and non-clustering, the three-reading decomposition itself may need a fourth reading or a different kernel structure; if sites cluster cleanly, the current three-way decomposition is validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether the three declared readings exhaust or oversimplify the historical variation in shinbutsu-shugo practice.').

omega_variable(
    extraction_vs_genuine_belief,
    'To the extent Buddhist institutional actors genuinely believed the syncretic cosmology (rather than merely administering it for institutional gain), does that reduce the extractiveness of the arrangement, or is extraction structurally present regardless of sincerity of belief?',
    'This is not fully resolvable empirically — belief states of historical actors are not directly observable — but comparative study of doctrinal writing by authors with and without institutional stake in the outcome (itinerant vs. beneficed clergy) could partially triangulate sincerity versus institutional interest.',
    'If belief was widespread and sincere across all strata including non-beneficiary actors, the tangled_rope classification''s asymmetric-extraction component weakens; if belief tracked institutional position closely, it strengthens the extraction reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_genuine_belief, preference, 'Whether sincere belief in the syncretic cosmology mitigates or is orthogonal to its structural extractiveness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__syncretic_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(shin_tr_t0, projected).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 200, 0.22).
narrative_ontology:measurement_basis(shin_tr_t200, projected).
narrative_ontology:measurement(shin_tr_t400, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 400, 0.3).
narrative_ontology:measurement_basis(shin_tr_t400, projected).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 600, 0.35).
narrative_ontology:measurement_basis(shin_tr_t600, projected).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 900, 0.4).
narrative_ontology:measurement_basis(shin_tr_t900, projected).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 1200, 0.42).
narrative_ontology:measurement_basis(shin_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(shin_be_t0, projected).
narrative_ontology:measurement(shin_be_t200, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 200, 0.45).
narrative_ontology:measurement_basis(shin_be_t200, projected).
narrative_ontology:measurement(shin_be_t400, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 400, 0.55).
narrative_ontology:measurement_basis(shin_be_t400, projected).
narrative_ontology:measurement(shin_be_t600, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 600, 0.6).
narrative_ontology:measurement_basis(shin_be_t600, projected).
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 900, 0.62).
narrative_ontology:measurement_basis(shin_be_t900, projected).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 1200, 0.62).
narrative_ontology:measurement_basis(shin_be_t1200, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(shin_su_t0, projected).
narrative_ontology:measurement(shin_su_t200, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 200, 0.48).
narrative_ontology:measurement_basis(shin_su_t200, projected).
narrative_ontology:measurement(shin_su_t400, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 400, 0.55).
narrative_ontology:measurement_basis(shin_su_t400, projected).
narrative_ontology:measurement(shin_su_t600, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 600, 0.58).
narrative_ontology:measurement_basis(shin_su_t600, projected).
narrative_ontology:measurement(shin_su_t900, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 900, 0.57).
narrative_ontology:measurement_basis(shin_su_t900, projected).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 1200, 0.58).
narrative_ontology:measurement_basis(shin_su_t1200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__syncretic_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the shinbutsu_ontological_commitment kernel, decomposed per the ε-invariance principle because the natural-language label 'honji-suijaku' / 'shinbutsu-shugo' covers structurally distinct claims about kami-buddha ontology that different historical actors and modern historians hold with different confidence and different implications for institutional beneficiary structure. The syncretic reading (this story) claims genuine ontological integration with Buddhist cosmological primacy — highest institutional integration, highest doctrinal coherence claim, clearest beneficiary/victim asymmetry (tangled_rope). The partition reading claims separate non-overlapping domains without integration — lower institutional stakes, likely lower ε. The incoherence reading claims no stable commitment existed at all — institutionally tolerated ambiguity rather than doctrine, which likely reads closer to piton or a low-coordination rope depending on whether the tolerated incoherence itself served an institutional function. Each story authors its own ε, its own stakeholder set, and its own claimed_type; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
