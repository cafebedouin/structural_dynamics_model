% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__amun_polytheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__amun_polytheistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__amun_polytheistic_reading
 *   human_readable: Amun-Ra Priestly Legitimation of Pharaonic Divine Authority
 *   domain: religious/political economy of belief systems
 *
 * SUMMARY:
 *   This story instantiates the Amun-Ra polytheistic reading of the divine
 *   legitimacy substrate kernel: legitimacy is distributed through an
 *   established priestly interpretive apparatus operating over a multi-deity
 *   cosmology with Amun-Ra installed as chief patron among many recognized
 *   gods. This reading is structurally distinct from the Atenist monotheistic
 *   reading (pharaoh-as-sole-revealer, all other cults delegitimized) and the
 *   folk syncretistic reading (household/village-level pragmatic multi-deity
 *   practice largely outside elite priestly control). Each reading is its own
 *   constraint with its own beneficiary structure, its own epsilon, and its
 *   own victims — they are linked here only through
 *   network.affects_constraints and are not blended into this file.
 *
 * KEY AGENTS:
 *   - amun_priesthood: interpretive monopolist and chief beneficiary (institutional/arbitrage)
 *   - karnak_temple_economy: accumulating institutional beneficiary (institutional/arbitrage)
 *   - pharaoh_when_validated: constrained beneficiary, funds the apparatus that sanctions him (powerful/constrained)
 *   - pharaoh_when_denied_validation: same nominal power, trapped by the same structure when validation is withheld
 *   - peasant_taxpayers: powerless payer, funds the system with no interpretive voice
 *   - rival_regional_cults: excluded subordinate cults, folded into the hierarchy rather than granted independent standing
 *   - modern_egyptologists: analytical observer reconstructing the economic and interpretive apparatus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, 0.62).
domain_priors:suppression_score(divine_legitimacy_substrate__amun_polytheistic_reading, 0.58).
domain_priors:theater_ratio(divine_legitimacy_substrate__amun_polytheistic_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__amun_polytheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__amun_polytheistic_reading, "Amun-Ra Priestly Legitimation of Pharaonic Divine Authority").
narrative_ontology:topic_domain(divine_legitimacy_substrate__amun_polytheistic_reading, "religious/political economy of belief systems").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__amun_polytheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__amun_polytheistic_reading, '96022535-62a7-4d96-a174-90f71a23ebd6').
narrative_ontology:cs_kernel_codification('96022535-62a7-4d96-a174-90f71a23ebd6', distributed).
narrative_ontology:cs_authority_grounding('96022535-62a7-4d96-a174-90f71a23ebd6', lineage).
narrative_ontology:cs_interpretation_layer_present('96022535-62a7-4d96-a174-90f71a23ebd6').
narrative_ontology:cs_reading_relation('96022535-62a7-4d96-a174-90f71a23ebd6', divine_legitimacy_substrate__atenist_monotheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('96022535-62a7-4d96-a174-90f71a23ebd6', divine_legitimacy_substrate__folk_syncretistic_reading, coexists_with).
narrative_ontology:cs_axiom('96022535-62a7-4d96-a174-90f71a23ebd6', foundational, plural_divine_manifestation_is_legitimate).
narrative_ontology:cs_axiom_status(plural_divine_manifestation_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('96022535-62a7-4d96-a174-90f71a23ebd6', plural_divine_manifestation_is_legitimate, theological).
narrative_ontology:cs_axiom('96022535-62a7-4d96-a174-90f71a23ebd6', foundational, trained_priestly_lineage_alone_may_authoritatively_interpret_divine_will).
narrative_ontology:cs_axiom_status(trained_priestly_lineage_alone_may_authoritatively_interpret_divine_will, holdable).
narrative_ontology:cs_axiom_grounding('96022535-62a7-4d96-a174-90f71a23ebd6', trained_priestly_lineage_alone_may_authoritatively_interpret_divine_will, conventional).
narrative_ontology:cs_reference_frame('96022535-62a7-4d96-a174-90f71a23ebd6', middle_kingdom_amun_thebes_ascendancy).
narrative_ontology:cs_drift_state('96022535-62a7-4d96-a174-90f71a23ebd6', late_new_kingdom_temple_wealth_peak, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('96022535-62a7-4d96-a174-90f71a23ebd6', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, karnak_temple_economy).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh_when_validated).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, peasant_taxpayers).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, rival_regional_cults).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh_when_denied_validation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh_when_validated).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, maat_cosmic_order_doctrine).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, amun_ra_supremacy_among_gods).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the interpretive apparatus that determines which royal acts, building programs, and successions count as divinely sanctioned. Administers oracular pronouncements, temple treasuries, and the ritual calendar. Can withhold or grant legitimacy to a pharaoh by interpreting Amun's will, and collects land grants, labor levies, and tribute as the price of that validation.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood, beneficiary).

% Accumulates vast landholdings, granaries, workshops, and workforces under the cover of maintaining the god's cult. Grows wealthier as the legitimation function is exercised more often and more visibly; has no incentive to see the coordination function shrink.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, karnak_temple_economy, beneficiary,
    institutional, generational, arbitrage, national).

% Receives cosmic sanction for rule, military campaigns, and succession by aligning with priestly interpretation of Amun's favor. Must fund temple construction, endowments, and festivals to sustain that sanction, and cannot simply declare legitimacy unilaterally — the interpretive apparatus stands between the throne and divine warrant.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh_when_validated, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh_when_validated, payer).

% A ruler whose succession is contested, whose campaigns fail, or whose policies threaten priestly prerogatives can find oracles and interpretations turning against them. Exit from the arrangement is nearly impossible without either destroying the priesthood's power base or ruling in open defiance of the cosmological order the throne itself depends on for legitimacy.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh_when_denied_validation, payer,
    powerful, biographical, trapped, national).

% Provide the grain, labor corvee, and goods that fund temple estates and festivals, understood as their contribution to maintaining cosmic order (maat). Have no voice in how divine will is interpreted and no realistic alternative source of legitimate religious authority in their region.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, peasant_taxpayers, payer,
    powerless, biographical, trapped, local).

% Local and regional deities (Ptah at Memphis, Ra at Heliopolis, various nome gods) are folded into the Amun-Ra syncretic hierarchy as subordinate manifestations rather than treated as independent sources of legitimacy. Their priesthoods can gain some standing by alliance but cannot challenge Amun's chief-patron status without risking exclusion from state patronage.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, rival_regional_cults, excluded,
    moderate, generational, constrained, regional).

% Reconstruct the interpretive and economic apparatus from temple records, stelae, and administrative papyri, assessing how much of the cosmological claim tracked genuine coordination function versus institutional rent extraction over centuries.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, modern_egyptologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__amun_polytheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared cosmological framework (maat, the pantheon, Amun-Ra's chief patronage) that lets a vast, ethnically and regionally diverse population, plus a hereditary monarchy, agree on what counts as legitimate rule, succession, and public order — solving a genuine coordination problem in a pre-modern state without centralized bureaucratic legitimation.
% TRANSFER_FUNCTION: Moves land, grain, labor, and tribute from peasant taxpayers and the royal treasury into temple estates and priestly households, in exchange for interpretive validation of pharaonic authority and cosmic order; also moves political leverage from the throne to the priesthood whenever royal policy depends on priestly cooperation.
% ABSENT_VOICES: Peasant taxpayers who fund the system have no interpretive standing and are not part of the conversation about what divine will requires. Rival regional cults are structurally subordinated and would, if empowered, argue for polycentric or locally autonomous legitimation rather than a single chief-patron hierarchy.
% DISAPPEARANCE_RATIONALE: If the Amun priesthood's interpretive monopoly vanished overnight, pharaonic succession disputes would lose their primary adjudication mechanism, temple estates would face expropriation or fragmentation, regional cults would compete openly for state patronage, and the throne would need to construct an entirely different legitimation apparatus — precisely what happened, in compressed and violent form, during the Amarna period.
% FOUNDING_PROBLEM: Early dynastic and Middle Kingdom Egypt needed a stable, transmissible mechanism for legitimating pharaonic authority across successions, military crises, and regional integration, without relying solely on force; a shared cosmology interpreted by a specialized priestly class solved this by making legitimacy a matter of ritual and interpretive correctness rather than raw dynastic assertion.
% FOUNDING_PROBLEM_CORROBORATION: The Amun priesthood and its administrative records attest the founding problem (cosmic order requires priestly stewardship) as permanently live. Independent evidence — the Amarna interlude's attempt to abolish the entire apparatus, and later Ramesside-era administrative records showing temple estates functioning as an autonomous economic power center largely decoupled from active legitimation crises — supports a reading, corroborated by modern Egyptological economic-archive analysis outside the priesthood's own record-keeping, that by the New Kingdom's later phases the arrangement had substantially shifted from crisis-legitimation toward institutionalized rent extraction.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__amun_polytheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__amun_polytheistic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__amun_polytheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__amun_polytheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 by the end of the interval: substantial but not maximal, because the coordination function (a shared legitimation framework across a large diverse population) is genuinely real, not merely cover. Suppression at 0.58 reflects real but partial coercive backing — the priesthood's power rests more on interpretive gatekeeping and economic leverage than on direct violence, though both were available. Theater ratio rises from 0.20 to 0.45 over five centuries as festival and oracular apparatus grows more elaborate relative to any live succession-legitimation crisis it addresses, consistent with institutional entrenchment. Accessibility collapse is moderate (0.50): a pharaoh in extremis (Akhenaten) could and did attempt to bypass the apparatus entirely, showing alternatives were not fully foreclosed, though doing so provoked massive resistance. Resistance is moderate (0.40), reflecting periodic royal pushback (culminating in the Amarna rupture) without becoming the norm.
 *
 * PERSPECTIVAL GAP:
 *   From the Amun priesthood's seat, the arrangement is coordination they steward faithfully — cosmic order requires trained interpreters, and their wealth is the legitimate price of that service. From the peasant taxpayer's seat, the same structure is an unquestionable tax obligation justified by a cosmology they have no say in interpreting. From the pharaoh's seat, the experience bifurcates sharply depending on whether validation is granted or withheld — the same nominal power level (powerful) produces radically different structural experiences (constrained beneficiary vs. trapped payer) depending on the priesthood's interpretive verdict, which is the seat-divergence this story is built to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   The Amun priesthood and Karnak's temple economy sit near the full-beneficiary end: they administer the interpretive apparatus and capture its rents (arbitrage exit, institutional power). The pharaoh occupies a genuinely split position — validated, the throne benefits from cosmic sanction; denied validation (succession disputes, failed campaigns, policy conflicts with temple interests), the same structural apparatus becomes a trap the pharaoh cannot easily exit without dismantling the entire legitimation basis of the monarchy itself. Peasant taxpayers are structural payers with no interpretive standing and no exit (trapped, powerless) — they fund the coordination function without access to its benefits or its meaning-making authority. Rival regional cults are subordinated rather than actively persecuted in this reading, consistent with polytheistic accommodation, but excluded from chief-patron status and therefore from the largest share of state patronage.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legitimating a hereditary monarchy's authority across successions and crises without pure reliance on force — is genuinely old and was genuinely real. Classifying this as tangled_rope (not snare) preserves the fact that the interpretive apparatus solved a real coordination problem for a large state; classifying it as tangled_rope rather than pure rope preserves the fact that a specific institutional actor (the Amun priesthood) captured disproportionate, growing rents through that same apparatus, extracting from peasant taxpayers and constraining even the pharaoh who nominally sat atop the hierarchy. The theater_ratio's rise over the interval documents the coordination function eroding relative to rent-extraction — by the later New Kingdom, this reading suggests the coordination problem was largely solved (a working framework existed) but the apparatus persisted and expanded because the priesthood benefited from its continuation, which is exactly the tangled-rope signature the classification is designed to catch rather than mislabel as either pure Mountain-like natural order or pure Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_ratio_over_time,
    'At what point in the New Kingdom did the Amun priesthood''s interpretive apparatus shift from primarily solving a genuine succession-legitimation coordination problem to primarily functioning as an entrenched rent-extraction mechanism riding on that original function?',
    'Comparative analysis of temple land-grant records, corvee labor allocation, and succession-crisis frequency across dynastic periods; a declining correlation between recorded legitimation crises and temple economic growth would support the entrenchment reading.',
    'An early shift would support classifying most of the interval as tangled_rope trending toward snare; a late shift would support tangled_rope holding stable for most of the period with only late-period drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_ratio_over_time, empirical, 'Timing of the coordination-to-extraction balance shift in the priestly apparatus.').

omega_variable(
    priestly_authority_as_genuine_natural_cosmology_or_constructed_institution,
    'Within this reading''s own framework, is the maat-based multi-deity cosmology treated by its adherents as a discovered feature of reality (a mountain-like natural order) or as a constructed institutional arrangement that happens to benefit the priesthood who maintain it?',
    'Textual analysis of priestly self-justification (temple inscriptions, wisdom literature) versus external/comparative accounts (Amarna-period polemics, later Greek observers) for evidence of whether contemporaries within the tradition itself recognized the constructed, interest-laden character of the interpretive monopoly.',
    'If the tradition''s own adherents recognized construction, this strengthens the tangled_rope classification against any naturalized mountain-like framing the priesthood may have offered; if genuinely believed as natural cosmic order by all parties including priests, the classification remains tangled_rope but the suppression component shifts from cynical enforcement toward good-faith belief enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(priestly_authority_as_genuine_natural_cosmology_or_constructed_institution, conceptual, 'Whether the cosmology was self-consciously constructed or genuinely believed as natural order, including by its administrators.').

omega_variable(
    reading_boundary_disagreement_location,
    'Where exactly does the amun_polytheistic_reading''s account of priestly interpretive authority diverge from the folk_syncretistic_reading''s account of household-level practice — is this a difference of social stratum (elite vs. commoner) or a genuine difference in the underlying legitimacy mechanism?',
    'Comparative analysis of household shrine archaeology and village-level religious practice records against state temple records for the same time periods and regions.',
    'If the difference is purely stratum (same underlying mechanism, different scale), the two readings may be more continuous than the kernel contest suggests; if genuinely different mechanisms (institutional interpretive monopoly vs. pragmatic household bricolage), the readings remain properly distinct constraints as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_disagreement_location, conceptual, 'Whether the amun and folk-syncretistic readings differ by social stratum or by underlying legitimacy mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__amun_polytheistic_reading, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(divi_tr_t100, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 100, 0.28).
narrative_ontology:measurement(divi_tr_t200, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 200, 0.34).
narrative_ontology:measurement(divi_tr_t300, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 300, 0.38).
narrative_ontology:measurement(divi_tr_t400, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 400, 0.42).
narrative_ontology:measurement(divi_tr_t500, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 500, 0.45).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(divi_be_t100, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 100, 0.42).
narrative_ontology:measurement(divi_be_t200, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 200, 0.5).
narrative_ontology:measurement(divi_be_t300, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 300, 0.56).
narrative_ontology:measurement(divi_be_t400, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 400, 0.6).
narrative_ontology:measurement(divi_be_t500, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 500, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(divi_su_t100, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 100, 0.44).
narrative_ontology:measurement(divi_su_t200, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 200, 0.48).
narrative_ontology:measurement(divi_su_t300, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 300, 0.52).
narrative_ontology:measurement(divi_su_t400, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 400, 0.55).
narrative_ontology:measurement(divi_su_t500, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 500, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__amun_polytheistic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__amun_polytheistic_reading, 0.08).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, atenist_monotheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the divine_legitimacy_substrate kernel. atenist_monotheistic_reading inverts the beneficiary structure entirely (throne captures interpretive authority, priesthood is dispossessed) and would show a markedly different epsilon and victim set (the Amun priesthood becomes the victim class). folk_syncretistic_reading operates largely outside this constraint's institutional economy, at household/village scale, with different power atoms throughout (mostly powerless/moderate agents, no institutional temple economy). All three are linked here for contamination-propagation analysis: a shift in one reading's legitimacy (e.g., the historical Amarna interruption, which is the atenist reading's high-water mark) directly destabilizes this reading's temple-economy beneficiary structure, and the subsequent Ramesside restoration is this reading's re-assertion against the atenist attempt.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
