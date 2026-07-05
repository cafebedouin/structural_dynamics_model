% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__amun_polytheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Amun-Ra Priestly Interpretive Legitimacy (New Kingdom Polytheistic Reading)
 *   domain: ancient_history/religious_studies/political_economy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the divine_legitimacy_substrate
 *   kernel: the Amun-Ra-centered polytheistic reading in which divine
 *   sanction for pharaonic rule flows through established priestly
 *   interpretation of a multi-deity cosmology, with Amun-Ra elevated to chief
 *   patron but coexisting with an accommodative pantheon (Ptah, Re, Osiris,
 *   and local deities absorbed as aspects). This is structurally distinct
 *   from the atenist_monotheistic_reading (sole pharaonic revelation,
 *   exclusive deity, no interpretive intermediary) and the
 *   folk_syncretistic_reading (household/village pragmatic multi-deity
 *   practice with no centralized priestly authority). Under this reading,
 *   interpretive authority is DISTRIBUTED across a professional priesthood
 *   whose material base is temple land and grain, legitimacy accommodates
 *   regional cultic variation by subordinating rather than eliminating rival
 *   cults, and the pharaoh is meaningfully CONSTRAINED by the need for
 *   continuous priestly validation — a structurally different power relation
 *   from either sibling reading. ε for this reading (0.62 by interval end)
 *   reflects the temple economy's substantial and growing capture of land and
 *   labor over the New Kingdom; this value is NOT averaged against the
 *   siblings' ε values, per the ε-invariance principle — each reading is its
 *   own constraint.
 *
 * KEY AGENTS:
 *   - amun_priesthood_karnak: primary agenda-setter and beneficiary (institutional/arbitrage) — administers oracle and temple economy
 *   - pharaoh: constrained beneficiary/payer (powerful/constrained) — legitimacy dependent on priestly validation
 *   - peasant_taxpayers and corvee_laborers: primary targets (powerless/trapped) — fund the temple economy through tithe and conscription
 *   - minor_regional_priesthoods: excluded voice — subordinated rather than consulted
 *   - modern_egyptologists: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, 0.62).
domain_priors:suppression_score(divine_legitimacy_substrate__amun_polytheistic_reading, 0.48).
domain_priors:theater_ratio(divine_legitimacy_substrate__amun_polytheistic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__amun_polytheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__amun_polytheistic_reading, "Amun-Ra Priestly Interpretive Legitimacy (New Kingdom Polytheistic Reading)").
narrative_ontology:topic_domain(divine_legitimacy_substrate__amun_polytheistic_reading, "ancient_history/religious_studies/political_economy").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__amun_polytheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__amun_polytheistic_reading, '93130980-0084-46af-8d02-011602f3bdeb').
narrative_ontology:cs_kernel_codification('93130980-0084-46af-8d02-011602f3bdeb', distributed).
narrative_ontology:cs_authority_grounding('93130980-0084-46af-8d02-011602f3bdeb', practice).
narrative_ontology:cs_interpretation_layer_present('93130980-0084-46af-8d02-011602f3bdeb').
narrative_ontology:cs_reading_relation('93130980-0084-46af-8d02-011602f3bdeb', divine_legitimacy_substrate__atenist_monotheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('93130980-0084-46af-8d02-011602f3bdeb', divine_legitimacy_substrate__folk_syncretistic_reading, influences).
narrative_ontology:cs_axiom('93130980-0084-46af-8d02-011602f3bdeb', foundational, divine_multiplicity_is_cosmically_true).
narrative_ontology:cs_axiom_status(divine_multiplicity_is_cosmically_true, holdable).
narrative_ontology:cs_axiom_grounding('93130980-0084-46af-8d02-011602f3bdeb', divine_multiplicity_is_cosmically_true, theological).
narrative_ontology:cs_axiom('93130980-0084-46af-8d02-011602f3bdeb', foundational, priestly_professional_class_mediates_divine_will).
narrative_ontology:cs_axiom_status(priestly_professional_class_mediates_divine_will, holdable).
narrative_ontology:cs_axiom_grounding('93130980-0084-46af-8d02-011602f3bdeb', priestly_professional_class_mediates_divine_will, conventional).
narrative_ontology:cs_reference_frame('93130980-0084-46af-8d02-011602f3bdeb', old_kingdom_solar_osirian_synthesis).
narrative_ontology:cs_drift_state('93130980-0084-46af-8d02-011602f3bdeb', late_new_kingdom_ramesside_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('93130980-0084-46af-8d02-011602f3bdeb', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood_karnak).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, temple_estates_and_granaries).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, provincial_cult_centers).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, peasant_taxpayers).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, corvee_laborers).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, minor_regional_priesthoods).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, provincial_cult_centers).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, maat_cosmic_order_doctrine).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_divine_sonship_of_amun).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the oracle of Amun at Karnak, interprets omens and dream-visions as confirmation or withdrawal of divine favor from the reigning pharaoh, and controls vast temple lands, granaries, and labor levies granted in Amun's name. Sets the terms under which a pharaoh's claim to rule is validated, and can withhold or grant that validation. Its own wealth grows in direct proportion to how much of the surplus economy is routed through temple administration.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood_karnak, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood_karnak, beneficiary).

% Claims descent from Amun-Ra and rules by divine mandate, but that mandate is legible to the populace only through priestly ritual, oracular pronouncement, and temple-controlled coronation rites. Grants land and labor to temples to secure and renew legitimacy, which shrinks the crown's independent tax base over generations. Could in principle found a rival cult (as Akhenaten later attempted) but doing so risks provoking the very interpretive apparatus that manufactures consent among the literate and administrative classes.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, beneficiary).

% Accumulate land grants, tax exemptions, and captured labor as the material substrate of the priestly interpretive monopoly. By the late New Kingdom hold a substantial fraction of cultivable land and control grain reserves that function as a shadow treasury independent of the pharaonic state.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, temple_estates_and_granaries, beneficiary,
    institutional, generational, arbitrage, national).

% Local temples to other deities (Ptah at Memphis, Re at Heliopolis, Osiris at Abydos) are absorbed into the Amun-centered cosmology as aspects or consorts of the chief patron, which lets them retain local revenue and prestige at the cost of theological subordination and periodic tribute to the Karnak establishment.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, provincial_cult_centers, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, provincial_cult_centers, payer).

% Pay grain tithes and dues that flow substantially to temple estates rather than the crown, and are told this arrangement secures cosmic order (maat) and the Nile's annual flood. Have no institutional voice in whether the priestly reading of divine will is accurate; their alternative would be to doubt a cosmology so total that doubt itself has no vocabulary in their world.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, peasant_taxpayers, payer,
    powerless, biographical, trapped, local).

% Conscripted for temple construction, canal maintenance on temple lands, and transport of temple goods, framed as devotional service to Amun-Ra rather than taxation in kind. Cannot refuse without defying both state and divine order simultaneously, since the two are the same structure.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, corvee_laborers, payer,
    powerless, biographical, trapped, local).

% Local cults with older or rival claims to cosmological primacy are folded into the Amun-Ra syncretic hierarchy or marginalized entirely; their independent theological voice is not part of the negotiation over what counts as legitimate divine sanction — the Karnak interpretation simply absorbs or outcompetes them for royal patronage.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, minor_regional_priesthoods, excluded,
    moderate, generational, constrained, regional).

% Reconstruct the economic and political function of the Amun cult from temple inventories, tomb inscriptions, and administrative papyri, distinguishing the coordination functions (calendrical ritual, famine-buffering grain storage) from the extractive functions (land accumulation, priestly political leverage over succession).
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, modern_egyptologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__amun_polytheistic_reading, amun_priesthood_karnak).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__amun_polytheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared cosmological framework — maat, the Nile flood cycle tied to divine favor, a coherent ritual calendar — that coordinates agricultural planning, succession legitimacy, and inter-regional cultic alliance across a large and otherwise fragmented river-valley polity, and stores real surplus (temple granaries) against famine years.
% TRANSFER_FUNCTION: Moves grain, land, and conscripted labor from peasant taxpayers and corvee laborers upward into temple estates administered by the Amun priesthood at Karnak, in exchange for the priesthood's continued ritual certification of the pharaoh's divine sonship and the cosmic order that legitimizes taxation itself.
% ABSENT_VOICES: Minor regional priesthoods with older, non-Amun cosmological claims are absorbed or sidelined rather than consulted; peasant and laborer classes who fund the entire structure have no interpretive standing to contest a priestly reading of divine will, since the vocabulary of legitimate objection does not exist outside the cosmology itself.
% DISAPPEARANCE_RATIONALE: If Amun priestly interpretive authority vanished, the pharaonic succession mechanism would lose its primary legitimation channel overnight (as nearly happened under Akhenaten), temple estates would face expropriation or fragmentation, and the grain-storage/famine-buffering function would need to be re-institutionalized under direct state administration rather than temple administration — a genuine reorganization of Egypt's political economy, not a return to a prior natural state.
% FOUNDING_PROBLEM: Early dynastic and pre-dynastic Egypt lacked a unifying ideological apparatus binding a long, ecologically diverse river valley into a single polity with a stable succession mechanism and a buffer against Nile flood variability.
% FOUNDING_PROBLEM_CORROBORATION: The Karnak priesthood's own inscriptions attest the founding problem (cosmic/agricultural order) as permanently live and requiring their perpetual mediation. Independent corroboration outside the beneficiary class is thin for this period, but comparative administrative-papyri analysis by modern Egyptologists (grain-account discrepancies, land-survey records showing temple acreage outpacing crown acreage over centuries) supports the reading that the original coordination problem was substantially solved early and the apparatus persisted primarily as an extraction and legitimation mechanism thereafter — no fully independent ancient voice exists to corroborate or refute this, since literacy and record-keeping were themselves largely temple functions.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__amun_polytheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__amun_polytheistic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.35 to 0.62 across the New Kingdom interval as temple land holdings accumulate relative to crown holdings — a well-documented pattern in surviving land-survey and grain-account papyri. Suppression is moderate (0.48) rather than high because the coercive force operates mostly through cosmological totality (no vocabulary for dissent) rather than direct violence — comparable to internalized rather than purely structural suppression. Theater ratio rises to 0.4 as oracular ritual increasingly serves to launder existing power arrangements (late-New-Kingdom oracles are documented settling secular administrative and legal disputes, a function drift from pure religious validation toward priestly juridical authority).
 *
 * DIRECTIONALITY LOGIC:
 *   The Amun priesthood and temple estates sit near the full-beneficiary end: they administer the interpretive apparatus and their wealth scales with its operation, with arbitrage-grade exit (they survive dynastic transitions, unlike individual pharaohs). Peasant taxpayers and corvee laborers sit near the full-target end: trapped exit, no interpretive standing, and the tithe/labor extraction flows through the same cosmological structure that is presented as coordination (flood prediction, maat, succession stability). The pharaoh occupies a genuinely mixed position — beneficiary of the legitimacy the system confers, but structurally constrained (payer of land grants, dependent on priestly cooperation) in a way that differentiates this reading sharply from the atenist reading, where the pharaoh IS the sole interpretive authority with no comparable constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (agricultural/cosmic order coordination across a fragmented river valley) shows signs of having been substantially solved early, while the interpretive apparatus continued to expand its material claims for centuries afterward — the classic mandatrophy signature. Classifying this as tangled_rope rather than pure snare preserves the genuine coordination function (calendrical/ritual synchronization, famine-buffering grain storage) that a pure-extraction reading would erase, while the requires_active_enforcement flag and named victims capture that the coordination function alone does not explain the scale of land and labor capture observed in the archaeological and papyrological record.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_amun_vs_atenist_foreclosure,
    'Does the amun_polytheistic_reading''s premise of distributed priestly interpretive authority logically foreclose the atenist_monotheistic_reading''s premise of sole pharaonic revelation, or can a single Egyptian religious-political system hold both across time?',
    'Historical evidence: the Amarna period shows the atenist reading was attempted as an explicit REPLACEMENT of the Amun-centered structure (temple closures, name erasures, priesthood disempowerment), and the subsequent Restoration reversed this — suggesting the two readings cannot coexist within one operative state apparatus at the same time, even though both are historically attested in sequence.',
    'If foreclosure is correct, the two readings should be linked with a forecloses relation rather than coexists_with, since Akhenaten''s reform was structurally an attempt to dismantle this constraint, not merely compete with it rhetorically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_amun_vs_atenist_foreclosure, conceptual, 'Whether the Amun and Aten readings are mutually exclusive within one operative religious-political framework or merely sequential/competing claims.').

omega_variable(
    coordination_extraction_separability_amun,
    'Is the grain-storage and calendrical-coordination function of the Amun temple economy separable from its land-and-labor extraction function, or did the extraction scale required to fund monumental temple building make the two inseparable in practice?',
    'Comparative analysis of grain-account papyri showing storage-to-monumental-expenditure ratios across dynasties; a rising ratio of construction/luxury spending relative to famine-reserve spending would indicate growing separability of extraction from coordination.',
    'If separable, the extractiveness measured here overstates the coordination component''s true cost; if inseparable, the observed 0.62 terminal extractiveness may understate how much of even the ''coordination'' function was already extraction-funded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability_amun, empirical, 'Whether temple coordination functions and extraction functions can be economically disentangled in the surviving record.').

omega_variable(
    naturalness_of_polytheistic_cosmology_claim,
    'Was the Amun-Ra-centered cosmology experienced by contemporaries as a discovered cosmic truth (mountain-like) or as a known-constructed political-religious settlement maintained by an identifiable priestly class (constructed, tangled-rope-like)?',
    'This constraint does not claim mountain status, but the omega is recorded because any reading of this kernel risks conflation with naturalized cosmological claims; textual analysis of priestly self-description (e.g., whether inscriptions acknowledge interpretive discretion or claim unmediated cosmic fact) would help resolve which frame contemporaries themselves used.',
    'If contemporaries broadly understood priestly interpretation as discretionary and political, the tangled_rope classification is strongly supported; if genuinely experienced as unmediated cosmic fact by nearly all strata, the accessibility_collapse metric may be understated here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_polytheistic_cosmology_claim, empirical, 'Whether the cosmology was understood by contemporaries as discovered truth or as a maintained political-religious construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__amun_polytheistic_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(divi_tr_t80, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(divi_tr_t160, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 160, 0.31).
narrative_ontology:measurement(divi_tr_t240, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 240, 0.35).
narrative_ontology:measurement(divi_tr_t320, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 320, 0.38).
narrative_ontology:measurement(divi_tr_t400, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 400, 0.4).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(divi_be_t80, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 80, 0.42).
narrative_ontology:measurement(divi_be_t160, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 160, 0.5).
narrative_ontology:measurement(divi_be_t240, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 240, 0.56).
narrative_ontology:measurement(divi_be_t320, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 320, 0.6).
narrative_ontology:measurement(divi_be_t400, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 400, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(divi_su_t80, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 80, 0.34).
narrative_ontology:measurement(divi_su_t160, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 160, 0.38).
narrative_ontology:measurement(divi_su_t240, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 240, 0.42).
narrative_ontology:measurement(divi_su_t320, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 320, 0.46).
narrative_ontology:measurement(divi_su_t400, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 400, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__amun_polytheistic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__amun_polytheistic_reading, 0.08).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, atenist_monotheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the divine_legitimacy_substrate kernel, decomposed per the ε-invariance principle: amun_polytheistic_reading (this story, tangled_rope, distributed priestly authority), atenist_monotheistic_reading (sole pharaonic revelation, expected snare-adjacent given total power concentration), and folk_syncretistic_reading (village-level pragmatic practice, expected rope-adjacent given low institutional overhead). Each carries its own ε and stakeholder structure; they are linked here rather than merged because measuring 'divine legitimacy in ancient Egypt' along different observational axes (institutional priestly record vs. royal inscription vs. household ritual deposit) yields incompatible ε values — the classic signal that one label covers multiple constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
