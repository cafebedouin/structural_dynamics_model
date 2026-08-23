% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__iconoclast_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: decalogue_image_prohibition__iconoclast_reading
 *   human_readable: Iconoclast Reading of the Second Commandment: Total Prohibition of Religious Imagery
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   The iconoclast reading of the Second Commandment prohibits all religious
 *   imagery as idolatry, asserting that any material representation used in
 *   worship violates the commandment. This reading was enforced by Byzantine
 *   imperial authority during the first (726–787) and second (814–842)
 *   iconoclast periods. The constraint operates as a wall-type prohibition:
 *   it does not regulate imagery, it eliminates it. Victims include icon
 *   producers (artisans, monastic scriptoria), monastic communities organized
 *   around icon veneration, lay devotional practices dependent on imagery,
 *   and iconodule clergy. The beneficiary is centralizing imperial authority,
 *   which monopolizes religious form, confiscates monastic assets, and
 *   eliminates regional cult centers that competed with imperial orthodoxy.
 *   The iconodule reading (sibling) permits veneration of images (dulia)
 *   distinct from worship (latria), grounded in Incarnational theology. The
 *   moderate iconoclast reading permits two-dimensional images under
 *   regulation. This reading forecloses both: material mediation is
 *   categorically impermissible.
 *
 * KEY AGENTS:
 *   - imperial_authority: Primary beneficiary (institutional/arbitrage) — monopolizes religious form, confiscates assets, centralizes orthodoxy
 *   - iconoclast_hierarchy: Agenda setter (institutional/identity_locked) — administers enforcement, defines orthodoxy, careers depend on the prohibition
 *   - icon_producers: Primary payer (organized/trapped) — artisans and monastic workshops whose livelihood and vocation are destroyed
 *   - monastic_iconophile_communities: Primary payer (organized/identity_locked) — communities constituted by icon veneration; suppression targets their corporate identity
 *   - lay_devotional_practitioners: Payer (powerless/trapped) — devotional life organized around domestic and public imagery; no exit from the constraint's reach
 *   - iconodule_clergy: Payer (moderate/constrained) — clergy who defend image veneration; face deposition, exile, or martyrdom
 *   - iconodule_reading: Excluded (observer/analytical) — the sibling reading that permits dulia; its adherents are the constraint's victims
 *   - moderate_iconoclast_reading: Excluded (observer/analytical) — the sibling reading permitting regulated 2D images; foreclosed by this reading's categorical prohibition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, 0.78).
domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, 0.85).
domain_priors:theater_ratio(decalogue_image_prohibition__iconoclast_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__iconoclast_reading, "Iconoclast Reading of the Second Commandment: Total Prohibition of Religious Imagery").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconoclast_reading, 'bb0d985d-6f14-4765-984a-35821d036b01').
narrative_ontology:cs_kernel_codification('bb0d985d-6f14-4765-984a-35821d036b01', fixed_text).
narrative_ontology:cs_authority_grounding('bb0d985d-6f14-4765-984a-35821d036b01', extraction).
narrative_ontology:cs_reading_relation('bb0d985d-6f14-4765-984a-35821d036b01', decalogue_image_prohibition__iconodule_reading, forecloses).
narrative_ontology:cs_reading_relation('bb0d985d-6f14-4765-984a-35821d036b01', decalogue_image_prohibition__moderate_iconoclast_reading, forecloses).
narrative_ontology:cs_axiom('bb0d985d-6f14-4765-984a-35821d036b01', foundational, material_mediation_categorically_impermissible).
narrative_ontology:cs_axiom_status(material_mediation_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('bb0d985d-6f14-4765-984a-35821d036b01', material_mediation_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('bb0d985d-6f14-4765-984a-35821d036b01', foundational, image_as_idolatry_identical).
narrative_ontology:cs_axiom_status(image_as_idolatry_identical, holdable).
narrative_ontology:cs_axiom_grounding('bb0d985d-6f14-4765-984a-35821d036b01', image_as_idolatry_identical, deontological).
narrative_ontology:cs_axiom('bb0d985d-6f14-4765-984a-35821d036b01', secondary, imperial_authority_defines_orthodoxy).
narrative_ontology:cs_axiom_status(imperial_authority_defines_orthodoxy, holdable).
narrative_ontology:cs_axiom_grounding('bb0d985d-6f14-4765-984a-35821d036b01', imperial_authority_defines_orthodoxy, conventional).
narrative_ontology:cs_reference_frame('bb0d985d-6f14-4765-984a-35821d036b01', apostolic_spiritual_worship).
narrative_ontology:cs_drift_state('bb0d985d-6f14-4765-984a-35821d036b01', iconoclast_imperial_enforcement, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bb0d985d-6f14-4765-984a-35821d036b01', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, imperial_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, iconoclast_hierarchy).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, icon_producers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, monastic_iconophile_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, lay_devotional_practitioners).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, iconodule_clergy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, iconoclast_hierarchy).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconoclast_reading, spiritual_worship_excludes_material_mediation).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconoclast_reading, imperial_authority_over_religious_form).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconoclast_reading, image_as_idolatry_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues iconoclast edicts, confiscates monastic lands and icon workshops, appoints compliant patriarchs, and uses religious uniformity to consolidate political control over themes and provinces. The prohibition is the legal instrument for asset seizure and centralization. Exit is trivial — they write the rules.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, imperial_authority, beneficiary,
    institutional, generational, arbitrage, continental).

% Patriarchs and bishops who enforce the prohibition: they preside over icon destruction, depose iconodule clergy, and define orthodoxy. Their ecclesiastical careers and institutional legitimacy are fused to the prohibition; recanting means deposition. They administer the constraint but are also trapped by it.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconoclast_hierarchy, agenda_setter,
    institutional, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconoclast_reading, iconoclast_hierarchy, payer).

% Artisans, painters, mosaicists, and monastic workshops whose livelihood is icon production. The prohibition criminalizes their trade, destroys their works, and seizes their workshops. No alternative market exists; their skills are specific to the prohibited practice. Exit means abandonment of vocation and impoverishment.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, icon_producers, payer,
    organized, biographical, trapped, regional).

% Monasteries organized around icon veneration, icon production, and pilgrimage. The prohibition dissolves their corporate identity: icons are destroyed, pilgrimage ends, endowments are confiscated. Monks who resist face exile or martyrdom. The community cannot 'exit' the constraint without ceasing to be what it is.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, monastic_iconophile_communities, payer,
    organized, generational, identity_locked, regional).

% Ordinary Christians whose devotional life centers on domestic icons, church images, and public processions. The prohibition reaches into homes (iconoclast patrols), churches (whitewashing), and public space. No theological alternative is offered; the devotional world is simply removed. Exit is impossible — the constraint governs the only religious world they know.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, lay_devotional_practitioners, payer,
    powerless, biographical, trapped, local).

% Clergy who defend image veneration theologically and pastorally. They face deposition, exile, imprisonment, or martyrdom. Some recant under pressure; others lead resistance networks. Exit options: recant (identity fracture), exile (loss of flock), or resistance (high risk). Their institutional position gives them voice but not protection.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconodule_clergy, payer,
    moderate, biographical, constrained, continental).

% The sibling reading that permits veneration (dulia) of images as honor to their prototypes, grounded in the Incarnation sanctifying matter. Its adherents are the constraint's primary victims. It is excluded from the iconoclast framework — not merely disagreed with but defined as heresy. The reading persists in exile and underground, re-emerging after 843.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconodule_reading, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(decalogue_image_prohibition__iconoclast_reading, iconodule_reading).

% The sibling reading that forbids three-dimensional statuary but permits regulated two-dimensional images. It attempts a compromise position. The iconoclast reading forecloses it: any material mediation is idolatry. This reading is suppressed as 'half-measure' heresy by the iconoclast hierarchy.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, moderate_iconoclast_reading, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(decalogue_image_prohibition__iconoclast_reading, moderate_iconoclast_reading).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates religious practice around purely spiritual worship, eliminating material mediation that the reading identifies as idolatrous. Solves the problem of pagan syncretism and image worship by categorical prohibition.
% TRANSFER_FUNCTION: Moves control of religious form, monastic assets, icon workshop production, and devotional practice from distributed communities (monastic, lay, artistic) to centralized imperial authority. The extraction is total: the material culture of devotion is destroyed or confiscated; the regulatory monopoly is captured.
% ABSENT_VOICES: Iconodule theologians (John of Damascus, Theodore the Studite), monastic communities in exile, lay practitioners driven underground, and the moderate iconoclast position. They are excluded by definition — the constraint's enforcement machinery exists to silence them. Their absence is not accidental; it is the constraint's function.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished overnight, icon production would resume, monastic economies would rebuild, lay devotional practice would re-emerge publicly, iconodule clergy would return from exile, and imperial authority would lose its primary instrument of religious centralization. The visual culture of Byzantium would reorganize around image veneration within years — as it did in 843.
% FOUNDING_PROBLEM: The perceived contamination of Christian worship by pagan idolatry: the use of images in Christian practice was seen as indistinguishable from pagan cult statues, risking syncretism and violating the Second Commandment's prohibition of graven images.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (pagan idolatry threat) is attested as dead by the iconodule reading's own theologians (John of Damascus: 'In former times, God, who is without form or body, could never be depicted. But now that God has appeared in the flesh... I do not venerate matter, I venerate the Creator of matter'). The imperial authority's continued enforcement after the pagan threat receded is attested by the confiscation of monastic assets and the suppression of the moderate iconoclast compromise — actions that serve centralization, not theological purity. No source outside the iconoclast hierarchy attests the founding problem as live after c. 750.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconoclast_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconoclast_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(decalogue_image_prohibition__iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconoclast_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__iconoclast_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(decalogue_image_prohibition__iconoclast_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high: the constraint extracts the entire material infrastructure of devotion (icons, workshops, monastic economies, lay devotional objects) and redirects control to imperial authority. Suppression (0.85) is very high: enforcement requires active destruction of images, persecution of producers and devotees, and policing of private devotion. Theater ratio (0.22) is low: the prohibition is not performative; the destruction is real and thorough. Accessibility collapse (0.92) is near-total: once the prohibition is understood, the alternative (image veneration) is not merely discouraged but structurally eliminated — no legal space, no institutional support, no public practice survives. Resistance (0.68) is substantial: iconophile resistance persisted for over a century, producing sophisticated theology, underground networks, and martyrdom. The resistance is practice-anchored (see omega) not merely theological.
 *
 * PERSPECTIVAL GAP:
 *   From the imperial/agenda-setter seat, the constraint appears as necessary theological purification — a coordination function (pure worship) that requires elimination of a corrupting practice. From the payer seats (icon producers, monastic communities, lay practitioners), the same structure operates as total extraction: their material-devotional world is dismantled, their vocation criminalized, their communities dispersed. The iconoclast hierarchy sits in a dual position: they administer the constraint (agenda_setter) but their institutional existence depends on it (identity_locked payer). The engine computes per-seat types from these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Imperial authority: d ≈ 0.1 (beneficiary — extracts control, assets, monopoly over religious form). Iconoclast hierarchy: d ≈ 0.25 (agenda_setter with identity lock — administers but depends on the constraint for institutional legitimacy). Icon producers: d ≈ 0.95 (trapped — livelihood and vocation destroyed, no exit). Monastic iconophile communities: d ≈ 0.9 (identity_locked — corporate identity constituted by icon veneration; exit means dissolution of the community). Lay practitioners: d ≈ 0.85 (trapped — devotional world dismantled, private practice policed). Iconodule clergy: d ≈ 0.75 (constrained — can recant or resist; some exit via exile). The beneficiary/victim declarations drive this derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pagan idolatry contaminating Christian worship) was live in the 8th century but the prohibition's scope (all religious imagery) vastly exceeds it. The constraint persists by redefining all material mediation as idolatry, expanding the founding problem to cover the sibling readings' practices. Mandatrophy is unresolved: the arrangement has outlived its original function and now serves imperial centralization. The founding_problem_status is 'dead' (the specific pagan threat is gone) but the constraint persists — classic mandatrophy signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is this constraint one reading of a contested kernel (decalogue_image_prohibition) or an independent constraint?',
    'Structural comparison with sibling readings (iconodule_reading, moderate_iconoclast_reading): if they share the same kernel_id but instantiate different beneficiary/victim structures and different ε, they are readings of the same kernel. The ε-invariance principle requires separate constraint stories.',
    'If confirmed as a kernel reading, this constraint''s ε and classification are reading-indexed, not kernel-indexed. The sibling readings generate their own constraints with their own metrics. Misidentifying this as an independent constraint would conflate the kernel''s structural ambiguity with a single constraint''s profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether this is a kernel reading requiring committer-frame discipline.').

omega_variable(
    imperial_beneficiary_intent,
    'Does the imperial authority genuinely benefit from the prohibition, or is the benefit incidental to theological conviction?',
    'Historical analysis of imperial policy: correlate iconoclast edicts with centralization of religious authority, confiscation of monastic assets, and suppression of regional cults. If enforcement patterns track political consolidation more than theological debate, the beneficiary declaration is structural.',
    'If imperial benefit is structural, the constraint is a snare with a clear coordination-extraction split. If benefit is incidental, the constraint may be a tangled rope where theological coordination and political extraction are inseparable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_beneficiary_intent, empirical, 'Whether the imperial authority is a structural beneficiary or a coincidental one.').

omega_variable(
    iconophile_resistance_nature,
    'Is the resistance from iconophile communities primarily theological conviction or defense of material practice?',
    'Examine the arguments of iconodule theologians (John of Damascus, Theodore the Studite) vs. the material dependencies of monastic icon production. If resistance collapses when material practice is suppressed but theological arguments persist, the resistance is practice-anchored.',
    'If resistance is practice-anchored, the constraint''s suppression metric (0.85) reflects coercion of material life, not just belief. If resistance is purely theological, suppression may be overstated relative to the constraint''s actual enforcement capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iconophile_resistance_nature, empirical, 'Whether resistance is anchored in material practice or abstract conviction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconoclast_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iconoclast_reading_tr_t0, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(iconoclast_reading_tr_t50, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement(iconoclast_reading_tr_t100, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 100, 0.18).
narrative_ontology:measurement(iconoclast_reading_tr_t150, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 150, 0.22).
narrative_ontology:measurement(iconoclast_reading_tr_t200, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 200, 0.2).
narrative_ontology:measurement(iconoclast_reading_tr_t250, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 250, 0.22).

% Extraction over time
narrative_ontology:measurement(iconoclast_reading_be_t0, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(iconoclast_reading_be_t50, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(iconoclast_reading_be_t100, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 100, 0.71).
narrative_ontology:measurement(iconoclast_reading_be_t150, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 150, 0.78).
narrative_ontology:measurement(iconoclast_reading_be_t200, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 200, 0.75).
narrative_ontology:measurement(iconoclast_reading_be_t250, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 250, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(iconoclast_reading_su_t0, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(iconoclast_reading_su_t50, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(iconoclast_reading_su_t100, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 100, 0.81).
narrative_ontology:measurement(iconoclast_reading_su_t150, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 150, 0.85).
narrative_ontology:measurement(iconoclast_reading_su_t200, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 200, 0.78).
narrative_ontology:measurement(iconoclast_reading_su_t250, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 250, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconoclast_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(decalogue_image_prohibition__iconoclast_reading, 0.08).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__iconodule_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__moderate_iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, byzantine_imperial_orthodoxy_enforcement).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, monastic_economy_icon_production).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, lay_devotional_practice_network).

% DUAL FORMULATION NOTE:
% This constraint is one member of the decalogue_image_prohibition constraint family. The kernel is the Second Commandment; the three readings instantiate three structurally distinct constraints with different ε values (iconoclast: high extraction, iconodule: low extraction, moderate: intermediate). They are linked via affects_constraints. The iconoclast reading forecloses the others; the iconodule reading coexists with the moderate reading; the moderate reading influences the iconoclast reading by offering a compromise that the iconoclast reading must actively suppress.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decalogue_image_prohibition__iconoclast_reading, institutional, 0.1).
constraint_indexing:directionality_override(decalogue_image_prohibition__iconoclast_reading, organized, 0.9).
constraint_indexing:directionality_override(decalogue_image_prohibition__iconoclast_reading, powerless, 0.85).
constraint_indexing:directionality_override(decalogue_image_prohibition__iconoclast_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
