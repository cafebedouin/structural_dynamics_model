% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: decalogue_image_prohibition__iconoclast_reading
 *   human_readable: Iconoclast Reading: Total Prohibition of Religious Imagery
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   The iconoclast reading of the Second Commandment prohibits all religious
 *   imagery categorically: any material representation used in worship
 *   constitutes idolatry. This reading was imperially enforced in the
 *   Byzantine Empire during two periods (726–787, 815–843), resulting in
 *   widespread destruction of icons, persecution of iconodule monks, and
 *   centralization of ecclesiastical authority under the emperor. The reading
 *   claims divine law (mountain) but operates through imperial enforcement
 *   with identifiable beneficiaries (imperial authority monopolizing
 *   religious form) and victims (icon producers, monastic communities, lay
 *   devotional practices). The measurement series tracks the controversy's
 *   arc: initial policy (726), first peak (754, Council of Hieria), interim
 *   restoration (787), second peak (815–843), and final restoration of icons
 *   (843).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, 0.82).
domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, 0.88).
domain_priors:theater_ratio(decalogue_image_prohibition__iconoclast_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconoclast_reading, mountain).
narrative_ontology:human_readable(decalogue_image_prohibition__iconoclast_reading, "Iconoclast Reading: Total Prohibition of Religious Imagery").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconoclast_reading).
domain_priors:emerges_naturally(decalogue_image_prohibition__iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconoclast_reading, '610fb03b-8f8b-45d3-9603-ce3934feaea0').
narrative_ontology:cs_kernel_codification('610fb03b-8f8b-45d3-9603-ce3934feaea0', fixed_text).
narrative_ontology:cs_authority_grounding('610fb03b-8f8b-45d3-9603-ce3934feaea0', extraction).
narrative_ontology:cs_interpretation_layer_present('610fb03b-8f8b-45d3-9603-ce3934feaea0').
narrative_ontology:cs_reading_relation('610fb03b-8f8b-45d3-9603-ce3934feaea0', decalogue_image_prohibition__iconodule_reading, forecloses).
narrative_ontology:cs_reading_relation('610fb03b-8f8b-45d3-9603-ce3934feaea0', decalogue_image_prohibition__moderate_iconoclast_reading, forecloses).
narrative_ontology:cs_axiom('610fb03b-8f8b-45d3-9603-ce3934feaea0', foundational, material_mediation_categorically_impermissible).
narrative_ontology:cs_axiom_status(material_mediation_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('610fb03b-8f8b-45d3-9603-ce3934feaea0', material_mediation_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('610fb03b-8f8b-45d3-9603-ce3934feaea0', secondary, imperial_authority_guardian_of_pure_worship).
narrative_ontology:cs_axiom_status(imperial_authority_guardian_of_pure_worship, overridden).
narrative_ontology:cs_axiom_grounding('610fb03b-8f8b-45d3-9603-ce3934feaea0', imperial_authority_guardian_of_pure_worship, conventional).
narrative_ontology:cs_reference_frame('610fb03b-8f8b-45d3-9603-ce3934feaea0', apostolic_aniconic_worship).
narrative_ontology:cs_drift_state('610fb03b-8f8b-45d3-9603-ce3934feaea0', contemporary_post_restoration, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('610fb03b-8f8b-45d3-9603-ce3934feaea0', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, imperial_authority).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, icon_producers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, monastic_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, devotional_practitioners).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconoclast_reading, second_commandment_literalism).
narrative_ontology:constraint_vindicates(decalogue_image_prohibition__iconoclast_reading, aniconic_worship_purity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The emperor and his appointed patriarch set and enforce the iconoclast policy. They control ecclesiastical appointments, convene councils, and direct the destruction of images. The prohibition centralizes religious authority in the imperial office, eliminating the independent spiritual authority of monastic communities and the symbolic power of icons. The emperor can shift policy with succession (Leo III, Constantine V, Leo IV, Leo V, Michael II, Theophilos) — exit is arbitrage-grade.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, imperial_authority, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconoclast_reading, imperial_authority, beneficiary).

% Artisans, painters, and craftspeople who produce icons for churches and private devotion. Their livelihood is destroyed by the prohibition; their work is burned or scraped. Some flee to iconodule regions (Rome, monastic peripheries); others convert to secular production. Exit is constrained — skills are specialized, markets are local, and flight means losing patronage networks.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, icon_producers, payer,
    moderate, biographical, constrained, regional).

% Monasteries are centers of icon production and icon veneration. Monks resist iconoclasm as a betrayal of their vocation; many are exiled, imprisoned, or martyred (e.g., St. Stephen the Younger). Their identity is fused with icon veneration — the monastic rule, liturgy, and spiritual practice are constituted through images. Exit is identity_locked: to abandon icons is to abandon their self-understanding as monks.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, monastic_communities, payer,
    organized, biographical, identity_locked, regional).

% Lay Christians who use icons in home prayer, church worship, and civic processions. Their devotional practice is disrupted: home icons are confiscated, church icons destroyed, processions banned. Some conform outwardly; others practice covertly. Exit is constrained — the prohibition is empire-wide, and their devotional habitus is formed around images. Resistance manifests as popular unrest, icon processions, and refusal to commune with iconoclast clergy.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, devotional_practitioners, payer,
    moderate, biographical, constrained, local).

% Theologians (John of Damascus, Theodore the Studite, Patriarch Nicephorus) who articulate the theological defense of icons: the Incarnation sanctifies matter; icons honor the prototype, not the wood/paint. They are excluded from imperial councils, exiled, anathematized. Their exclusion is structural — the iconoclast councils define orthodoxy to exclude them. They are trapped: they cannot leave the theological field without abandoning their vocation, but they cannot participate under iconoclast rule.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconodule_theologians, excluded,
    organized, generational, trapped, continental).

% Historical and theological analysis from outside the controversy. Sees the full structure: a mountain claim (divine law) that operates as imperial extraction. Notes the ε-invariance violation: the same commandment text produces three readings with radically different extractiveness. The analytical seat computes the per-seat classifications the engine will derive.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, modern_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates religious uniformity by defining the boundary of permissible worship: prevents idolatry by eliminating all material mediation, creating a single aniconic standard across the empire.
% TRANSFER_FUNCTION: Moves religious authority from distributed monastic and local devotional practices to the centralized imperial throne; moves artistic labor from icon production to state-approved secular or destructive work; moves spiritual autonomy from lay practitioners to imperial clergy.
% ABSENT_VOICES: Iconodule theologians (exiled/silenced), lay women (primary domestic icon veneration, unrecorded), peripheral monastic communities (Sinai, Palestine, Rome — outside imperial reach but theologically central), icon artisans (no guild representation in councils). Their absence is structural: the iconoclast councils defined participation to exclude them.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished overnight (as in 787 and 843), icon veneration returned rapidly: icons were restored to churches, monastic production resumed, lay processions restarted, and the imperial monopoly on religious form fractured. The world rearranged because arrangements (liturgical, artistic, devotional, political) depended on the constraint.
% FOUNDING_PROBLEM: Early Christian communities in a pagan visual culture needed a clear boundary against idolatry: the prohibition prevented confusion between the Creator and created images, protecting monotheistic worship from assimilation to pagan representational practices.
% FOUNDING_PROBLEM_CORROBORATION: Iconoclasts (imperial authority, iconoclast patriarchs) attest the problem is live — any image risks idolatry. Iconodules (John of Damascus, Theodore the Studite, Seventh Ecumenical Council 787) attest the problem was resolved by the Incarnation: God became visible in Christ, sanctifying matter as a conduit. Modern historical theology (outside both parties) corroborates that the founding problem shifted: early aniconism was cultural-contextual; the Incarnation provided a new theological horizon that the iconoclast reading refused to integrate.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconoclast_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconoclast_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__iconoclast_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, ExtMetricName, E),
    domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(decalogue_image_prohibition__iconoclast_reading),
    narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(decalogue_image_prohibition__iconoclast_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the prohibition extracts devotional autonomy, artistic livelihoods, and monastic spiritual practices, transferring religious authority to the imperial center. Suppression is very high (0.88) — the constraint persisted only through active imperial violence: image destruction, exile, blinding, and execution of resisters. Theater ratio is moderate (0.42) — genuine theological conviction existed among iconoclast theologians, but enforcement intensity correlated with imperial political needs. Accessibility collapse is high (0.85) — for populations under imperial control, icon veneration was practically eliminated. Resistance is high (0.72) — sustained popular and monastic resistance forced two restorations of icons.
 *
 * PERSPECTIVAL GAP:
 *   From the imperial/iconoclast seat, the constraint is genuine coordination (preventing idolatry, preserving apostolic purity) — a mountain. From the monastic/icon producer seat, it is enforced extraction destroying their spiritual and material lives — a snare. From the lay practitioner seat, it is a tangled rope: some genuine theological guidance mixed with coercive removal of their devotional tools. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Imperial authority is the structural beneficiary (d ≈ 0.1): it sets the agenda, collects the monopoly on religious form, and controls ecclesiastical appointments. Icon producers, monastic communities, and devotional practitioners are targets (d ≈ 0.9): they bear the costs (destruction of work, exile, loss of devotional practice) with constrained exit (identity_locked for monastics, constrained for lay practitioners). Iconodule theologians are excluded — their voice was silenced by imperial decree. The analytical observer sees the full structure: a mountain claim masking extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing idolatry in a culture saturated with pagan imagery) was live in the 8th century but became contested as Christian visual culture developed sophisticated theology of incarnation and representation. The arrangement persisted 117 years past its founding justification because it served imperial centralization — a classic mandatrophy where the mandate (prevent idolatry) atrophied but the constraint (imperial control of religious form) persisted. The founding_problem_status is contested: iconoclasts claim the problem is live; iconodules and later theology claim it was resolved by the Incarnation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_imperial_construction,
    'Is the total prohibition of religious imagery a genuine divine command (natural law) or an imperial construction that centralizes religious authority?',
    'Comparative analysis of pre-iconoclastic theology, imperial legislative records, and the correlation between iconoclast policy and centralization of ecclesiastical appointments under the emperor.',
    'If imperial construction, the constraint is a false summit mountain (FSM) — claimed as natural law but operating as extraction for centralizing authority. If genuine divine command, the beneficiary declaration reflects providential ordering, not extractive design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_imperial_construction, conceptual, 'Whether the constraint''s mountain claim masks imperial extraction.').

omega_variable(
    theological_conviction_vs_political_instrument,
    'Was the iconoclast enforcement driven by sincere theological conviction or by the political utility of controlling religious symbolism?',
    'Examine the correlation between iconoclast policy shifts and imperial succession crises; analyze private correspondence of Leo III and Constantine V for theological vs. political language.',
    'If political instrument, the constraint''s theater_ratio is higher (performative theology masking control); if sincere conviction, lower theater_ratio but still extractive in effect on victims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_conviction_vs_political_instrument, empirical, 'Motivation ambiguity behind enforcement.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of icon veneration primarily structural (imperial decree, destruction of images, exile of monks) or did it become internalized (theological terror of idolatry persisting after enforcement ended)?',
    'Post-843 restoration trajectory: measure persistence of aniconic conviction in formerly iconoclast regions after imperial enforcement ceased. If suppression persists without enforcement, reclassify as partially internalized.',
    'If internalized, effective suppression exceeds structural measure — the constraint continues extracting devotional autonomy after formal enforcement ends.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the iconoclast period.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconoclast_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(decalogue_image_prohibition__iconoclast_reading_tr_t0, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(decalogue_image_prohibition__iconoclast_reading_tr_t0, observed).
narrative_ontology:measurement(decalogue_image_prohibition__iconoclast_reading_tr_t20, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(decalogue_image_prohibition__iconoclast_reading_tr_t20, observed).
narrative_ontology:measurement(decalogue_image_prohibition__iconoclast_reading_tr_t40, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(decalogue_image_prohibition__iconoclast_reading_tr_t40, observed).
narrative_ontology:measurement(decalogue_image_prohibition__iconoclast_reading_tr_t60, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 60, 0.52).
narrative_ontology:measurement_basis(decalogue_image_prohibition__iconoclast_reading_tr_t60, observed).
narrative_ontology:measurement(decalogue_image_prohibition__iconoclast_reading_tr_t80, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 80, 0.45).
narrative_ontology:measurement_basis(decalogue_image_prohibition__iconoclast_reading_tr_t80, observed).
narrative_ontology:measurement(decalogue_image_prohibition__iconoclast_reading_tr_t100, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement_basis(decalogue_image_prohibition__iconoclast_reading_tr_t100, observed).
narrative_ontology:measurement(decalogue_image_prohibition__iconoclast_reading_tr_t120, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 120, 0.15).
narrative_ontology:measurement_basis(decalogue_image_prohibition__iconoclast_reading_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(decalogue_image_prohibition__iconoclast_reading_be_t0, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(decalogue_image_prohibition__iconoclast_reading_be_t0, observed).
narrative_ontology:measurement(decalogue_image_prohibition__iconoclast_reading_be_t20, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(decalogue_image_prohibition__iconoclast_reading_be_t20, observed).
narrative_ontology:measurement(decalogue_image_prohibition__iconoclast_reading_be_t40, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement_basis(decalogue_image_prohibition__iconoclast_reading_be_t40, observed).
narrative_ontology:measurement(decalogue_image_prohibition__iconoclast_reading_be_t60, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 60, 0.88).
narrative_ontology:measurement_basis(decalogue_image_prohibition__iconoclast_reading_be_t60, observed).
narrative_ontology:measurement(decalogue_image_prohibition__iconoclast_reading_be_t80, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 80, 0.82).
narrative_ontology:measurement_basis(decalogue_image_prohibition__iconoclast_reading_be_t80, observed).
narrative_ontology:measurement(decalogue_image_prohibition__iconoclast_reading_be_t100, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 100, 0.55).
narrative_ontology:measurement_basis(decalogue_image_prohibition__iconoclast_reading_be_t100, observed).
narrative_ontology:measurement(decalogue_image_prohibition__iconoclast_reading_be_t120, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 120, 0.22).
narrative_ontology:measurement_basis(decalogue_image_prohibition__iconoclast_reading_be_t120, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(decalogue_image_prohibition__iconoclast_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconoclast_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(decalogue_image_prohibition__iconoclast_reading, 0.08).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__iconodule_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'Second Commandment' label into three structurally distinct readings with divergent ε values: iconoclast (high extraction, imperial enforcement), iconodule (low extraction, theological permission), moderate (medium extraction, regulated permission). The iconoclast reading's high ε derives from its enforcement mechanism, not its textual claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decalogue_image_prohibition__iconoclast_reading, institutional, 0.1).
constraint_indexing:directionality_override(decalogue_image_prohibition__iconoclast_reading, organized, 0.85).
constraint_indexing:directionality_override(decalogue_image_prohibition__iconoclast_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
