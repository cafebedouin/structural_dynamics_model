% ============================================================================
% CONSTRAINT STORY: nicene_christological_kernel__homoousios_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_christological_kernel__homoousios_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: nicene_christological_kernel__homoousios_reading
 *   human_readable: Nicene Homoousios Doctrine — Same Substance Reading
 *   domain: historical_theology/christology/ecclesiastical_authority
 *
 * SUMMARY:
 *   The homoousios reading of the Nicene Christological kernel — that Christ
 *   is of the same substance (homoousios) as the Father — was imposed as
 *   imperial orthodoxy through the Council of Nicaea (325) and Constantinople
 *   (381). The formula resolved the Arian controversy by doctrinal fiat
 *   backed by state coercion: anathema, exile, confiscation of church
 *   property, and suppression of Arian worship. What began as a theological
 *   dispute became a mechanism for consolidating ecclesiastical authority
 *   under imperial patronage. The reading claims to preserve the mystery of
 *   the Trinity and the full divinity of Christ; its operation extracts
 *   conformity from dissenting communities (Gothic Arians, North African
 *   homeousians/homoiousians) and channels resources and legitimacy to the
 *   imperially aligned episcopate. The coordination function — a unified
 *   imperial church — is genuine but inseparable from the extraction of
 *   theological and regional autonomy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, 0.78).
domain_priors:suppression_score(nicene_christological_kernel__homoousios_reading, 0.82).
domain_priors:theater_ratio(nicene_christological_kernel__homoousios_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoousios_reading, tangled_rope).
narrative_ontology:human_readable(nicene_christological_kernel__homoousios_reading, "Nicene Homoousios Doctrine — Same Substance Reading").
narrative_ontology:topic_domain(nicene_christological_kernel__homoousios_reading, "historical_theology/christology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoousios_reading, 'de94fba0-5a35-4ac8-8b5b-6c19fdb95a71').
narrative_ontology:cs_kernel_codification('de94fba0-5a35-4ac8-8b5b-6c19fdb95a71', fixed_text).
narrative_ontology:cs_authority_grounding('de94fba0-5a35-4ac8-8b5b-6c19fdb95a71', lineage).
narrative_ontology:cs_interpretation_layer_present('de94fba0-5a35-4ac8-8b5b-6c19fdb95a71').
narrative_ontology:cs_reading_relation('de94fba0-5a35-4ac8-8b5b-6c19fdb95a71', nicene_christological_kernel__homoiousios_reading, forecloses).
narrative_ontology:cs_axiom('de94fba0-5a35-4ac8-8b5b-6c19fdb95a71', foundational, consubstantial_identity_necessary_for_salvation).
narrative_ontology:cs_axiom_status(consubstantial_identity_necessary_for_salvation, holdable).
narrative_ontology:cs_axiom_grounding('de94fba0-5a35-4ac8-8b5b-6c19fdb95a71', consubstantial_identity_necessary_for_salvation, deontological).
narrative_ontology:cs_axiom('de94fba0-5a35-4ac8-8b5b-6c19fdb95a71', secondary, imperial_council_defines_orthodoxy).
narrative_ontology:cs_axiom_status(imperial_council_defines_orthodoxy, holdable).
narrative_ontology:cs_axiom_grounding('de94fba0-5a35-4ac8-8b5b-6c19fdb95a71', imperial_council_defines_orthodoxy, conventional).
narrative_ontology:cs_reference_frame('de94fba0-5a35-4ac8-8b5b-6c19fdb95a71', nicene_constantinopolitan_creed_381).
narrative_ontology:cs_drift_state('de94fba0-5a35-4ac8-8b5b-6c19fdb95a71', post_chalcedon_451, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('de94fba0-5a35-4ac8-8b5b-6c19fdb95a71', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, imperial_church_leadership).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, nicene_orthodox_bishops).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, roman_state_ideology).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, arian_gothic_communities).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, north_african_dissenting_churches).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, theological_diversity).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, regional_ecclesiastical_autonomy).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoousios_reading, trinitarian_orthodoxy).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoousios_reading, christ_full_divinity).
narrative_ontology:constraint_vindicates(nicene_christological_kernel__homoousios_reading, imperial_church_unity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the convocation of councils, the formulation of creeds, and the deployment of state coercion (exile, property seizure) against dissent. Collects the legitimacy dividend of being the 'orthodox' church. Can redefine the constraint (e.g., Chalcedon 451) without losing authority — exit is arbitrage because the institution itself sets the terms of orthodoxy.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, imperial_church_leadership, agenda_setter,
    institutional, generational, arbitrage, continental).

% Receive imperial patronage, legal privileges, and sacramental monopoly in their sees. Their authority derives from communion with the homoousios-defining center. Exit is constrained: breaking communion means losing the imperial protection that secures their position, but some (e.g., Meletius of Antioch) navigated between homoousios and homoiousios factions.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, nicene_orthodox_bishops, beneficiary,
    organized, biographical, constrained, regional).

% The doctrine functions as the theological cement of the Christian Roman Empire. It is not an agent but a structural beneficiary: the state's legitimacy depends on the church's unity, and the church's unity depends on the homoousios formula. The formula vindicates the proposition 'imperial_church_unity' — the state collects the political order, the church collects the souls.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, roman_state_ideology, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(nicene_christological_kernel__homoousios_reading, roman_state_ideology).

% Gothic peoples converted to Arian Christianity before entering the empire. Their ecclesiastical structure (bishops, liturgy, Gothic-language scriptures) is fused with ethnic identity. The homoousios constraint demands their reordination, surrender of churches, and acceptance of Latin/Greek theological vocabulary. Exit means abandoning the form of life that defines them as a people — not merely changing belief but ceasing to be Gothic in the way their ancestors were.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, arian_gothic_communities, payer,
    organized, generational, identity_locked, regional).

% Homeousian and homoiousian communities in North Africa (e.g., around Carthage) resisted both Arian Vandal imposition and Nicene imperial reconquest. They lack the ethnic fusion of the Goths — their exit_options are trapped: geographic confinement, no alternative patriarchate, and the Donatist schism already exhausted their organizational capacity. They pay through property loss, clerical exile, and eventual absorption or disappearance.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, north_african_dissenting_churches, payer,
    moderate, biographical, trapped, regional).

% The range of live Christological positions (homoousios, homoiousios, homeousios, heteroousios, Apollinarian, Nestorian, Monophysite) is collapsed to one. This is not an agent but a structural victim: the constraint's operation destroys the ecological niche in which theological alternatives could evolve. The extraction is the foreclosure of the possibility space itself.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, theological_diversity, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(nicene_christological_kernel__homoousios_reading, theological_diversity).

% Pre-Nicene, regional churches (Alexandria, Antioch, Rome, Carthage, Milan) had substantial doctrinal autonomy. The homoousios constraint, enforced by imperial councils, centralizes doctrinal authority in the imperially favored sees. Regional autonomy pays the cost: local theological traditions are suppressed, and episcopal appointments become subject to imperial approval.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, regional_ecclesiastical_autonomy, payer,
    powerless, civilizational, trapped, continental).
narrative_ontology:stakeholder_non_agent(nicene_christological_kernel__homoousios_reading, regional_ecclesiastical_autonomy).

% Bishops and theologians (e.g., Basil of Ancyra, George of Laodicea) who held that the Son is 'of similar substance' (homoiousios) to the Father — preserving distinction while affirming likeness. They were anathematized at Constantinople (381) and excluded from the imperial church. Their position was not merely defeated but rendered unthinkable within the Nicene framework. They would object that homoousios confuses the persons; they are absent because the constraint's enforcement machinery (anathemas, imperial exile) removed them from the conversation.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, homoiousios_adherents, excluded,
    moderate, biographical, constrained, regional).

% Contemporary theologians (Catholic, Orthodox, Protestant) who inherit the homoousios formula as dogma but analyze its historical operation. They see the full structure: the genuine coordination of Trinitarian orthodoxy, the extraction from Arian communities, the theatrical maintenance of the formula long after its founding problem shifted. They neither collect nor pay — they diagnose.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, modern_chalcedonian_theologians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the Christological chaos threatening the unity of the Christian Roman Empire by establishing a single, imperially enforced definition of the Son's relationship to the Father — enabling a unified church, a unified creed, and a unified imperial religion.
% TRANSFER_FUNCTION: Moves ecclesiastical property, imperial patronage, sacramental validity, and civic participation from non-homoousios communities (Arian Goths, North African dissenters) to the Nicene episcopate and the imperial center. The currency is conformity; the price is anatomical exile and asset seizure.
% ABSENT_VOICES: The homoiousios bishops (Basil of Ancyra, George of Laodicea, the entire Homoean party under Constantius) would object that homoousios collapses the distinction between Father and Son into Sabellianism. They are absent because the constraint's enforcement (Constantinople 381, Theodosius's edicts) removed them from the episcopal order. Gothic Arian laity — who left no written record — would object that their form of Christian life was criminalized; they are absent because they were a preliterate minority under military pressure.
% DISAPPEARANCE_RATIONALE: If the homoousios constraint vanished overnight (e.g., Theodosius's edicts repealed, Constantinople's anathemas lifted), the Gothic Arian churches would reclaim their property and hierarchy; North African homeousians would re-emerge; the imperial church would fracture into regional Christologies; the Chalcedonian settlement (which depends on homoousios as its Trinitarian precondition) would destabilize. The world rearranges because the constraint's extraction infrastructure (property, exile, sacramental monopoly) is the only thing holding the Nicene monopoly in place.
% FOUNDING_PROBLEM: The Arian controversy (c. 318–325) threatened to split the Christian church and thereby the Roman Empire: Arius taught the Son was a created being, not co-eternal with the Father; Alexander of Alexandria and Athanasius countered that this destroyed salvation. Constantine convened Nicaea to impose unity. The founding problem was imperial survival requiring ecclesiastical unity requiring Christological definition.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (Arianism as an existential threat to imperial unity) is dead: Arianism as a live theological option was eradicated in the empire by 600, and the Gothic Arian kingdoms converted to Nicene Christianity (Visigoths 587, Lombards 7th century). Corroboration from outside the beneficiary set: modern secular historians (e.g., Peter Brown, Ramsay MacMullen) attest the Arian threat was political-theological and ended with the conversion of the Gothic kingdoms; the homoousios formula persists as dogma without its founding political justification.
narrative_ontology:disappearance_verdict(nicene_christological_kernel__homoousios_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_christological_kernel__homoousios_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoousios_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(nicene_christological_kernel__homoousios_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_christological_kernel__homoousios_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_christological_kernel__homoousios_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_christological_kernel__homoousios_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint transfers material resources (church property, imperial patronage), social capital (episcopal authority, sacramental validity), and political legitimacy from non-conforming communities to the Nicene establishment. Suppression is very high (0.82) — alternatives were not merely discouraged but criminalized; Arian bishops were exiled, Gothic Arian churches were confiscated, and the homoiousios middle position was anathematized. Theater ratio (0.35) reflects that the theological apparatus (creeds, councils, anathemas) performs real doctrinal work but increasingly serves to legitimate the institutional order. Accessibility collapse (0.65) is substantial: once the imperial-church fusion is accepted, the homoousios formula becomes the only thinkable orthodoxy. Resistance (0.55) persisted for centuries — Gothic Arianism survived into the 7th century; North African resistance fed into Donatist and later Islamic-era identities — but was structurally contained.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (imperial episcopate), the constraint is a Rope: it coordinates a fragmented church into imperial unity, solves the Christological chaos threatening social order, and the suppression is the price of coherence. From the payer seats (Arian communities), it is a Snare: the coordination story is cover for the annihilation of their form of life. The engine computes this divergence from the structural data — the claimed_type (tangled_rope) acknowledges both coordination and extraction are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Imperial church leadership (agenda_setter) and Nicene bishops (beneficiary) sit at the beneficiary end: they collect patronage, authority, and the power to define orthodoxy. Arian Gothic communities and North African dissenters (payers) bear the full cost: loss of property, exile, denial of civic participation, and eventual eradication. Theological diversity and regional autonomy are structural victims — not agents but the coordination goods destroyed. The homoiousios reading (sibling) is not a victim but a foreclosed alternative; its adherents were reclassified as heretics. Directionality derives from beneficiary/victim declarations plus exit options: the agenda_setter has arbitrage-grade exit (can redefine the constraint); payers are identity_locked (ecclesial identity fused with theological position) or trapped (Goths under imperial pressure).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Christological chaos threatening imperial unity — was live in 325. By 381 it was contested (Arianism still dominant among Goths). By 600 the founding problem was dead in the West (Nicene orthodoxy universal) but the constraint persisted and intensified, now extracting conformity from Monophysite and Nestorian communities. The mandatrophy is resolved: the arrangement outlived its founding justification and became a permanent mechanism of ecclesiastical control. The theater_ratio rise from 0.15 to 0.35 tracks this — the coordination function atrophied while the extraction infrastructure hardened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_inseparability,
    'Is the coordination function (imperial church unity) structurally inseparable from the extraction mechanism (anathema, exile, property confiscation), or could unity have been achieved without suppressing the homoiousios alternative?',
    'Counterfactual analysis: examine whether the Meletian schism in Antioch or the Homoean compromise under Constantius produced stable coordination without homoousios enforcement. If yes, the extraction is separable; if no, the constraint is genuinely tangled.',
    'If separable, the high extractiveness is not the price of coordination but a choice — reclassification toward snare for the payer seats. If inseparable, tangled_rope stands as the honest structural description.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_inseparability, conceptual, 'Whether the coordination and extraction components are structurally fused or contingently joined.').

omega_variable(
    identity_lock_mechanism_arian,
    'For Arian Gothic communities, was identity_locked exit driven by theological conviction (the homoousios formula is salvation-denying) or by communal survival (Arianism as ethnic marker under Roman pressure)?',
    'Compare Gothic Arian persistence after imperial persecution (6th century) with North African homoiousian collapse. If Gothic identity fused with Arianism survives political defeat, the lock is identity-constitutive; if it dissolves, the lock was situational.',
    'If identity-constitutive, the payer seat''s directionality is amplified (d → 1.0) — they cannot exit without ceasing to be who they are. If situational, the extraction is coercive but not identity-trapping — d lower, effective extraction reduced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_arian, empirical, 'Whether Arian communities'' exit_options are identity_locked or trapped.').

omega_variable(
    kernel_framing_ambiguity,
    'Does the kernel ''Christ is [homoousios/homoiousios] with the Father'' name a single theological claim with two readings, or two distinct kernels (one about substance metaphysics, one about salvific identity)?',
    'Trace the pre-Nicene usage: if homoousios and homoiousios were competing answers to the SAME question (ontological relationship), one kernel. If they answered DIFFERENT questions (ousia vs. hypostasis, or metaphysics vs. soteriology), two kernels falsely unified by the creedal formula.',
    'If two kernels, this story and the homoiousios_reading story are not siblings but distinct constraints — network.affects_constraints should be removed. If one kernel, the forecloses relation stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the contested kernel is one commitment with two readings or two commitments conflated by history.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoousios_reading, 325, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nicene_homoousios_tr_t325, nicene_christological_kernel__homoousios_reading, theater_ratio, 325, 0.15).
narrative_ontology:measurement(nicene_homoousios_tr_t350, nicene_christological_kernel__homoousios_reading, theater_ratio, 350, 0.22).
narrative_ontology:measurement(nicene_homoousios_tr_t381, nicene_christological_kernel__homoousios_reading, theater_ratio, 381, 0.28).
narrative_ontology:measurement(nicene_homoousios_tr_t451, nicene_christological_kernel__homoousios_reading, theater_ratio, 451, 0.32).
narrative_ontology:measurement(nicene_homoousios_tr_t553, nicene_christological_kernel__homoousios_reading, theater_ratio, 553, 0.34).
narrative_ontology:measurement(nicene_homoousios_tr_t600, nicene_christological_kernel__homoousios_reading, theater_ratio, 600, 0.35).

% Extraction over time
narrative_ontology:measurement(nicene_homoousios_be_t325, nicene_christological_kernel__homoousios_reading, base_extractiveness, 325, 0.55).
narrative_ontology:measurement(nicene_homoousios_be_t350, nicene_christological_kernel__homoousios_reading, base_extractiveness, 350, 0.62).
narrative_ontology:measurement(nicene_homoousios_be_t381, nicene_christological_kernel__homoousios_reading, base_extractiveness, 381, 0.7).
narrative_ontology:measurement(nicene_homoousios_be_t451, nicene_christological_kernel__homoousios_reading, base_extractiveness, 451, 0.74).
narrative_ontology:measurement(nicene_homoousios_be_t553, nicene_christological_kernel__homoousios_reading, base_extractiveness, 553, 0.77).
narrative_ontology:measurement(nicene_homoousios_be_t600, nicene_christological_kernel__homoousios_reading, base_extractiveness, 600, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(nicene_homoousios_su_t325, nicene_christological_kernel__homoousios_reading, suppression_requirement, 325, 0.6).
narrative_ontology:measurement(nicene_homoousios_su_t350, nicene_christological_kernel__homoousios_reading, suppression_requirement, 350, 0.68).
narrative_ontology:measurement(nicene_homoousios_su_t381, nicene_christological_kernel__homoousios_reading, suppression_requirement, 381, 0.75).
narrative_ontology:measurement(nicene_homoousios_su_t451, nicene_christological_kernel__homoousios_reading, suppression_requirement, 451, 0.78).
narrative_ontology:measurement(nicene_homoousios_su_t553, nicene_christological_kernel__homoousios_reading, suppression_requirement, 553, 0.8).
narrative_ontology:measurement(nicene_homoousios_su_t600, nicene_christological_kernel__homoousios_reading, suppression_requirement, 600, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_christological_kernel__homoousios_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_christological_kernel__homoousios_reading, 0.12).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, nicene_christological_kernel__homoiousios_reading).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, chalcedonian_definition_kernel__dyophysite_reading).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, imperial_church_property_regime).

% DUAL FORMULATION NOTE:
% This story and homoiousios_reading form the Nicene Christological kernel family. The homoousios reading forecloses the homoiousios reading within any single ecclesiastical framework (no bishopric can hold both). The homoousios reading also influences the Chalcedonian kernel: the homoousios axiom creates the structural pressure that makes dyophysitism the only viable Christological extension. The imperial_church_property_regime is downstream — the property confiscation machinery authorized here becomes the template for later ecclesiastical asset control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nicene_christological_kernel__homoousios_reading, institutional, 0.1).
constraint_indexing:directionality_override(nicene_christological_kernel__homoousios_reading, organized, 0.85).
constraint_indexing:directionality_override(nicene_christological_kernel__homoousios_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
