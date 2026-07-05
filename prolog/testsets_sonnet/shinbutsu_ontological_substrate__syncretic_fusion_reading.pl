% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__syncretic_fusion_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__syncretic_fusion_reading
 *   human_readable: Honji Suijaku as Ontological Unity of Kami and Buddhas
 *   domain: religious/institutional
 *
 * SUMMARY:
 *   This story instantiates the syncretic_fusion_reading of the
 *   shinbutsu_ontological_substrate kernel: the claim that kami and buddhas
 *   are ontologically identical (kami as suijaku, 'manifest traces'; buddhas
 *   as honji, 'original ground'), such that honji suijaku doctrine describes
 *   a metaphysical fact rather than a convenient institutional bundling.
 *   Under this reading, the combined jingu-ji shrine-temple complexes, and
 *   the doctrinal authority of Tendai and Shingon scholar-monks who
 *   articulate the honji-suijaku cosmology, are not incidental to the
 *   metaphysics — they are what a true ontological unity would produce: total
 *   institutional entanglement, resistance to separation, and treatment of
 *   kami subordination to buddhas as discovered truth rather than negotiated
 *   arrangement. This reading is structurally distinct from the sibling
 *   domain_partition_reading (which holds kami and buddhas govern separate
 *   functional domains with no ontological claim) and from the
 *   incoherent_bundle_reading (which denies any coherent kernel exists at
 *   all, attributing the whole arrangement to accumulated state-enforced
 *   drift). Only this reading is authored here; the siblings are separate
 *   constraints linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - shrine_temple_administrative_complexes: institutional beneficiary/agenda_setter administering fused ritual and land economy
 *   - tendai_shingon_doctrinal_authorities: doctrinal beneficiary/agenda_setter producing the metaphysical unity claim
 *   - ruling_bakufu_and_court_patrons: political beneficiary using unified cosmology for dual legitimation
 *   - independent_kami_cult_priests: payer/victim losing autonomy and revenue to absorption
 *   - lay_practitioners_seeking_doctrinal_clarity: powerless payer with no venue to contest the metaphysical claim
 *   - later_kokugaku_purist_scholars: excluded voice arriving only after institutional facts were set
 *   - comparative_religion_historians: analytical observer assessing genealogy and Meiji-era rupture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.58).
domain_priors:suppression_score(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.62).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__syncretic_fusion_reading, "Honji Suijaku as Ontological Unity of Kami and Buddhas").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__syncretic_fusion_reading, "religious/institutional").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__syncretic_fusion_reading, '1c6ac8e9-55c7-4e54-bc61-211580141e63').
narrative_ontology:cs_kernel_codification('1c6ac8e9-55c7-4e54-bc61-211580141e63', distributed).
narrative_ontology:cs_authority_grounding('1c6ac8e9-55c7-4e54-bc61-211580141e63', lineage).
narrative_ontology:cs_interpretation_layer_present('1c6ac8e9-55c7-4e54-bc61-211580141e63').
narrative_ontology:cs_reading_relation('1c6ac8e9-55c7-4e54-bc61-211580141e63', shinbutsu_ontological_substrate__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c6ac8e9-55c7-4e54-bc61-211580141e63', shinbutsu_ontological_substrate__incoherent_bundle_reading, influences).
narrative_ontology:cs_axiom('1c6ac8e9-55c7-4e54-bc61-211580141e63', foundational, kami_and_buddhas_are_ontologically_identical).
narrative_ontology:cs_axiom_status(kami_and_buddhas_are_ontologically_identical, holdable).
narrative_ontology:cs_axiom_grounding('1c6ac8e9-55c7-4e54-bc61-211580141e63', kami_and_buddhas_are_ontologically_identical, theological).
narrative_ontology:cs_axiom('1c6ac8e9-55c7-4e54-bc61-211580141e63', foundational, honji_suijaku_describes_discovered_metaphysical_fact).
narrative_ontology:cs_axiom_status(honji_suijaku_describes_discovered_metaphysical_fact, overridden).
narrative_ontology:cs_axiom_grounding('1c6ac8e9-55c7-4e54-bc61-211580141e63', honji_suijaku_describes_discovered_metaphysical_fact, theological).
narrative_ontology:cs_reference_frame('1c6ac8e9-55c7-4e54-bc61-211580141e63', heian_tendai_shingon_doctrinal_synthesis).
narrative_ontology:cs_drift_state('1c6ac8e9-55c7-4e54-bc61-211580141e63', meiji_shinbutsu_bunri_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('1c6ac8e9-55c7-4e54-bc61-211580141e63', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, shrine_temple_administrative_complexes).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, tendai_shingon_doctrinal_authorities).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, ruling_bakufu_and_court_patrons).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, independent_kami_cult_priests).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, lay_practitioners_seeking_doctrinal_clarity).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, later_kokugaku_purist_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, lay_practitioners_seeking_doctrinal_clarity).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, buddhas_are_true_ground_kami_are_manifest_traces).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, unified_cosmological_order_of_honji_suijaku).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Combined shrine-temple complexes (jingu-ji) administer both kami rites and buddhist liturgy under one institutional roof, controlling land, ritual calendars, and pilgrimage revenue. The ontological unity claim justifies merging administrative functions and revenue streams that a domain-partition reading would keep separate; separation would fracture their landholdings and dual income.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, shrine_temple_administrative_complexes, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, shrine_temple_administrative_complexes, beneficiary).

% Tendai and Shingon scholar-monks produce and defend the honji suijaku doctrine as metaphysical truth (kami as suijaku, buddhas as honji), which subordinates kami cults to buddhist cosmology and validates their own interpretive authority over both traditions. They collect prestige, patronage, and doctrinal supremacy from the fusion claim being treated as truth rather than convenience.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, tendai_shingon_doctrinal_authorities, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, tendai_shingon_doctrinal_authorities, beneficiary).

% Court and later bakufu authorities patronize the unified system because it provides a single, totalizing cosmology that legitimates political rule through both kami ancestry claims and buddhist state-protection rituals simultaneously. They benefit from not having to choose between competing legitimation systems.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, ruling_bakufu_and_court_patrons, beneficiary,
    powerful, generational, constrained, national).

% Local kami priests whose cults predate or resist buddhist absorption find their kami reclassified as subordinate manifestations of buddhas rather than autonomous powers, losing interpretive authority and often revenue and land to the combined complexes. Exit means abandoning institutional recognition and patronage networks that now run through the fused system.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, independent_kami_cult_priests, payer,
    moderate, generational, constrained, regional).

% Ordinary worshippers receive a workable ritual life (kami for this-world concerns, buddhas for afterlife) but are told this reflects a single metaphysical truth rather than a practical division of labor; they have no venue to ask whether the ontological unity is literally true and no alternative ritual infrastructure to consult if they doubt it.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, lay_practitioners_seeking_doctrinal_clarity, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, lay_practitioners_seeking_doctrinal_clarity, beneficiary).

% Nativist scholars who later argue kami traditions were an indigenous system corrupted by buddhist overlay are not part of the honji suijaku era's interpretive conversation at all; their objection to the fusion claim arrives centuries later, after the institutional entanglement is already dense and politically load-bearing (eventually feeding the Meiji shinbutsu bunri separation).
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, later_kokugaku_purist_scholars, excluded,
    moderate, civilizational, constrained, national).

% Modern historians of Japanese religion examine honji suijaku doctrine, jingu-ji institutional records, and the later Meiji-era forced separation to assess whether the claimed ontological unity was a genuine metaphysical commitment or a durable institutional convenience that outlived scrutiny.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, comparative_religion_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Honji suijaku as ontological unity solves a real coordination problem: it lets a single set of institutions, personnel, and ritual calendars serve both kami veneration and buddhist practice without requiring practitioners or administrators to treat the two traditions as rivals, avoiding costly doctrinal conflict and duplicated infrastructure.
% TRANSFER_FUNCTION: Interpretive authority and revenue move from independent kami cult priests and lay doubters toward the combined shrine-temple complexes and the Tendai/Shingon doctrinal authorities who author and police the fusion claim; political legitimacy flows to court and bakufu patrons who no longer must choose one cosmology over the other.
% ABSENT_VOICES: Independent kami priests displaced by absorption, and later kokugaku scholars who would argue the 'unity' was constructed rather than discovered, are structurally absent from the doctrinal conversation that produced and sustained the honji suijaku framework — the former lack the venue, the latter arrive only after institutional facts are set.
% DISAPPEARANCE_RATIONALE: If the ontological-unity claim were withdrawn (as it effectively was at the Meiji shinbutsu bunri separation of 1868), combined shrine-temple complexes would be forcibly split, land and personnel reallocated, kami and buddhist institutions administratively divorced, and doctrinal authority over kami reverted to newly independent Shinto institutions — precisely what happened historically, demonstrating the arrangement's world was load-bearing, not descriptive of an inert fact.
% FOUNDING_PROBLEM: Medieval Japanese religious institutions needed to explain and administratively integrate the coexistence of indigenous kami veneration with an imported, doctrinally sophisticated buddhist tradition, avoiding a zero-sum contest between the two for ritual authority, land, and patronage.
% FOUNDING_PROBLEM_CORROBORATION: The Meiji government's 1868 shinbutsu bunri edict, enacted from outside the beneficiary institutions and against their explicit objection, formally declared the fusion administratively separable and dismantled it within a generation; modern historians of Japanese religion (outside both the medieval jingu-ji complexes and the doctrinal lineages that authored honji suijaku) treat the separation as evidence the 'ontological unity' was institutionally contingent rather than metaphysically necessary.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__syncretic_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_substrate__syncretic_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 at t=1000) reflects the steady transfer of interpretive authority and revenue from independent kami priests toward the combined administrative complexes and doctrinal authorities over the medieval period, rising as jingu-ji institutions consolidated landholdings under the fusion doctrine. Suppression (0.62) is authored higher than extraction alone would suggest because maintaining the ontological-unity claim as metaphysical truth (rather than functional convenience) required active doctrinal policing — subordinating rival kami-only interpretations and treating dissent as theological error rather than legitimate alternative reading. Theater ratio (0.28) stays moderate-low: the coordination function (avoiding costly sectarian conflict, sharing ritual infrastructure) is genuinely functional for most of the interval, not pure performance, though its performative share grows as institutional entrenchment outpaces live theological need.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of the Tendai/Shingon doctrinal authorities and the combined shrine-temple complexes, honji suijaku is discovered metaphysical truth that happens to organize institutions efficiently — a rope. From the seat of independent kami priests and later kokugaku scholars, the same 'truth claim' operates as an enforced subordination that could not survive contact with an outside adjudicator (and did not survive the Meiji state's shinbutsu bunri). The engine's per-seat computation should register this asymmetry: agenda_setter seats compute coordination-dominant, payer seats compute extraction-dominant, from the identical structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional beneficiaries (shrine-temple complexes, doctrinal authorities, court/bakufu patrons) sit near the beneficiary end of directionality: they hold arbitrage-grade or constrained-but-privileged exit and collect the doctrinal and material benefits of fusion being treated as necessary rather than optional. Independent kami priests and lay practitioners sit near the target end: their exit is constrained or trapped, and the fusion claim's persistence directly costs them autonomy or clarity. Later kokugaku scholars are excluded rather than coordinated — their absence from the founding conversation is structural, not incidental.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding zero-sum doctrinal contest between kami and buddhist traditions) was genuinely live in the early medieval period, which is why this reading is authored as tangled_rope rather than pure snare — real coordination value existed. But founding_problem_status is declared dead: by the time of stable bakufu rule, the practical coordination problem was largely solved, yet the ontological-unity claim persisted and hardened into doctrine backed by institutional enforcement, extracting from dissenting seats long after the original problem had been resolved by simpler functional coexistence. The Meiji-era forced separation is the outside corroboration that the metaphysical claim was institutionally contingent, not metaphysically necessary — exactly the mismatch (status=dead, verdict=world_rearranges) the R5 interview is designed to surface as a capture flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fusion_reading_versus_domain_partition,
    'Is the honji suijaku doctrine better modeled as a genuine ontological-unity commitment (this reading) or as a functional domain-partition where kami and buddhas coexist without metaphysical identity claims (the sibling domain_partition_reading)?',
    'Close textual analysis of medieval doctrinal treatises (e.g. Ryobu Shinto texts, Tendai honji suijaku commentaries) to determine whether practitioners and theologians of the period treated the identity claim as literally true metaphysics or as a licensing convention for institutional coexistence; cross-reference with how quickly and completely the arrangement dissolved once state pressure (Meiji shinbutsu bunri) removed institutional incentives to maintain it.',
    'If the historical record supports domain_partition more than fusion, this story''s high accessibility_collapse and resistance-to-separation framing overstates the metaphysical commitment and understates the purely institutional convenience — reclassification toward a less entangled tangled_rope or even rope would follow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fusion_reading_versus_domain_partition, conceptual, 'Whether medieval sources support genuine ontological fusion versus functional domain partition.').

omega_variable(
    fusion_versus_incoherent_bundle,
    'Did honji suijaku ever constitute a single coherent kernel at all, or is ''the honji suijaku doctrine'' itself a retrospective simplification of centuries of varied, locally negotiated, state-pressured institutional arrangements with no unifying commitment (the sibling incoherent_bundle_reading)?',
    'Comparative survey of regional jingu-ji institutional records across different centuries and provinces to test whether a stable, shared doctrinal commitment is discernible or whether local variation and opportunistic state intervention dominate the historical pattern.',
    'If the bundle reading is more accurate, the ease and speed of the 1868 shinbutsu bunri separation is better explained by absence of a real kernel to dissolve, rather than by this reading''s account of a metaphysical claim losing institutional cover — this would weaken the tangled_rope classification''s coordination-function premise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fusion_versus_incoherent_bundle, conceptual, 'Whether a single coherent ontological kernel existed, or only accumulated institutional drift.').

omega_variable(
    meiji_separation_as_natural_experiment,
    'Does the speed and administrative completeness of the 1868 shinbutsu bunri separation constitute strong evidence that the fusion claim was institutionally contingent rather than metaphysically necessary?',
    'Historical analysis of implementation records: how quickly complexes were split, how much resistance was mounted by doctrinal authorities versus lay practitioners, and whether any residual fused institutions persisted informally after the formal edict.',
    'Rapid, relatively low-resistance separation would corroborate the founding_problem_status=''dead'' + disappearance_verdict=''world_rearranges'' mismatch flag as a genuine capture signature; significant persistence of informal fusion practices would suggest the ontological commitment had deeper purchase than pure institutional convenience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_separation_as_natural_experiment, empirical, 'Whether the historical separation event corroborates institutional contingency over metaphysical necessity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 200, 0.16).
narrative_ontology:measurement(shin_tr_t400, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 400, 0.2).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 600, 0.24).
narrative_ontology:measurement(shin_tr_t800, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 800, 0.27).
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1000, 0.28).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(shin_be_t200, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 200, 0.4).
narrative_ontology:measurement(shin_be_t400, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 400, 0.48).
narrative_ontology:measurement(shin_be_t600, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 600, 0.53).
narrative_ontology:measurement(shin_be_t800, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 800, 0.56).
narrative_ontology:measurement(shin_be_t1000, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1000, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(shin_su_t200, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 200, 0.43).
narrative_ontology:measurement(shin_su_t400, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 400, 0.5).
narrative_ontology:measurement(shin_su_t600, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 600, 0.55).
narrative_ontology:measurement(shin_su_t800, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 800, 0.6).
narrative_ontology:measurement(shin_su_t1000, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1000, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__syncretic_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.1).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the shinbutsu_ontological_substrate kernel, decomposed per the epsilon-invariance principle: syncretic_fusion_reading (this story, tangled_rope — high entanglement, doctrinal enforcement of metaphysical unity claim), domain_partition_reading (a distinct, less totalizing story where kami/buddhas govern separate functional domains without ontological identity claims — likely lower extraction, closer to rope), and incoherent_bundle_reading (a story denying any coherent kernel exists, treating the whole arrangement as state-enforced institutional drift — likely piton or snare depending on whose enforcement is emphasized). Each reading carries its own epsilon and its own beneficiary/victim structure; they are linked here rather than merged because measuring 'honji suijaku' by different observables (doctrinal texts vs. institutional records vs. political enforcement patterns) yields materially different epsilon values, which per the epsilon-invariance principle means they are different constraints, not one constraint viewed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
