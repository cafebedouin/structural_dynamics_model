% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__amun_polytheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Priestly-Mediated Divine Legitimacy under Amun-Ra Imperial Patronage
 *   domain: religious/political-economic (ancient Near East)
 *
 * SUMMARY:
 *   In the Amun-polytheistic settlement, divine legitimacy flows through
 *   established priestly interpretation of a plural cosmology headed by
 *   Amun-Ra. The pharaoh reigns as pivot: confirmed in divine sonship by the
 *   interpretive apparatus, obliged to endow the temples that validate him.
 *   Temple economies - above all the Theban Amun complex - accumulate land,
 *   labor, and exemption privileges across the interval, while peasants bear
 *   corvee and non-exempt landholders carry disproportionate assessment.
 *   Regional cults and local variation are accommodated inside the framework
 *   rather than suppressed. This file is one member of a constraint family
 *   decomposing the divine_legitimacy_substrate kernel; its epsilon (0.62 at
 *   interval end) is indexed to the priestly-mediated arrangement as it
 *   stood, with extraction weighted by the documented coercive instruments
 *   and offset by festival, granary, and funerary provision. The sibling
 *   readings carry different epsilon over different beneficiary structures
 *   and are linked via network.affects_constraints. KEY AGENTS (by structural
 *   relationship): - theban_amun_priesthood: Agenda-setter
 *   (institutional/identity_locked) - administers interpretation and the
 *   largest estate economy - pharaoh: Dual-positioned beneficiary-payer
 *   (powerful/identity_locked) - legitimated and constrained by the same
 *   apparatus - regional_cult_estates: Secondary beneficiaries
 *   (institutional/constrained) - accommodated under the settlement -
 *   corvee_peasant_laborers: Primary targets (powerless/trapped) - bear levy
 *   and surplus transfer - secular_landholders: Targets
 *   (organized/constrained) - bear disproportionate assessment via exemption
 *   asymmetry - festival_participating_populace: Beneficiary-payer
 *   (powerless/constrained) - receives the providential face, carries the
 *   indirect cost - scribal_administrative_class: Analytical observer
 *   (moderate/mobile) - sees the full ledger across palace and temple -
 *   subject_foreign_populations: Excluded (powerless/trapped) - governed by
 *   the arrangement, absent from it
 *
 * KEY AGENTS:
 *   - theban_amun_priesthood: agenda-setter seat (institutional power, identity_locked exit) - runs interpretation, oracles, and the dominant estate economy
 *   - pharaoh: dual beneficiary-payer seat (powerful, identity_locked exit) - receives legitimation, pays endowment and autonomy
 *   - regional_cult_estates: beneficiary seat (institutional, constrained exit) - accommodated regional cults under the settlement
 *   - corvee_peasant_laborers: payer seat (powerless, trapped exit) - corvee and surplus transfer
 *   - secular_landholders: payer seat (organized, constrained exit) - disproportionate assessment from temple exemptions
 *   - festival_participating_populace: beneficiary-payer seat (powerless, constrained exit) - festival access against indirect cost
 *   - scribal_administrative_class: observer seat (moderate, mobile exit) - full-ledger visibility across palace and temple
 *   - subject_foreign_populations: excluded seat (powerless, trapped exit) - imperial subjects outside the interpretive conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__amun_polytheistic_reading, 0.62).
domain_priors:suppression_score(divine_legitimacy_substrate__amun_polytheistic_reading, 0.5).
domain_priors:theater_ratio(divine_legitimacy_substrate__amun_polytheistic_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__amun_polytheistic_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__amun_polytheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__amun_polytheistic_reading, "Priestly-Mediated Divine Legitimacy under Amun-Ra Imperial Patronage").
narrative_ontology:topic_domain(divine_legitimacy_substrate__amun_polytheistic_reading, "religious/political-economic (ancient Near East)").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__amun_polytheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__amun_polytheistic_reading, '7f3eb3d3-66f8-4f32-b4ed-5d898598aeac').
narrative_ontology:cs_kernel_codification('7f3eb3d3-66f8-4f32-b4ed-5d898598aeac', distributed).
narrative_ontology:cs_authority_grounding('7f3eb3d3-66f8-4f32-b4ed-5d898598aeac', lineage).
narrative_ontology:cs_interpretation_layer_present('7f3eb3d3-66f8-4f32-b4ed-5d898598aeac').
narrative_ontology:cs_reading_relation('7f3eb3d3-66f8-4f32-b4ed-5d898598aeac', divine_legitimacy_substrate__atenist_monotheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('7f3eb3d3-66f8-4f32-b4ed-5d898598aeac', divine_legitimacy_substrate__folk_syncretistic_reading, coexists_with).
narrative_ontology:cs_axiom('7f3eb3d3-66f8-4f32-b4ed-5d898598aeac', foundational, legitimacy_flows_through_plural_priestly_interpretation).
narrative_ontology:cs_axiom_status(legitimacy_flows_through_plural_priestly_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('7f3eb3d3-66f8-4f32-b4ed-5d898598aeac', legitimacy_flows_through_plural_priestly_interpretation, theological).
narrative_ontology:cs_axiom('7f3eb3d3-66f8-4f32-b4ed-5d898598aeac', foundational, amun_ra_supreme_imperial_patron).
narrative_ontology:cs_axiom_status(amun_ra_supreme_imperial_patron, holdable).
narrative_ontology:cs_axiom_grounding('7f3eb3d3-66f8-4f32-b4ed-5d898598aeac', amun_ra_supreme_imperial_patron, theological).
narrative_ontology:cs_reference_frame('7f3eb3d3-66f8-4f32-b4ed-5d898598aeac', priestly_validated_maatic_kingship).
narrative_ontology:cs_drift_state('7f3eb3d3-66f8-4f32-b4ed-5d898598aeac', late_ramesside_fragmentation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7f3eb3d3-66f8-4f32-b4ed-5d898598aeac', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__amun_polytheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, theban_amun_priesthood).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, regional_cult_estates).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, festival_participating_populace).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, corvee_peasant_laborers).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, secular_landholders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__amun_polytheistic_reading, festival_participating_populace).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, maat_cosmic_order_doctrine).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, amun_ra_chief_patron_theology).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__amun_polytheistic_reading, pharaonic_divine_birth_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the largest temple economy in the polity: collects grain rents, livestock, and offerings from vast estate holdings, allocates corvee labor, trains scribes, maintains the theological corpus, and performs the daily rituals and oracles through which the gods' will becomes known. Priestly office increasingly passes along family lines. Exit would mean abandoning accumulated land, rank, and sacred knowledge that constitute the officeholder's place in the world.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, theban_amun_priesthood, agenda_setter,
    institutional, generational, identity_locked, national).

% Temples of Ptah at Memphis, Ra at Heliopolis, and the provincial gods hold smaller estates with their own priesthoods. They receive endowments and festival traffic under the Amun-centered settlement and house local cult traditions within the shared cosmology. Their fortunes track royal and Theban patronage decisions they do not control.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, regional_cult_estates, beneficiary,
    institutional, generational, constrained, regional).

% Reigns as the pivot of the arrangement: receives public legitimation through priestly confirmation of divine sonship, funds and endows the temple estates, and appears in festival processions enacting the gods' favor. His autonomy is bounded by the interpretive apparatus - oracles, priestly councils, and doctrine can constrain policy, delay campaigns, and condition a successor's recognition. Leaving the arrangement would mean ceasing to be king; the office and the validation system constitute each other.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, beneficiary,
    powerful, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, pharaoh, payer).

% Work the fields, haul stone, and staff building projects on rotating labor levies owed to temple and state alike. Grain from their surplus fills temple granaries; in exchange they receive festival access, funerary provision, and famine relief when the granaries open. Flight from levy is punishable and movement is monitored through village headmen.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, corvee_peasant_laborers, payer,
    powerless, immediate, trapped, national).

% Hold military and administrative land grants outside temple ownership. Because temple lands carry exemption from levies and assessments, the full weight of state extraction falls disproportionately on their holdings. They petition for relief, dedicate land to temples to escape assessment - feeding the very accumulation that burdens them - or campaign abroad hoping for fresh grants.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, secular_landholders, payer,
    organized, biographical, constrained, national).

% Attend the great river processions when the gods leave their shrines, consult traveling oracles, purchase votives, and rely on temple granaries in bad years. They carry the surplus transfers indirectly through levies and prices, and encounter the arrangement chiefly through its festive and providential face rather than its ledgers.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, festival_participating_populace, beneficiary,
    powerless, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__amun_polytheistic_reading, festival_participating_populace, payer).

% Staff both palace offices and temple scriptoria; literate careers move between the two. They copy the theological corpus, draft endowment charters, record oracle outcomes, and see the whole ledger - who gives, who receives, and what the exemptions cost the treasury.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, scribal_administrative_class, observer,
    moderate, biographical, mobile, national).

% Populations incorporated by conquest in Nubia and the Levant owe tribute and labor to the imperial system and are expected to honor its gods, but hold no seat in the interpretive apparatus and receive none of the temple economy's provisioning. Their own cults persist locally under supervision.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__amun_polytheistic_reading, subject_foreign_populations, excluded,
    powerless, biographical, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__amun_polytheistic_reading, theban_amun_priesthood).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__amun_polytheistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared cosmological framework and a working legitimacy protocol for a large agrarian empire: validates succession, synchronizes the calendar and festival cycle across regions, distributes ritual expertise, links provincial cults into one network, and routes famine relief and funerary provision through temple granaries and mortuary establishments.
% TRANSFER_FUNCTION: Moves agricultural surplus - grain, livestock, labor days - from peasant producers and non-exempt landholdings to temple estates; moves legitimation upward to the reigning pharaoh; moves prestige, land, and administrative careers through priestly networks.
% ABSENT_VOICES: Subject foreign populations have no voice in the interpretive apparatus that governs them. Village practitioners reach the system only through festival mediation and oracle petitions, with no channel to contest an interpretation. Corvee laborers speak only through the exceptional strike (Deir el-Medina, when rations failed outright). The unanimity of the written record reflects who was permitted to write.
% DISAPPEARANCE_RATIONALE: Succession would lose its validation protocol and every reign change would become an open contest among military claimants; the festival calendar, granary relief, and funerary economy would collapse with the estates that fund them; land tenure registered in endowment charters would dissolve into litigation; the scribal class's employment and the transmission of the theological corpus would lapse within a generation.
% FOUNDING_PROBLEM: After the Hyksos expulsion, the restored Theban house needed an imperial-scale legitimacy protocol: how to bind Delta, Valley, and Nubia into one polity, stabilize succession, and honor the war-god whose patronage had delivered liberation. The Amun-Ra synthesis answered it, raising a Theban patron into an imperial chief deity whose priesthood could validate kingship from Elephantine to the Levantine garrisons.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary foreign chanceries (Hittite and Babylonian diplomatic correspondence) treat the god-validating character of Egyptian kingship as operational fact, attesting the system from outside; the Deir el-Medina strike records and the tomb-robbery papyri attest material strain beneath the settlement from below; modern papyrology (endowment stelae, the Wilbour Papyrus) corroborates the scale of the transfer. No contemporary voice outside the benefiting parties disputes that the founding problem was real; the recorded disputes concern who should interpret, not whether validation was needed.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__amun_polytheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__amun_polytheistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__amun_polytheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__amun_polytheistic_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.62 (interval end) and rises monotonically across the shared six-point grid: campaign spoils and royal endowment swell the estates early, then exemption asymmetry compounds the accumulation - by the Deir el-Medina strikes the state cannot pay its own workmen while temple granaries overflow. Suppression is a raw structural property, unscaled by power or scope, and is authored at 0.50 with a deliberate hump in the temporal series: enforcement machinery hardened sharply after the Amarna rupture (memory proscription, tribunal purges) and then decayed as central capacity fragmented - the falling tail reflects enforcement decay, not reduced need. Theater ratio rises from 0.18 to 0.38 as daily liturgy grew more elaborate and less publicly accessible, oracle procedure became stagier, and personal piety emerged as a parallel channel compensating for the closing of direct access. Accessibility collapse is low (0.35) because alternatives never collapsed: local cults, household rites, and regional variation were accommodated inside the framework - that accommodation is constitutive of this reading. Resistance is moderate (0.40): the first recorded labor strikes in history, systematic tomb robbery, landholder tax avoidance via strategic dedication, and one dynastic-scale revolt from within the royal house. All three tracked metrics are authored at every shared time point on one grid.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat the arrangement is the cosmic order itself - maintenance of Maat, not extraction; the priesthood experiences its receipts as the gods' due. From the payer seats the same structure is levy, corvee, and disproportionate assessment. The pharaoh straddles: the apparatus that legitimates him is the apparatus that constrains him, and his exit is closed because the office is constituted by the validation. The populace meets the festive and providential face and carries the indirect cost. The scribal observer sees the whole ledger. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for theban_amun_priesthood, regional_cult_estates, and festival_participating_populace; victim declarations drive high directionality for corvee_peasant_laborers (trapped, powerless - nearest the full-target end), secular_landholders (constrained), and subject_foreign_populations (excluded, trapped, highest d of all). One override is declared: the derivation chain reads the pharaoh's declared beneficiary role and would return a deeply beneficiary-side d, but his legitimation is strictly conditional on endowment and compliance, his exit is identity_locked, and by interval end the Theban establishment could make or unmake reigns - his net structural position is near-symmetric, slightly target-side (d = 0.45). The override applies to the powerful seat, which in this story is occupied only by the pharaoh.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - imperial-scale legitimation and succession stability after the Hyksos expulsion - remained live across the entire interval, so no mandatrophy resolution is declared and no sunset clause is authored. The tangled_rope classification is what prevents mislabeling in both directions: a pure-extraction reading would miss the genuine coordination the arrangement delivers (stable succession across twenty dynastic transitions, famine relief, calendrical and cultural integration of an empire), while a pure-coordination reading would miss the structural accumulation, the exemption asymmetry, and the enforcement machinery the receipts required. The temporal series shows the classic accumulation signature - extraction and theater rising together while the coordination function persists - which is drift within a tangled rope, not proof that coordination was always cover. Fixing cost is prohibitive: the one attempt to replace the arrangement wholesale consumed a dynasty and had to be erased from the record.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This story instantiates the amun_polytheistic_reading of the divine_legitimacy_substrate kernel; epsilon is indexed to the priestly-mediated arrangement as it stood. Does the classification travel across sibling readings, or does each reading constitute a distinct constraint?',
    'Compile the sibling stories (atenist_monotheistic_reading, folk_syncretistic_reading) and compare computed types and effective extraction across the family; the engine''s per-seat classifications over identical structural data would expose any reading-invariance.',
    'If readings are not classification-invariant, cross-reading comparisons of this story''s numbers are invalid and the family must be analyzed reading-by-reading, as authored here.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Reading-indexed nature of epsilon over the shared legitimacy kernel.').

omega_variable(
    atenist_structural_delta,
    'What structurally changes under the atenist_monotheistic_reading, where legitimacy flows solely through pharaonic revelation of Aten and all other gods are false?',
    'Author the sibling story: interpretive authority concentrates in the pharaoh alone, temple estates shift from beneficiaries to expropriated targets, regional variation is foreclosed rather than accommodated, and the enforcement burden rises to sustain exclusivity.',
    'The disagreement between readings is located on the locus-of-interpretive-authority axis (distributed priesthood versus sole royal revelation); whichever reading governs determines who the beneficiaries and victims are, so the two files must not share epsilon or beneficiary sets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(atenist_structural_delta, conceptual, 'Sibling-reading structural delta on the interpretive-authority axis.').

omega_variable(
    folk_substrate_interdependence,
    'Household and village ritual practice persisted beneath and alongside the state cult for the whole interval; how much of this reading''s coordination function depended on folk practice it did not administer, and did temple extraction crowd out or feed village observance?',
    'Comparative analysis of votive deposits, domestic shrines, and festival attendance records against temple endowment growth; the folk_syncretistic_reading sibling story carries the household-level structural data.',
    'If the state reading was parasitic on an independent folk substrate, part of its measured coordination credit belongs elsewhere and its effective extraction is understated; if temple festivals fed village practice, part of the transfer was reciprocated service rather than one-way extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(folk_substrate_interdependence, empirical, 'Dependence of the state reading on the unadministered folk substrate.').

omega_variable(
    constructed_vs_irreducible_mediation,
    'Is priestly mediation of royal legitimacy a constructed, rent-bearing arrangement, or a near-irreducible feature of large agrarian sacral kingship (compare Mesopotamia, Shang China, the Maya)?',
    'Cross-civilizational comparison of sacral-kingship systems at comparable scale and technology: if every large agrarian polity converges on specialist mediators of divine warrant, the arrangement approaches a structural limit and part of the measured extraction is coordination-floor cost.',
    'If near-irreducible, the constraint sits closer to the coordination end than the raw metrics suggest; if contingent on Egyptian choices (estate accumulation, hereditary office), the extraction is discretionary and the classification hardens toward the extraction end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_irreducible_mediation, empirical, 'Naturalness ambiguity of priestly legitimation at imperial scale.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was compliance sustained by structural enforcement (memory proscription after the Amarna episode, oracle control, economic dependency on temple granaries) or by internalized conviction that made alternatives unthinkable?',
    'Behavior under enforcement collapse: in the late Twentieth Dynasty, as central enforcement capacity decayed, temples kept functioning and populations kept participating while strikes and tomb robberies targeted the delivery system, not the cosmology - suggesting a substantial internalized component alongside the structural one.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure and would survive enforcement removal; if structural, the observed late-interval enforcement decay predicts rapid loosening.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized share of measured suppression.').

omega_variable(
    devotional_weight_of_surplus,
    'How much of the temple-bound surplus transfer was experienced by participants as devotion freely given rather than extraction borne?',
    'Reading the votive record, donation inscriptions by non-elites, and festival participation against the levy records: voluntary dedication by secular landholders seeking spiritual merit coexists with coerced corvee in the same ledger.',
    'Epsilon is reading-indexed; a devotional-heavy weighting lowers the extraction this reading reports, a levy-heavy weighting raises it. The scalar authored here weights the documented coercive instruments (levy, exemption asymmetry, proscription) as extraction and festival access as offsetting benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(devotional_weight_of_surplus, preference, 'Devotion-versus-extraction weighting inside the surplus transfer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__amun_polytheistic_reading, 1550, 1070).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t1550, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 1550, 0.18).
narrative_ontology:measurement(divi_tr_t1450, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 1450, 0.21).
narrative_ontology:measurement(divi_tr_t1350, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 1350, 0.24).
narrative_ontology:measurement(divi_tr_t1250, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 1250, 0.28).
narrative_ontology:measurement(divi_tr_t1150, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 1150, 0.33).
narrative_ontology:measurement(divi_tr_t1070, divine_legitimacy_substrate__amun_polytheistic_reading, theater_ratio, 1070, 0.38).

% Extraction over time
narrative_ontology:measurement(divi_be_t1550, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 1550, 0.4).
narrative_ontology:measurement(divi_be_t1450, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 1450, 0.46).
narrative_ontology:measurement(divi_be_t1350, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 1350, 0.5).
narrative_ontology:measurement(divi_be_t1250, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 1250, 0.54).
narrative_ontology:measurement(divi_be_t1150, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 1150, 0.58).
narrative_ontology:measurement(divi_be_t1070, divine_legitimacy_substrate__amun_polytheistic_reading, base_extractiveness, 1070, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t1550, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 1550, 0.35).
narrative_ontology:measurement(divi_su_t1450, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 1450, 0.38).
narrative_ontology:measurement(divi_su_t1350, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 1350, 0.44).
narrative_ontology:measurement(divi_su_t1250, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 1250, 0.6).
narrative_ontology:measurement(divi_su_t1150, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 1150, 0.55).
narrative_ontology:measurement(divi_su_t1070, divine_legitimacy_substrate__amun_polytheistic_reading, suppression_requirement, 1070, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__amun_polytheistic_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, atenist_monotheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__amun_polytheistic_reading, folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Egyptian divine legitimacy' conflates three structurally distinct arrangements sharing one kernel. This file (amun_polytheistic_reading) is the long-dominant instantiation: distributed interpretive authority, temple-economy beneficiaries, accommodated regional variation, epsilon 0.62. The atenist_monotheistic_reading sibling is the revolutionary instantiation: sole pharaonic revelation, expropriated temples, foreclosed variation, high enforcement burden - structurally incompatible with this file (mutual foreclosure). The folk_syncretistic_reading sibling is the persistent household-level substrate: minimal formal extraction, no estate economy, coexisting with this file for the whole interval. Upstream/downstream: this reading's festival calendar and oracle network channel and partially absorb folk practice (influence without foreclosure), and the Atenist sibling could only emerge by first capturing the legitimacy flow this reading administers. Each file carries its own epsilon, beneficiaries, and victims; the family is linked exclusively through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_legitimacy_substrate__amun_polytheistic_reading, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
