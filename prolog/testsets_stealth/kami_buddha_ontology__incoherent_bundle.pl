% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__incoherent_bundle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__incoherent_bundle, []).

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
 *   constraint_id: kami_buddha_ontology__incoherent_bundle
 *   human_readable: Shinbutsu-Shugo Fused Cultic Economy (Incoherent-Bundle Reading)
 *   domain: religious studies/japanese cultural history
 *
 * SUMMARY:
 *   From the Nara period onward, Japanese religious institutions fused the
 *   cults of indigenous kami with Buddhist institutions: shrines received
 *   attached temples (jinguji), kami were ordained as bodhisattvas and read
 *   sutras, and doctrinal lineages produced successive systematizations
 *   (honji-suijaku, Ryobu Shinto, Sanno Shinto, Yoshida Shinto). This story
 *   instantiates the incoherent-bundle reading of the kami-buddha ontology
 *   kernel: the arrangement was never one ontology but a bundle of
 *   contradictory commitments - fusion and separation practiced
 *   simultaneously, hierarchy (kami subordinate to buddhas) and reciprocity
 *   (kami as protectors of dharma and realm) held at once, elaborate
 *   systematization coexisting with wholly unsystematized parish practice.
 *   What sustained the bundle was not doctrinal coherence but institutional
 *   inertia and demonstrated ritual efficacy: the fused economy delivered
 *   lifecycle coverage, legitimation, and revenue, and every attempt to
 *   separate the strands before 1868 failed for want of coercive capacity.
 *   Constraint-family note (epsilon-invariance decomposition): the colloquial
 *   label 'Shinbutsu-shugo' covers three structurally distinct claims,
 *   written as three linked stories - honji_suijaku_monism (ontological
 *   identity, hierarchical), domain_partition (functional division of labor),
 *   and this file (no-ontology institutional bundle). The epsilon values
 *   differ because the constraints differ: monism's epsilon attaches to the
 *   subordination of kami to buddhas; partition's epsilon attaches to a
 *   comparatively clean division of labor; this story's epsilon attaches to
 *   the double obligations, cult absorption, and funerary monopoly of the
 *   bundle as actually operated.
 *
 * KEY AGENTS:
 *   - buddhist_monastic_establishments: agenda-setter and primary beneficiary (institutional/arbitrage) - administers the fusion, runs the funerary monopoly, absorbs kami cults
 *   - shrine_priestly_lineages: dual-positioned beneficiary-payer (organized/identity_locked) - gains rank and revenue, cedes interpretive authority over its own deities
 *   - court_and_bakufu_regimes: agenda-setter and beneficiary (institutional/mobile) - purchases legitimation, issues fusion mandates, manages inter-institutional conflict
 *   - village_dues_payers: primary target (organized/constrained) - bears doubled obligations to temple and shrine alike; mounts ikki when exactions spike
 *   - autonomous_kami_cults: primary target (powerless/trapped) - local deities reinterpreted and absorbed; the cult cannot move its god
 *   - shugendo_mountain_ascetics: secondary beneficiary (moderate/mobile) - thrives on tolerated mixture without administering it
 *   - exclusivist_reform_movements: excluded voice (moderate/identity_locked) - kokugaku and Pure Land/Nichiren exclusivists object for centuries without a seat
 *   - religious_studies_historians: analytical observer (analytical/analytical) - sees the full contradictory structure no medieval party articulated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, 0.68).
domain_priors:suppression_score(kami_buddha_ontology__incoherent_bundle, 0.66).
domain_priors:theater_ratio(kami_buddha_ontology__incoherent_bundle, 0.53).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, extractiveness, 0.68).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, theater_ratio, 0.53).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__incoherent_bundle, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__incoherent_bundle, "Shinbutsu-Shugo Fused Cultic Economy (Incoherent-Bundle Reading)").
narrative_ontology:topic_domain(kami_buddha_ontology__incoherent_bundle, "religious studies/japanese cultural history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__incoherent_bundle).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__incoherent_bundle, '8c60294d-e8bb-4879-9437-d9705406d89e').
narrative_ontology:cs_kernel_codification('8c60294d-e8bb-4879-9437-d9705406d89e', distributed).
narrative_ontology:cs_authority_grounding('8c60294d-e8bb-4879-9437-d9705406d89e', practice).
narrative_ontology:cs_interpretation_layer_present('8c60294d-e8bb-4879-9437-d9705406d89e').
narrative_ontology:cs_reading_relation('8c60294d-e8bb-4879-9437-d9705406d89e', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('8c60294d-e8bb-4879-9437-d9705406d89e', kami_buddha_ontology__domain_partition, coexists_with).
narrative_ontology:cs_axiom('8c60294d-e8bb-4879-9437-d9705406d89e', foundational, ritual_efficacy_outranks_doctrinal_coherence).
narrative_ontology:cs_axiom_status(ritual_efficacy_outranks_doctrinal_coherence, holdable).
narrative_ontology:cs_axiom_grounding('8c60294d-e8bb-4879-9437-d9705406d89e', ritual_efficacy_outranks_doctrinal_coherence, empirically_contingent).
narrative_ontology:cs_axiom('8c60294d-e8bb-4879-9437-d9705406d89e', secondary, institutional_inertia_preserves_contradictory_commitments).
narrative_ontology:cs_axiom_status(institutional_inertia_preserves_contradictory_commitments, holdable).
narrative_ontology:cs_axiom_grounding('8c60294d-e8bb-4879-9437-d9705406d89e', institutional_inertia_preserves_contradictory_commitments, empirically_contingent).
narrative_ontology:cs_reference_frame('8c60294d-e8bb-4879-9437-d9705406d89e', pragmatic_unsynthesized_bundle).
narrative_ontology:cs_drift_state('8c60294d-e8bb-4879-9437-d9705406d89e', meiji_separation_edicts, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('8c60294d-e8bb-4879-9437-d9705406d89e', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, buddhist_monastic_establishments).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, shrine_priestly_lineages).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, court_and_bakufu_regimes).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, shugendo_mountain_ascetics).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, village_dues_payers).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, autonomous_kami_cults).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, shrine_priestly_lineages).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__incoherent_bundle, ritual_efficacy_primacy).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__incoherent_bundle, doctrine_practice_decoupling).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Builds and administers the fused arrangement: attaches temples to shrines (jinguji), ordains kami as bodhisattvas, produces the doctrinal systematizations, and operates the funerary monopoly under which every household registers with a temple. Collects estate rents, funeral and memorial fees, and the revenue of absorbed shrine complexes. Its classificatory machinery can fold any new cult into the existing order, so it never needs to leave the arrangement it runs.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, buddhist_monastic_establishments, agenda_setter,
    institutional, generational, arbitrage, national).

% Hereditary custodians of particular shrines. Association with Buddhist institutions brings state rank, doctrinal respectability, and material support; the price is ceding interpretive authority over their own deities, routing shrine finances through attached temples, and absorbing purity burdens that Buddhist death-handling creates. The priesthood is bound to a specific place and lineage: abandoning the post means ceasing to be what the family is.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, shrine_priestly_lineages, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, shrine_priestly_lineages, payer).

% Issues the edicts that mandate offerings to kami as protectors of the dharma and the realm, ranks shrines, and patronizes both institutions. Draws legitimation from the fused order (Hachiman for warrior governments, Ise for the court) and pays for it in patronage obligations and the recurring cost of adjudicating temple-shrine conflicts. Whichever regime holds power inherits the same machinery.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, court_and_bakufu_regimes, agenda_setter,
    institutional, civilizational, mobile, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, court_and_bakufu_regimes, beneficiary).

% Households registered with a temple under the parishioner system owe funeral and memorial fees and annual certification, and separately owe festival levies and purification offerings to shrines. Land-bound and legally enrolled, they cannot decline either obligation; when exactions spike they withhold, flee to other jurisdictions, or rise in armed protest (ikki), which is periodically punished and periodically accommodated.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, village_dues_payers, payer,
    organized, biographical, constrained, local).

% Local deity cults without Buddhist affiliation. When the fusion apparatus reaches a region, the deity is reinterpreted as a manifestation or protector within the Buddhist scheme, an attached temple is founded, and cult revenues are administered through the temple complex. A deity cannot be relocated, and the community tending it rarely has standing to refuse; absorption proceeds cult by cult.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, autonomous_kami_cults, payer,
    powerless, generational, trapped, local).

% Itinerant mountain practitioners who combine kami veneration, esoteric Buddhist technique, and folk healing freely. The tolerated mixture is precisely their operating environment: they move between patrons, regions, and institutions at will, collecting fees for rites that draw on both sides of the boundary everyone else polices.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, shugendo_mountain_ascetics, beneficiary,
    moderate, biographical, mobile, regional).

% Kokugaku nativist scholars, Pure Land devotees, and Nichiren-lineage polemicists who insist the fusion is contamination or slander and that kami worship or the Lotus teaching must stand alone. For centuries they publish, teach, and petition without holding any seat in shrine or temple administration; their oppositional commitment is constitutive, so joining the fused order would dissolve the movement itself.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, exclusivist_reform_movements, excluded,
    moderate, generational, identity_locked, national).

% Modern scholarship reconstructing the arrangement from doctrinal treatises, shrine ledgers, parish registers, and festival accounts. Positioned to see the full contradictory structure - the simultaneous fusion and separation, hierarchy and reciprocity - that no medieval party articulated as a single position.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, religious_studies_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__incoherent_bundle, buddhist_monastic_establishments).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__incoherent_bundle, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one integrated ritual economy across the archipelago: kami rites handle purification, fertility, and this-worldly benefits; Buddhist rites handle death, funerals, and posthumous fate; a shared festival calendar, shared sacred geography, and a shared legitimation language let two cultic systems serve the same population without open competition over every rite and every household.
% TRANSFER_FUNCTION: Moves material support - rice levies, estate rents, funeral and memorial fees, festival labor, purification offerings - from village households and estate producers to the combined temple-shrine complex; moves doctrinal authority downward from Buddhist institutions onto kami cults; moves legitimacy upward from local cults into the state-recognized order; returns ritual services (protection, rain, funerals, memorial salvation) to the paying communities.
% ABSENT_VOICES: Exclusivist voices - kokugaku nativists demanding the kami way be purified of Buddhist accretion, Pure Land and Nichiren exclusivists calling the fusion slander - objected for centuries without a seat in shrine or temple administration. The kami cults are doubly absent: deities cannot testify, and the communities that tended them were absorbed before they could bargain over terms.
% DISAPPEARANCE_RATIONALE: If the fused arrangement vanished overnight in, say, 1400: temples lose shrine-attached revenue and rural reach; shrines lose doctrinal cover, funerary partnership, and state rank; villages face two competing cultic economies bidding for the same households instead of one bundled obligation; the court and shogunate must build a new legitimation language. Every major seat's position depends on the bundle.
% FOUNDING_PROBLEM: An imported universal religion (Buddhism) arrives in a land already thick with indigenous deities tied to kin groups, localities, and the court's own origin myths. The Ritsuryo state needed those cults governed, enrolled, and aligned with the new order rather than left as rival sources of authority.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists outside the benefiting parties, and it disputes the problem's framing: kokugaku scholars (Motoori Norinaga, Hirata Atsutane) attested from outside that no integration was ever needed - only purification of Buddhist accretion; Western observers (Kaempfer, Satow, Chamberlain) documented the fused practice and the nineteenth-century dispute over it; Meiji ideologues and Buddhist apologists argued opposite answers, which is why the status is contested rather than live.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__incoherent_bundle, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__incoherent_bundle, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__incoherent_bundle, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kami_buddha_ontology__incoherent_bundle, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__incoherent_bundle, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__incoherent_bundle_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__incoherent_bundle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 at interval end: the danka parishioner system made temple registration and funeral payment compulsory for every household, estate rents flowed to temple-shrine complexes at their medieval peak, and autonomous cults were absorbed without compensation - heavy extraction riding on real services. Suppression is 0.66 and is a raw structural property, unscaled by power or scope: court edicts mandated offerings, bakufu law tied household registration to temples and used annual terauke certificates to police belief, and monastic institutions wielded genuine coercive force (warrior monks, estate enforcement). Theater rises to 0.53 because the doctrinal superstructure decoupled from practice: esoteric transmissions and honji-suijaku treatises were mastered by small specialist lineages while parish religion ran on efficacy and habit, and by the late Edo period terauke certification was becoming perfunctory paperwork. Accessibility_collapse is 0.50 - alternatives (exclusive kami worship, exclusive Buddhism, independent cults) remained visible and practicable at the margins throughout, which is why this is not a natural-law profile. Resistance is 0.55: ikki uprisings against temple-shrine dues, Nichiren's polemics against kami worship, Ippen's exclusivism, and the kokugaku attack on fusion were persistent, organized, and ultimately consequential. The measurement series share one eight-point grid (710-1868) so every metric is authored at every examined time point; trajectories are monotonic rather than cyclical - extraction and enforcement accumulated, theater grew as doctrine decoupled from function.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute divergent types from identical structural data. From the monastic seat the bundle is a coordination machine it built and staffed - a rope-like arrangement delivering lifecycle rites and legitimation. From the village seat the same structure computes as extraction with a liturgical surface - approaching snare territory, since exit was legally closed and coalition resistance (ikki) was met with punishment. The shrine-lineage seat occupies the hinge: gilded subordination, beneficiary in revenue and rank, payer in authority and purity costs. The excluded reformer seat experiences the bundle as corruption sustained by inertia. Identity-lock binds two seats differently: shrine lineages are locked by relational and institutional identity (hereditary office at a fixed place), exclusivist movements by ideological identity (opposition constitutes the movement). The engine derives these per-seat classifications from power, exit, and directionality data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Buddhist monastic establishments sit near the beneficiary pole: they collect fees, rents, and absorbed-cult revenue, with arbitrage-grade flexibility to reinterpret anything. Court and bakufu regimes derive near-full beneficiary from their beneficiary role, but they purchase legitimation and bankroll conflict management, so an override lifts d to 0.22. Shrine priestly lineages are the clearest override case: the derivation reads their beneficiary declaration and would push d toward roughly 0.15, but they cede interpretive authority over their own deities, route shrine finances through temple complexes, and cannot leave hereditary posts - net position near symmetric, d 0.45. Village dues payers sit near the target pole (d approximately 0.9): double obligations, constrained exit, land-bound. Autonomous kami cults sit nearest full target (d approximately 0.95): trapped, absorbed, without standing. Shugendo ascetics are mobile beneficiaries (d approximately 0.25) who profit from tolerated mixture. Excluded reformers hold no transfer position; their exclusion is the enforcement object at the margins.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical errors. Reading the bundle as pure snare erases its genuine coordination function - one integrated lifecycle economy, shared sacred geography, a working legitimation language - which is why tangled_rope, not snare, is the authored claim. Reading it as piton (pure inertia) fails because concentrated beneficiaries actively maintained it: monasteries protested, fought, and lobbied for the arrangement for a millennium, and no seat was hurt enough to dismantle it before a modern state acquired the capacity. On the R5 interview the founding problem is contested rather than dead, so the capture/zombie mismatch does not fire; but the theater trajectory (0.20 to 0.53) tracks the mandate aging - doctrine decoupling from function - and had the Meiji state not intervened, the bundle's drift path ran toward piton as enforcement ossified around a funerary paperwork monopoly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_incoherent_bundle,
    'Is the fused arrangement best modeled as one coherent ontology (as the honji_suijaku_monism or domain_partition readings hold) or as an institutionally sustained bundle of contradictory commitments (this reading)?',
    'Systematic comparison of doctrinal corpora against practice records (shrine ledgers, parish registers, festival accounts): if practice consistently tracks a single ontology, this reading collapses into the corresponding sibling.',
    'If a coherent ontology underlies the arrangement, epsilon and classification migrate to the sibling story''s parameters and this file becomes a misdescription of the referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incoherent_bundle, conceptual, 'Which reading of the kami-buddha kernel the historical record supports.').

omega_variable(
    contradiction_functionality,
    'Does each contradiction in the bundle serve a distinct function - fusion serving monastic expansion, separation practices serving shrine purity and finance - making the bundle a stable equilibrium rather than mere inertia?',
    'Trace which parties enforce which half of each contradiction and who bears the cost when each half wins locally.',
    'If the contradictions are functional, the bundle is robust to reform pressure and sits firmly in hybrid territory; if merely inertial, piton drift becomes likely once the concentrated beneficiaries weaken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contradiction_functionality, empirical, 'Whether the bundle''s contradictions are load-bearing or vestigial.').

omega_variable(
    separation_capacity_contingency,
    'Did pre-Meiji separation attempts fail because the bundle was genuinely viable, or because no actor before 1868 commanded sufficient coercive capacity?',
    'Compare failed attempts (Yoshida exclusivity campaigns, domain-level purges) against the Meiji success, isolating state capacity as the variable.',
    'If capacity explains the outcome, the bundle''s millennium of persistence measures enforcement scarcity rather than religious viability, and a larger share of the measured suppression attaches to this constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separation_capacity_contingency, empirical, 'Whether the bundle persisted by viability or by the absence of overwhelming force.').

omega_variable(
    internalized_syncretism,
    'Is the fusion''s grip on lay practice structural (registration, fees, mandates) or internalized (folk certainty that kami and buddhas are obviously one)?',
    'Post-1868 practice records: if mixed practice persisted after legal separation dismantled the structural machinery, a large share was internalized.',
    'An internalized share means the arrangement outlives its own enforcement - suppression carried by practitioners after the barriers fall, and classification of the post-separation residue changes accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(internalized_syncretism, empirical, 'Structural versus internalized persistence of the fused practice.').

omega_variable(
    extraction_attribution,
    'How much of the measured extraction belongs to the fusion structure itself versus general religious taxation that any temple-shrine system would levy?',
    'Compare extraction levels in fused regions against contemporaneous settings outside the fusion apparatus (sect-exclusive enclaves, the Okinawan cultic economy, Christian-era Kyushu communities).',
    'If most extraction recurs under any arrangement, the epsilon attributable to this constraint drops materially and the bundle''s hybrid character shifts toward its coordination component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_attribution, empirical, 'Attribution of measured extraction to the fusion structure versus generic religious finance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__incoherent_bundle, 710, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t710, kami_buddha_ontology__incoherent_bundle, theater_ratio, 710, 0.2).
narrative_ontology:measurement_basis(kami_tr_t710, observed).
narrative_ontology:measurement(kami_tr_t900, kami_buddha_ontology__incoherent_bundle, theater_ratio, 900, 0.25).
narrative_ontology:measurement_basis(kami_tr_t900, observed).
narrative_ontology:measurement(kami_tr_t1150, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1150, 0.33).
narrative_ontology:measurement_basis(kami_tr_t1150, observed).
narrative_ontology:measurement(kami_tr_t1350, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1350, 0.38).
narrative_ontology:measurement_basis(kami_tr_t1350, observed).
narrative_ontology:measurement(kami_tr_t1550, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1550, 0.42).
narrative_ontology:measurement_basis(kami_tr_t1550, observed).
narrative_ontology:measurement(kami_tr_t1700, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1700, 0.47).
narrative_ontology:measurement_basis(kami_tr_t1700, observed).
narrative_ontology:measurement(kami_tr_t1800, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1800, 0.5).
narrative_ontology:measurement_basis(kami_tr_t1800, observed).
narrative_ontology:measurement(kami_tr_t1868, kami_buddha_ontology__incoherent_bundle, theater_ratio, 1868, 0.53).
narrative_ontology:measurement_basis(kami_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(kami_be_t710, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 710, 0.32).
narrative_ontology:measurement_basis(kami_be_t710, observed).
narrative_ontology:measurement(kami_be_t900, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 900, 0.4).
narrative_ontology:measurement_basis(kami_be_t900, observed).
narrative_ontology:measurement(kami_be_t1150, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1150, 0.5).
narrative_ontology:measurement_basis(kami_be_t1150, observed).
narrative_ontology:measurement(kami_be_t1350, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1350, 0.58).
narrative_ontology:measurement_basis(kami_be_t1350, observed).
narrative_ontology:measurement(kami_be_t1550, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1550, 0.61).
narrative_ontology:measurement_basis(kami_be_t1550, observed).
narrative_ontology:measurement(kami_be_t1700, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1700, 0.64).
narrative_ontology:measurement_basis(kami_be_t1700, observed).
narrative_ontology:measurement(kami_be_t1800, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1800, 0.66).
narrative_ontology:measurement_basis(kami_be_t1800, observed).
narrative_ontology:measurement(kami_be_t1868, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 1868, 0.68).
narrative_ontology:measurement_basis(kami_be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t710, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 710, 0.3).
narrative_ontology:measurement_basis(kami_su_t710, observed).
narrative_ontology:measurement(kami_su_t900, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 900, 0.38).
narrative_ontology:measurement_basis(kami_su_t900, observed).
narrative_ontology:measurement(kami_su_t1150, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1150, 0.48).
narrative_ontology:measurement_basis(kami_su_t1150, observed).
narrative_ontology:measurement(kami_su_t1350, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1350, 0.55).
narrative_ontology:measurement_basis(kami_su_t1350, observed).
narrative_ontology:measurement(kami_su_t1550, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1550, 0.58).
narrative_ontology:measurement_basis(kami_su_t1550, observed).
narrative_ontology:measurement(kami_su_t1700, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1700, 0.62).
narrative_ontology:measurement_basis(kami_su_t1700, observed).
narrative_ontology:measurement(kami_su_t1800, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1800, 0.64).
narrative_ontology:measurement_basis(kami_su_t1800, observed).
narrative_ontology:measurement(kami_su_t1868, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 1868, 0.66).
narrative_ontology:measurement_basis(kami_su_t1868, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__incoherent_bundle, resource_allocation).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, domain_partition).

% DUAL FORMULATION NOTE:
% The colloquial label 'Shinbutsu-shugo' decomposes per the epsilon-invariance principle into three structurally distinct claims: ontological identity (honji_suijaku_monism), functional partition (domain_partition), and no-ontology institutional bundle (this file). Each carries its own epsilon, beneficiaries, and victims. Monism is the doctrinal upstream - its hierarchical ontology was cited to justify the bundle's operations - while this story is the practice-side downstream that monism was invoked to explain; partition describes the separation practices that coexist inside the bundle. All three files link one another through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__incoherent_bundle, organized, 0.45).
constraint_indexing:directionality_override(kami_buddha_ontology__incoherent_bundle, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
