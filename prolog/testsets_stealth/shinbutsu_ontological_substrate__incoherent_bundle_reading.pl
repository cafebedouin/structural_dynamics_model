% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__incoherent_bundle_reading
 *   human_readable: Enforced Kami-Buddha Fusion as Institutional Accretion (Incoherent-Bundle Reading)
 *   domain: religious/history/political
 *
 * SUMMARY:
 *   From the ninth century to the Meiji separation edicts, Japanese religious
 *   life was organized by an enforced fusion of kami worship and Buddhism:
 *   shrine-temple complexes, tutelary shrine chapels on temple grounds,
 *   doctrine declaring local kami to be manifestations of buddhas (honji
 *   suijaku), and, in its mature Tokugawa form, compulsory parish
 *   registration at Buddhist temples. This story authors the
 *   incoherent_bundle_reading of that arrangement: that no coherent
 *   theological kernel ever governed it; that the fusion grew by accretion,
 *   each layer added for fiscal or administrative reasons; and that what held
 *   it together was state enforcement, not shared commitment. On this reading
 *   the beneficiaries were the throne (unified ritual legitimacy) and the
 *   great monastic complexes (land, dues, funeral monopolies, doctrinal
 *   supremacy over the kami); the bearers of cost were village households
 *   paying doubled obligations and hereditary shrine priests administering
 *   the subordination of their own deities. The epsilon referent is the
 *   standing fusion arrangement itself, assessed by this reading's lights;
 *   this reading endorses no alternative arrangement. Claim and metrics are
 *   authored independently: the claim states snare because this reading finds
 *   no genuine coordination function in the enforced ontological content
 *   itself, while the metrics describe the arrangement's actual operation;
 *   the engine computes per-seat classifications from the structural data.
 *   KEY AGENTS (by structural relationship): - imperial_state: agenda-setter
 *   (powerful/arbitrage) — mandated fusion by statute, collected legitimacy
 *   and administrative reach - great_temple_establishments: primary
 *   beneficiary (institutional/mobile) — administered shrines, collected
 *   dues, defined kami as subordinate traces - hereditary_shrine_priests:
 *   primary bearer of cost (moderate/trapped) — lineage-bound office, bore
 *   subordination and doctrinal contradiction locally -
 *   village_practitioners: primary bearer of cost (powerless/constrained) —
 *   paid doubled obligations, participated in an unreconciled ritual system -
 *   yoshida_nativist_priesthood: excluded voice (moderate/constrained) —
 *   asserted kami primacy from outside the enforced arrangement -
 *   ise_segregationist_priesthood: excluded voice (moderate/constrained) —
 *   maintained internal segregation of buddhas from the shrine -
 *   modern_religion_scholars: analytical observer (analytical/analytical) —
 *   sees the full structure across the archive
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.72).
domain_priors:suppression_score(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.78).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__incoherent_bundle_reading, snare).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__incoherent_bundle_reading, "Enforced Kami-Buddha Fusion as Institutional Accretion (Incoherent-Bundle Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__incoherent_bundle_reading, "religious/history/political").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'a8858d2e-6255-490f-a436-44c7324ef300').
narrative_ontology:cs_kernel_codification('a8858d2e-6255-490f-a436-44c7324ef300', distributed).
narrative_ontology:cs_authority_grounding('a8858d2e-6255-490f-a436-44c7324ef300', extraction).
narrative_ontology:cs_interpretation_layer_present('a8858d2e-6255-490f-a436-44c7324ef300').
narrative_ontology:cs_reading_relation('a8858d2e-6255-490f-a436-44c7324ef300', shinbutsu_ontological_substrate__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('a8858d2e-6255-490f-a436-44c7324ef300', shinbutsu_ontological_substrate__domain_partition_reading, forecloses).
narrative_ontology:cs_axiom('a8858d2e-6255-490f-a436-44c7324ef300', foundational, no_coherent_ontological_substrate).
narrative_ontology:cs_axiom_status(no_coherent_ontological_substrate, holdable).
narrative_ontology:cs_axiom_grounding('a8858d2e-6255-490f-a436-44c7324ef300', no_coherent_ontological_substrate, empirically_contingent).
narrative_ontology:cs_axiom('a8858d2e-6255-490f-a436-44c7324ef300', secondary, persistence_requires_state_enforcement).
narrative_ontology:cs_axiom_status(persistence_requires_state_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('a8858d2e-6255-490f-a436-44c7324ef300', persistence_requires_state_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('a8858d2e-6255-490f-a436-44c7324ef300', enforced_institutional_drift).
narrative_ontology:cs_drift_state('a8858d2e-6255-490f-a436-44c7324ef300', meiji_bunri_repudiation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('a8858d2e-6255-490f-a436-44c7324ef300', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, imperial_state).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, great_temple_establishments).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, village_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, hereditary_shrine_priests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% From the ritsuryo codes onward the court placed kami worship and Buddhist affairs under a single administrative order, and later warrior governments made temple affiliation compulsory for every household. The fused cult gave the throne a unified ritual language of legitimacy and let officials govern all cultic activity through one apparatus. The state could redefine the arrangement at will, as it did in 1868 when it ordered the opposite separation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, imperial_state, agenda_setter,
    powerful, generational, arbitrage, national).

% Great monastic complexes and their branches took over administration of thousands of shrines, appointing abbots over shrine clergy, collecting dues and labor from shrine lands, and preaching that local kami were manifestations of buddhas. Ritual fees, funeral rights, and estate income flowed to the temples, and doctrinal authority over the kami rested with them. Their property networks let them shift resources among regions when one locality resisted.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, great_temple_establishments, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, great_temple_establishments, agenda_setter).

% Families holding shrine office by birth served the kami rites while answering to temple superiors, performing Buddhist ordination of their own deities and reciting doctrines that ranked the kami below the buddhas they supposedly manifested. Leaving the office meant dissolving the lineage's livelihood and standing; staying meant administering a system that subordinated their own deity. Petitions for relief went through the very temple hierarchy they objected to.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, hereditary_shrine_priests, payer,
    moderate, generational, trapped, local).

% Farming households owed dues and labor to both shrine festival economies and temple funeral regimes, were registered at temples under compulsory affiliation, and moved through a ritual calendar that offered no single account of what the kami and buddhas were to each other. Participation was compelled by village membership and household registration; belief was left to reconcile itself, and the records show complaints about doubled burdens far more often than statements of unified faith.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, village_practitioners, payer,
    powerless, biographical, constrained, local).

% A priestly lineage centered on Yoshida Shinto argued from the fifteenth century onward that the kami were original and the buddhas derivative, the exact inversion of the official doctrine. They stood outside the court-temple settlement, gained a foothold by selling certification to shrine priests in the Tokugawa period, and supplied the intellectual arsenal the nineteenth-century separation edicts drew on.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, yoshida_nativist_priesthood, excluded,
    moderate, biographical, constrained, national).

% The priests of the Grand Shrine maintained a standing exception: buddhas were barred from the precincts, Buddhist vocabulary avoided, and the shrine's deity kept apart from the fused order. They could not leave the system, since the shrine existed inside the state's ritual order, but they demonstrated daily, by exclusion, that the arrangement's universality was not self-evident.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, ise_segregationist_priesthood, excluded,
    moderate, generational, constrained, local).

% Historians of Japanese religion working from estate documents, liturgical manuals, and village registers read the whole arc from the ninth century to the separation edicts. From this seat the sequence appears as accretion: each layer added for administrative or fiscal reasons, doctrine trailing practice, coherence asserted after the fact. The seat pays nothing and collects nothing; it sees the structure entire.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, modern_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__incoherent_bundle_reading, great_temple_establishments).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__incoherent_bundle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It integrated an indigenous cultic landscape and an imported monastic order into one administrable religious system: a single legal framework for festivals, funerals, land dues, clerical ranks, and royal legitimation, so that the court and later the warrior government could govern all cultic activity through one apparatus instead of negotiating with hundreds of autonomous shrine cults.
% TRANSFER_FUNCTION: It moved rice dues, labor service, and ritual fees from farming households and shrine communities up to monastic complexes and state coffers; moved doctrinal authority to temple hierarchies, who defined the kami as subordinate manifestations; and moved legitimacy upward to the throne, which presided over a ritually unified realm.
% ABSENT_VOICES: Kami-centered lineages, the Ise priesthood, and the Yoshida and kokugaku scholars objected that the kami were primary rather than derivative; their objections circulated outside the court-temple discourse for centuries. Village households that resented doubled obligations had no forum at all; their discontent surfaces in the record only as scattered complaints until the political opening of the late Tokugawa period.
% DISAPPEARANCE_RATIONALE: When the arrangement was abolished in 1868 it did not fade: shrines were stripped of Buddhist imagery, thousands of temples were demolished, monks were forcibly laicized, shrine and temple finances collapsed, and a new state cult had to be constructed from the wreckage. Every institution built on the fused order, including land tenure, parish registration, and festival finance, had to be rebuilt, which is the signature of a world arranged around the thing removed.
% FOUNDING_PROBLEM: The early fusion answered a concrete integration problem: how to absorb hundreds of local kami cults into a universal monastic religion without open conflict, by giving indigenous deities scriptural dignity, rooting new temples in local ground through tutelary shrines, and binding regional elites to the center through shared cult.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists from outside the beneficiary set: the Watarai and Yoshida lineages and Motoori Norinaga documented the doctrinal incoherence from the sixteenth century on; the Meiji reformers' own separation edicts declared the fusion a historical aberration to be corrected; and modern historians of Japanese religion, working from estate and village records, corroborate that the founding integration was achieved centuries before the arrangement's end, leaving the remainder sustained by fiscal privilege and compulsion.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_substrate__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.72) because the mature arrangement transferred a substantial share of rural surplus through compulsory parish registration, funeral monopolies, and shrine dues decoupled from any service households could refuse. Suppression (0.78) is authored as a raw structural property — statute, household registration, and the anti-Christian apparatus that made participation in the fused order a loyalty test — and is deliberately not scaled by power or scope; only extractiveness is scaled downstream by directionality and scope. Theater (0.55) rises across the interval as doctrinal coherence became something asserted in manuals and performed in rites rather than lived: the rituals kept functioning locally while the unifying account hollowed, a Goodhart drift toward proxy maintenance. Accessibility collapse sits at 0.50 because alternatives never vanished — the Ise enclave, kami-primacy lineages, and the looseness of folk practice persisted — but were politically fenced rather than logically impossible. Resistance (0.55) reflects a continuous nativist current that ultimately succeeded. The declared coordination type (enforcement_mechanism) names the administrative layer the arrangement genuinely provided, one legal apparatus for cultic governance; the snare claim attaches to the enforced ontological content riding on that apparatus, which is where this reading locates the transfer. The measurement series share one eight-point grid (900-1850); all three metrics rise together in an enforcement ratchet rather than oscillating, and the terminal collapse of 1868 lies just outside the interval, so the series ends at the mature extractive plateau rather than the wreck.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the state and monastic seats the arrangement is an order they built, staffed, and defended: continuity, legitimacy, and income all depend on it, and its incoherence is hard to see from inside because each layer was adopted for a reason that made sense at adoption. From the shrine-priest seat the same structure is hereditary subordination; the office cannot be left without dissolving the lineage, an identity-lock that fuses household, deity, and duty, and breaking that frame was historically the precondition for the nativist turn. From the villager seat it is doubled obligation with no reconciling account. The excluded nativist seats perceive the seam most sharply, since their objection presupposes seeing that the official account does not hold, and the analytical seat sees the whole accretion. Because the arrangement did solve a real administrative problem, seats weighted toward that layer may compute a hybrid coordination-and-transfer type; this reading's claim weights the enforced ontological content, whose operative function was protecting the transfer, as decisive. Nothing in the authored claim adjudicates between these computations; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (imperial_state, great_temple_establishments) drive those seats toward the subsidized end of the directionality scale; the victim declarations (village_practitioners, hereditary_shrine_priests) drive them toward the full-target end, amplified by trapped and constrained exit. The temples' mobile exit keeps them short of full subsidy despite their capture of the material gains. The excluded seats sit outside the beneficiary/victim derivation, since they neither collect nor pay under the arrangement, and the observer seat carries the analytical atom. No directionality overrides are authored: the declarations plus exit options already place every seated agent correctly, and the one genuine ambiguity, that villagers did receive funerals and festival goods alongside their obligations, is carried by the lived-coherence omega rather than papered over with an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem died around the twelfth century, once the kami cults were absorbed and administrable; the arrangement then persisted roughly seven further centuries on fiscal privilege and legal compulsion. That is the mismatch the genealogy battery is built to catch, founding_problem_status dead paired with a world that rearranges on removal, and it routes to the capture/zombie flag rather than to the inertial remnant path: theater rose as the unifying function atrophied, but a concentrated capturer, the monastic complexes, actively maintained the machinery, which is what separates an enforced arrangement from a merely neglected one. Reading the arrangement as pure coordination because it once solved a real integration problem would launder seven centuries of rent collection; reading it as pure performance would miss that the enforcement was real and the transfers were real. The classification keeps both facts visible: genuine administrative integration below, enforced extraction riding on it above, with the balance computed per seat rather than asserted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the shinbutsu_ontological_substrate kernel; what structural facts would distinguish it from the syncretic_fusion and domain_partition readings?',
    'Comparative documentary analysis: if elite doctrine constituted a lived unified commitment, the fusion reading gains; if administrative records show domain-separated operation, the partition reading gains; if records show ad hoc accretion under compulsion, this reading stands.',
    'Under the fusion reading the epsilon referent becomes a genuine unified commitment and the measured transfer reads partly as coordination cost; under the partition reading the arrangement decomposes into two weaker constraints; under this reading the snare classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: which reading of the kernel the surviving evidence best supports.').

omega_variable(
    lived_coherence_vs_juxtaposition,
    'Did ordinary practitioners experience the fused system as a coherent whole, or as contradictory juxtaposition borne without resolution?',
    'Village registers, diaries of mid-level priests and peasants, etoki performance evidence, and complaint records about doubled dues and obligations.',
    'Lived coherence would push the arrangement toward a hybrid coordination-and-transfer classification; confirmed unresolved contradiction supports the full snare reading and raises the effective burden on the practitioner seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lived_coherence_vs_juxtaposition, empirical, 'Whether the fusion was lived as unity or endured as unreconciled contradiction.').

omega_variable(
    compliance_mechanism_ambiguity,
    'Was practitioner compliance structural (statute, parish registration, estate law) or internalized (no perceived contradiction, habituated participation)?',
    'Post-1868 trajectory: when compulsion lifted, did fused practice persist voluntarily, indicating internalization, or collapse rapidly, indicating structural dependence?',
    'If substantially internalized, suppression outlived its legal machinery and the effective suppression exceeds the structural measure; if structural, removal of enforcement dissolves the arrangement, as the rapid dismantlement of 1868-71 suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_mechanism_ambiguity, empirical, 'Structural versus internalized compliance mechanism in the enforced fusion.').

omega_variable(
    counterfactual_enforcement_relaxation,
    'Was enforcement load-bearing throughout, or would the fusion have persisted voluntarily at some earlier point absent state backing?',
    'Natural experiments from periods of weakened central authority, particularly the Sengoku fifteenth and sixteenth centuries: did shrine-temple bonds loosen where enforcement lapsed, or did local elites maintain the fusion on their own?',
    'If the fusion self-sustained during enforcement lapses, part of the arrangement rests on latent commitment and the snare reading overstates coercion; if it required continuous enforcement, the reading stands and the enforcement ratchet is the true persistence mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_enforcement_relaxation, conceptual, 'Counterfactual test of whether state enforcement was the load-bearing element of persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__incoherent_bundle_reading, 900, 1850).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 900, 0.2).
narrative_ontology:measurement(shin_tr_t1050, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1050, 0.28).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1200, 0.33).
narrative_ontology:measurement(shin_tr_t1350, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1350, 0.38).
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1500, 0.42).
narrative_ontology:measurement(shin_tr_t1650, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1650, 0.48).
narrative_ontology:measurement(shin_tr_t1750, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1750, 0.52).
narrative_ontology:measurement(shin_tr_t1850, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1850, 0.55).

% Extraction over time
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 900, 0.35).
narrative_ontology:measurement(shin_be_t1050, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1050, 0.45).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1200, 0.55).
narrative_ontology:measurement(shin_be_t1350, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1350, 0.6).
narrative_ontology:measurement(shin_be_t1500, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1500, 0.62).
narrative_ontology:measurement(shin_be_t1650, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1650, 0.7).
narrative_ontology:measurement(shin_be_t1750, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1750, 0.71).
narrative_ontology:measurement(shin_be_t1850, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1850, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t900, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 900, 0.3).
narrative_ontology:measurement(shin_su_t1050, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1050, 0.4).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1200, 0.5).
narrative_ontology:measurement(shin_su_t1350, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1350, 0.55).
narrative_ontology:measurement(shin_su_t1500, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1500, 0.58).
narrative_ontology:measurement(shin_su_t1650, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1650, 0.75).
narrative_ontology:measurement(shin_su_t1750, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1750, 0.77).
narrative_ontology:measurement(shin_su_t1850, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1850, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__incoherent_bundle_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, domain_partition_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'shinbutsu shugo': the label conflates a metaphysical claim (syncretic_fusion_reading), a jurisdictional claim (domain_partition_reading), and a socio-institutional claim (this file). Each carries its own epsilon, its own victim structure, and its own classification. Historically the fusion reading supplied the legitimating doctrine that this reading identifies as post-hoc cover for the transfer machinery, so the doctrinal story functions as upstream justification for the institutional one; both siblings are linked here via affects_constraints, and this story's epsilon (0.72, referent: the enforced accretion) diverges sharply from what a fusion-reading story would author over the same arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
