% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__domain_partition_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__domain_partition_reading
 *   human_readable: Domain Partition of Kami and Buddhas (Functional Coexistence Reading)
 *   domain: religious/institutional/philosophical
 *
 * SUMMARY:
 *   From roughly the 9th century onward, the imperial court and Buddhist
 *   monasteries maintained an accommodation in which kami govern this-worldly
 *   affairs (agriculture, protection of the realm, seasonal cycles) while
 *   buddhas govern the transcendent realm (enlightenment, the afterlife,
 *   metaphysical nature). This reading instantiates the domain-partition
 *   interpretation: the two religious systems operate as coordinated but
 *   fundamentally separate commitments, not as merged ontologies. Syncretism
 *   under this reading is pragmatic institutional coexistence, not
 *   theological synthesis. The constraint's persistence depends on both the
 *   imperial authority's validation of Shinto's domain and the Buddhist
 *   monasteries' acceptance of kami as a legitimate (if subordinate) order—a
 *   settlement that avoids forcing both systems into a single metaphysical
 *   frame. The claim is rope (genuine coordination); the metrics reflect
 *   modest extraction and low theatrical overhead, consistent with a
 *   functional if somewhat tension-laden arrangement.
 *
 * KEY AGENTS:
 *   - Imperial court: maintains domain partition doctrine as state policy; benefits from stable allocation of spiritual authority without having to adjudicate metaphysical disputes
 *   - Buddhist monasteries (esp. Tendai): accept kami governance over this-world in exchange for exclusive jurisdiction over enlightenment and afterlife; extract institutional authority and material support
 *   - Shinto shrine networks: recognized as legitimately governing this-worldly kami cults; subordinate to Buddhist cosmological framing but autonomous in their domain
 *   - Popular religious practitioners: navigate both systems' domains pragmatically; no unified theory required for worship
 *   - Philosophical contesters (syncretic vs incoherent): intellectual parties who question whether the partition is ontologically defensible or merely institutional convenience
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, 0.32).
domain_priors:suppression_score(shinbutsu_ontological_substrate__domain_partition_reading, 0.28).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__domain_partition_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__domain_partition_reading, "Domain Partition of Kami and Buddhas (Functional Coexistence Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__domain_partition_reading, "religious/institutional/philosophical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__domain_partition_reading, '0047cb41-118d-49a8-8b66-26849216f1e1').
narrative_ontology:cs_kernel_codification('0047cb41-118d-49a8-8b66-26849216f1e1', distributed).
narrative_ontology:cs_authority_grounding('0047cb41-118d-49a8-8b66-26849216f1e1', lineage).
narrative_ontology:cs_interpretation_layer_present('0047cb41-118d-49a8-8b66-26849216f1e1').
narrative_ontology:cs_reading_relation('0047cb41-118d-49a8-8b66-26849216f1e1', shinbutsu_ontological_substrate__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('0047cb41-118d-49a8-8b66-26849216f1e1', shinbutsu_ontological_substrate__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('0047cb41-118d-49a8-8b66-26849216f1e1', foundational, ontological_partition_claim).
narrative_ontology:cs_axiom_status(ontological_partition_claim, holdable).
narrative_ontology:cs_axiom_grounding('0047cb41-118d-49a8-8b66-26849216f1e1', ontological_partition_claim, deontological).
narrative_ontology:cs_axiom('0047cb41-118d-49a8-8b66-26849216f1e1', foundational, functional_coexistence_without_merger).
narrative_ontology:cs_axiom_status(functional_coexistence_without_merger, holdable).
narrative_ontology:cs_axiom_grounding('0047cb41-118d-49a8-8b66-26849216f1e1', functional_coexistence_without_merger, conventional).
narrative_ontology:cs_axiom('0047cb41-118d-49a8-8b66-26849216f1e1', secondary, kami_governance_of_phenomenal_order).
narrative_ontology:cs_axiom_status(kami_governance_of_phenomenal_order, holdable).
narrative_ontology:cs_axiom_grounding('0047cb41-118d-49a8-8b66-26849216f1e1', kami_governance_of_phenomenal_order, conventional).
narrative_ontology:cs_reference_frame('0047cb41-118d-49a8-8b66-26849216f1e1', separate_ontological_domains_framework).
narrative_ontology:cs_drift_state('0047cb41-118d-49a8-8b66-26849216f1e1', contemporary_practice_vs_doctrine, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0047cb41-118d-49a8-8b66-26849216f1e1', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, imperial_court).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_monasteries).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, shinto_shrine_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, popular_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_monasteries).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__domain_partition_reading, shinto_shrine_networks).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__domain_partition_reading, court_officials_managing_religion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and defends the domain-partition doctrine as official state religious policy. Uses the doctrine to avoid having to choose between Buddhism and Shinto, instead ratifying both as legitimate. Enforces the separation through court decrees and shrine/monastery management. Extracts authority consolidation and political stability from the arrangement without having to resolve metaphysical disputes. Has the option to shift toward full Buddhist adoption (following Tang China), full Shinto centralism (rejecting foreign religion), or syncretic merger, but each would destabilize different constituencies.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, imperial_court, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain exclusive jurisdiction over metaphysical and transcendent domains, consolidate doctrinal coherence (especially Tendai honji suijaku framework), and secure material support and institutional autonomy from the court. Constrained by the need to accept kami governance over this-worldly matters and not claim metaphysical authority over the phenomenal realm. Cannot abandon the partition without losing their privileged institutional status, and cannot claim that kami are merely manifestations of buddhas (despite doctrinal arguments for this) without destabilizing the settlement. Hold significant institutional power through land, learning, and connections but remain subordinate to court authority.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_monasteries, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__domain_partition_reading, buddhist_monasteries, payer).

% Receive institutional recognition and autonomy over this-worldly kami cults, agricultural and seasonal rituals, and protection of the realm. Accept subordinate metaphysical ranking—kami are located within or below the Buddhist cosmic order—in exchange for that autonomy. Cannot contest the subordination without losing court protection and Buddhist institutional support. Depend on the partition to prevent monasteries from claiming jurisdiction over kami or converting shrine lands to Buddhist temples wholesale. Their exit option is resistance to the framework, which would trigger court and monastic suppression; constrained by the asymmetry of institutional power.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, shinto_shrine_networks, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__domain_partition_reading, shinto_shrine_networks, payer).

% Navigate both systems pragmatically without requiring unified metaphysical theory: pray to kami for harvests, protection, and this-worldly concerns; appeal to buddhas for enlightenment, merit transfer, and transcendent salvation. The partition makes sense lived experientially—different domains, different resources, no contradiction. No extraction is apparent at the practitioner level; they collect the benefit of stable dual access without bearing institutional costs. Exit is straightforward: adopt idiosyncratic religious synthesis or neither system, with local social cost but no structural barrier. The constraint is nearly transparent to their experience.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, popular_practitioners, beneficiary,
    powerless, biographical, mobile, local).

% Represent intellectual traditions (especially later Tendai, Nichiren Buddhism, and some Shinto scholars) that question the partition's ontological coherence. Point out that honji suijaku language in Buddhist texts suggests kami and buddhas are unified beneath the surface, not truly separate. Remain partially excluded from the state religious apparatus because their doctrinal positions threaten the partition settlement. Can publish and debate but cannot overtly claim court or shrine authority as long as the partition holds. Constrained by the institutional power defending the framework; would exit if the syncretic_fusion_reading gained court backing, or find common ground if incoherent_bundle_reading became acknowledged consensus.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, philosophical_contesters, excluded,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__domain_partition_reading, philosophical_contesters, observer).

% Manage day-to-day institutional coordination between Buddhist and Shinto spheres: approve shrine construction, manage monastic privileges, settle disputes over land or ritual authority, and defend the partition doctrine against challenge. Gain prestige and power from managing the system but are constrained by the need to keep both monasteries and shrines satisfied. Directly feel the tension when practitioners blur domains (shrine associated with transcendent ritual, monastery claiming this-worldly protective power) and must repeatedly reassert the boundary. Constrain their own exit: cannot shift the framework without losing their administrative function. This role is distinct from the abstract imperial court; these officials have biographical time horizons and experience the constraint as active work, not settled policy.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, court_officials_managing_religion, agenda_setter,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__domain_partition_reading, court_officials_managing_religion, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates spiritual authority across two major institutional traditions (Buddhism and Shinto) without requiring either to subsume the other or forcing the state to choose between them. Solves the problem of legitimizing both foreign Buddhist institutional sophistication and indigenous Shinto authority by declaring them non-competing domains: buddhas govern the transcendent/metaphysical, kami govern the this-worldly/phenomenal. Avoids destabilizing sectarian conflict and allows both systems to develop institutional autonomy.
% TRANSFER_FUNCTION: Moves institutional authority, court protection, material support, and legitimacy to Buddhist monasteries and Shinto shrine networks, conditional on their acceptance of domain subordination (monasteries acknowledge kami as a legitimate but non-equivalent order; shrines accept kami's metaphysical subordination). The court retains the power to arbitrate boundary disputes and enforce the partition through policy. No direct financial transfer under this reading, but control of shrine land, monastic privileges, and ritual prerogatives follows the domain allocation.
% ABSENT_VOICES: Syncretic practitioners who experience buddhas and kami as unified or interchangeable (excluded from official doctrine but likely present in lived practice); metaphysically rigorous philosophers who find the partition incoherent and argue for either full fusion or full separation; foreign Buddhist movements that reject the partition and claim buddhas' universal jurisdiction; indigenous religious specialists who predate state Shinto formalization and might resist the institutional framing entirely. These voices are structurally excluded from the state apparatus that maintains the doctrine, though some surface as scholarly debate.
% DISAPPEARANCE_RATIONALE: If the domain-partition doctrine vanished overnight, the religious landscape would reorganize rapidly. Buddhist monasteries would face pressure to claim or defend against claims to this-worldly authority; Shinto shrines would lose institutional justification for autonomy and face absorption into Buddhist institutions or imperial reorganization; practitioners would face doctrinal confusion or pressure to choose one system; the state would lose a framework for legitimizing both traditions simultaneously. Within years, the system would likely shift toward one of the sibling readings (fuller syncretism, explicit incoherence, or explicit subordination).
% FOUNDING_PROBLEM: In the 8th–9th centuries, the imperial court faced the problem of legitimizing Buddhism (a sophisticated continental religion with institutional power, associated with continental prestige and technical knowledge) while preserving the authority of indigenous kami cults (which gave the court and Japanese populace deep cultural roots and spiritual legitimacy tied to the land itself). The court could not choose only Buddhism without alienating the populace and losing indigenous legitimacy; it could not reject Buddhism without losing continental prestige and institutional sophistication. The partition doctrine solved this by declaring kami and buddhas to inhabit fundamentally separate domains—neither competes with the other, both remain legitimate, the state can support both.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by court chronicles and monastic records establishing the 9th-century context of simultaneous Buddhism expansion and Shinto institutional formalization. However, whether the founding problem remains LIVE is disputed: the court and mainstream institutional Buddhism maintain the partition remains essential to religious stability; philosophical traditions and contemporary scholars increasingly argue the partition is a fiction maintained for political convenience, and the true (syncretic_fusion) or actual (incoherent_bundle) situation has changed. No neutral party outside both Buddhism and Shinto institutional interests has full authority to settle the status; the testimonies are faction-indexed.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__domain_partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 0.32, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).
:- end_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is modest (0.32) because the partition genuinely coordinates two institutional domains without requiring either to subsume the other—each party gains jurisdictional autonomy it would not otherwise have. The court gets to avoid adjudicating metaphysical disputes; monasteries get to maintain doctrinal coherence within their domain; shrine networks get institutional recognition. Suppression is low (0.28) because the partition is not actively enforced through exclusion mechanisms—practitioners naturally route prayers to kami for this-world concerns and to buddhas for transcendent ones, and institutional actors have aligned interests in preserving the separation. Theater is minimal (0.18) because the arrangement's function (separating domains) is its stated purpose; there is little performative maintenance beyond doctrinal statements. Accessibility_collapse is moderate (0.42) because alternatives do exist historically—full syncretism, metaphysical unification, or pure rejection of one or the other—but are politically costly or doctrinally incoherent. Resistance is moderate-high (0.55) because philosophical traditions contest the partition (the syncretic_fusion_reading has deep textual roots in Tendai), and popular practice sometimes blurs the domains despite official doctrine. The measurement series shows a slight upward drift in extractiveness over the interval, reflecting increasing institutional complexity and the court's growing ability to leverage the partition for political control—a slow accumulation of overhead without fundamental structural change. Theater drifts upward as well, suggesting increasing doctrinal justification work needed to maintain the separation as lived practice drifts toward greater syncretism.
 *
 * PERSPECTIVAL GAP:
 *   From the court's seat, this is a successful coordination solving the problem of religious authority distribution without destabilizing the state. From the monastery seat, it is a pragmatic settlement that preserves Buddhist doctrinal integrity while accommodating indigenous kami cults. From the shrine seat, it is subordinating but stabilizing—kami are recognized but ranked below buddhas metaphysically. From the populace's perspective, it is workable and natural—different domains have different spiritual resources, which is obvious to lived experience. From the philosophical contesters' seat, it is unstable and eventually incoherent—honji suijaku language in Tendai texts suggests the unified ontology is the true frame, and the partition is just institutional convenience that will eventually collapse into the syncretic_fusion_reading or become unjustifiable (incoherent_bundle_reading). Each seat experiences a different constraint type, and the engine should surface that divergence as the primary finding.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial court sits as the agenda-setter (institutional power, defines state doctrine, enforces the separation via policy), with directionality near the beneficiary end—it collects jurisdictional autonomy and authority consolidation. Buddhist monasteries are partly beneficiary (gain exclusive transcendent domain, material support) and partly payer (constrained in this-worldly claims, subject to court oversight); their d sits near 0.35–0.45 (moderate with slight payer tilt, dependent on their cooperation). Shinto shrine networks are coordinated (autonomous kami domain, institutional recognition) but subordinate in the cosmic hierarchy; their d sits around 0.40 (symmetric-to-slightly-payer, as they gain autonomy but accept metaphysical subordination). Popular practitioners are beneficiaries with near-zero extraction burden—they use both systems unambiguously; d near 0.05. Philosophical contesters are observers (d = analytical); they experience the constraint as an intellectual puzzle rather than a lived arrangement. The asymmetry here is subtle: the court's power to enforce the doctrine is greater than anyone else's power to contest it, so the court's d stays lower (more beneficiary-like) than the monasteries' d, despite the latter's greater institutional resources.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is the problem of coordinating two major religious traditions that the court had incentive to legitimize: Buddhism brought institutional sophistication and continental prestige, while Shinto preserved indigenous authority and legitimacy with the populace. The partition doctrine solved this by declaring them non-competing. The founding problem status is CONTESTED: the court maintained it was live (need to coordinate religious traditions without centralizing authority dangerously), while the philosophical tradition increasingly questioned it (if buddhas and kami are truly separate, why do they inhabit the same sacred landscape and ritual calendar?). The partition reading avoids mandatrophy by asserting that the founding problem—allocating spiritual authority across two traditions—remains live and functional: the separation continues to serve that purpose. However, the sibling readings open the mandatrophy path: if syncretic_fusion is correct, the partition doctrine is cover for a more unified reality and has outlived its framing value; if incoherent_bundle is correct, there never was a stable kernel and the entire arrangement is zombie-like performance. This reading's stability depends on the domain partition holding as a genuine commitment, not as drift—hence the omega on ontological vs functional boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_functional_boundary,
    'Is the partition between kami and buddha domains genuinely ontological (they govern fundamentally different orders of reality), or is it a pragmatic institutional arrangement that papers over deeper metaphysical incoherence?',
    'Historical textual analysis of court doctrine (Tendai commentaries on honji suijaku) and shrine institutional records distinguishing preservation-of-separation language from merger-under-surface language. Comparison with syncretic_fusion_reading''s metaphysical claims.',
    'If genuinely ontological, the reading stands as a stable commitment to separate but coexistent domains. If purely pragmatic, the constraint is an unstable equilibrium and the incoherent_bundle_reading becomes more plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_vs_functional_boundary, conceptual, 'Whether the domain partition reflects true ontological distinction or institutional pragmatism').

omega_variable(
    extraction_cover_story_risk,
    'Does the partition doctrine provide institutional cover for the imperial court and monasteries to extract authority over mutually exclusive jurisdictions (temporal vs spiritual) without each claiming the other''s domain?',
    'Analysis of how the partition was invoked to defend institutional turf: did court cite separation to exclude monastic interference in temporal affairs, or did it equally protect monastic autonomy? Asymmetry would signal the doctrine covers extractive institutional capture.',
    'If asymmetric, the constraint shifts from rope (genuine coordination of separate domains) to tangled_rope (coordination + extraction) or snare (cover for institutional monopoly by seat). If symmetric, the coordination frame holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_cover_story_risk, empirical, 'Whether domain partition serves symmetric coordination or asymmetric institutional control').

omega_variable(
    sibling_reading_empirical_status,
    'Which sibling reading—syncretic_fusion_reading (honji suijaku as metaphysical truth) or incoherent_bundle_reading (syncretism as drift without kernel)—does the historical record support?',
    'Canonical Buddhist texts from Tendai and Nichiren schools on honji suijaku; shrine doctrinal statements on kami nature; court policy documents on separation. Presence of explicit kernel language (boundary assertions) vs. absence (de facto accommodation).',
    'If syncretic_fusion has textual support, this reading''s axiom ontological_partition_claim faces axiom_overriding drift. If incoherent_bundle is supported, the partition reading is the closest-fit coherent framing among three-way incoherence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_empirical_status, empirical, 'Historical support for sibling readings determines this reading''s standing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__domain_partition_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(shin_tr_t3, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 3, 0.11).
narrative_ontology:measurement(shin_tr_t6, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement(shin_tr_t9, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 9, 0.16).
narrative_ontology:measurement(shin_tr_t12, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 12, 0.18).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(shin_be_t3, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 3, 0.24).
narrative_ontology:measurement(shin_be_t6, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 6, 0.28).
narrative_ontology:measurement(shin_be_t9, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 9, 0.31).
narrative_ontology:measurement(shin_be_t12, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 12, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0, 0.14).
narrative_ontology:measurement(shin_su_t3, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 3, 0.18).
narrative_ontology:measurement(shin_su_t6, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 6, 0.22).
narrative_ontology:measurement(shin_su_t9, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 9, 0.25).
narrative_ontology:measurement(shin_su_t12, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 12, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__domain_partition_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__domain_partition_reading, 0.1).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% Part of shinbutsu_ontological_substrate kernel family. Domain-partition reading instantiates a structural commitment to separate but coordinate domains. Sibling readings advance alternative framings of the same historical phenomenon: syncretic_fusion claims metaphysical unity, incoherent_bundle claims absence of coherent kernel. The three readings decompose the contested kernel via ε-invariance: each reading has a stable, reading-indexed ε for the standing arrangement it describes (domain partition, metaphysical fusion, institutional drift), and they cannot coexist within a single reading framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
