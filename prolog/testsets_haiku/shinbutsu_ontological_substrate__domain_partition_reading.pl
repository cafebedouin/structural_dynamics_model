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
 *   human_readable: Kami-Buddha Domain Partition (Functional Coexistence)
 *   domain: religious_studies/institutional_coordination
 *
 * SUMMARY:
 *   This constraint is ONE READING of the contested kernel 'shinbutsu
 *   ontological substrate.' In THIS READING (domain_partition_reading), kami
 *   and buddhas are understood to govern FUNCTIONALLY SEPARATE domains: kami
 *   handle this-world concerns (prosperity, health, reproduction,
 *   protection); buddhas handle otherworldly concerns (salvation,
 *   metaphysical meaning, afterlife welfare). This reading frames syncretism
 *   as pragmatic institutional coexistence, NOT as ontological fusion or
 *   theoretical incoherence. The partition solves a genuine coordination
 *   problem: how to maintain both indigenous religious practice and
 *   institutionalized Buddhism without constant theological conflict. Low
 *   institutional entanglement is the key feature — easy separation is
 *   possible, and coexistence is functional rather than metaphysically
 *   mandated. SIBLING READINGS: domain_partition_reading (this one) coexists
 *   with syncretic_fusion_reading (kami and buddhas are ontologically unified
 *   through honji suijaku doctrine) and incoherent_bundle_reading (no
 *   coherent kernel exists; syncretism is accumulated institutional drift
 *   under state enforcement). These are three readings of ONE kernel, each
 *   generating a different constraint.
 *
 * KEY AGENTS:
 *   - institutional_buddhism_japan: benefits from domain partition; handles soteriological and metaphysical authority without competing with kami claims
 *   - shrine_and_temple_networks: organized religious specialists who maintain both kami and buddha ritual channels; direct resource beneficiaries
 *   - lay_practitioners: benefit from coexistence without cognitive dissonance; can invoke kami and buddha for domain-specific needs
 *   - state_authority (Heian, Edo): agenda-setter and enforcer; uses partition as governance tool to prevent religious conflict and stabilize legitimacy
 *   - syncretic_fusion_advocates: excluded from this reading's framework; their position requires overturning the partition itself
 *   - indigenous_religious_specialists: retain authority over this-world domains without subordination to Buddhism
 *   - doctrinal_rationalists (analytical seat): observe the functional structure from outside; external measurement authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, 0.38).
domain_priors:suppression_score(shinbutsu_ontological_substrate__domain_partition_reading, 0.42).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__domain_partition_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__domain_partition_reading, "Kami-Buddha Domain Partition (Functional Coexistence)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__domain_partition_reading, "religious_studies/institutional_coordination").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__domain_partition_reading, '54c063c5-b926-4adf-8a7f-1d6fd61d23f3').
narrative_ontology:cs_kernel_codification('54c063c5-b926-4adf-8a7f-1d6fd61d23f3', fixed_text).
narrative_ontology:cs_authority_grounding('54c063c5-b926-4adf-8a7f-1d6fd61d23f3', lineage).
narrative_ontology:cs_interpretation_layer_present('54c063c5-b926-4adf-8a7f-1d6fd61d23f3').
narrative_ontology:cs_reading_relation('54c063c5-b926-4adf-8a7f-1d6fd61d23f3', shinbutsu_ontological_substrate__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('54c063c5-b926-4adf-8a7f-1d6fd61d23f3', shinbutsu_ontological_substrate__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('54c063c5-b926-4adf-8a7f-1d6fd61d23f3', foundational, domain_functional_separation).
narrative_ontology:cs_axiom_status(domain_functional_separation, holdable).
narrative_ontology:cs_axiom_grounding('54c063c5-b926-4adf-8a7f-1d6fd61d23f3', domain_functional_separation, conventional).
narrative_ontology:cs_axiom('54c063c5-b926-4adf-8a7f-1d6fd61d23f3', secondary, coexistence_without_merger).
narrative_ontology:cs_axiom_status(coexistence_without_merger, holdable).
narrative_ontology:cs_axiom_grounding('54c063c5-b926-4adf-8a7f-1d6fd61d23f3', coexistence_without_merger, instrumental).
narrative_ontology:cs_reference_frame('54c063c5-b926-4adf-8a7f-1d6fd61d23f3', functional_religious_pluralism).
narrative_ontology:cs_drift_state('54c063c5-b926-4adf-8a7f-1d6fd61d23f3', contemporary_post_meiji_secularization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('54c063c5-b926-4adf-8a7f-1d6fd61d23f3', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, institutional_buddhism_japan).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, shrine_and_temple_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, lay_practitioners).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, indigenous_religious_specialists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buddhism as a unified institutional order, benefiting from the partition: handles salvation narratives, afterlife doctrine, and metaphysical authority without competing with kami-domain claims over this-world concerns. Maintains doctrinal coherence and liturgical authority within a bounded domain. The partition allows Buddhism to coexist with indigenous religious practice without requiring doctrinal merger or contradiction.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, institutional_buddhism_japan, beneficiary,
    institutional, generational, constrained, national).

% Local religious specialists (priests, monks, shrine keepers) benefit from the clear domain partition: they service both kami and Buddha domains through different ritualized channels without requiring unified doctrinal justification. A shrine conducts kami rituals for protection and prosperity (this-world); a temple conducts Buddhist rites for salvation and the afterlife. The partition enables institutional coexistence and resource allocation: practitioners visit both without logical contradiction.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, shrine_and_temple_networks, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__domain_partition_reading, shrine_and_temple_networks, agenda_setter).

% Ordinary people engaging in religious life benefit from the partition: they can pray to kami for rain, safe childbirth, and practical prosperity; they can invoke Buddha for philosophical meaning, salvation, and post-mortem welfare — without experiencing cognitive dissonance or being forced to choose one framework over another. The partition makes both practices accessible without demanding explicit reconciliation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, lay_practitioners, beneficiary,
    powerless, biographical, constrained, local).

% The Heian court and state administrative apparatus benefit from using the partition as a governance framework: the court positions itself as mediator between the two domains, draws legitimacy from both kami-descent narratives and Buddhist patronage, and avoids having to impose doctrinal unity. The partition provides a stable institutional arrangement that reduces religious conflict and allows differentiated state authority over both registers.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, state_authority_heian_period, agenda_setter,
    institutional, generational, arbitrage, national).

% Theological actors (certain Buddhist philosophers, esoteric lineages) arguing for deeper ontological unity through doctrines like honji suijaku (the kami are manifestations of buddhas) are structurally excluded from this reading's framework: their position requires the domains to be theoretically merged, which contradicts the partition's functional logic. They have to work within or against the partition, not coexist peacefully alongside it.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, syncretic_fusion_advocates, excluded,
    powerful, generational, constrained, national).

% Local shrine keepers, ascetics, and kami specialists retain authority over this-world domains (healing, harvest, childbirth, protection, prosperity) without their practices being absorbed into or subordinated to Buddhist institutions. The partition protects their institutional autonomy and ritual competence. They do not compete with Buddhist soteriological claims because those claims operate in a different domain.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, indigenous_religious_specialists, beneficiary,
    moderate, biographical, constrained, regional).

% The Tokugawa shogunate enforces the partition through administrative infrastructure (registration systems, doctrinal monitoring, temple establishment certificates). The state uses the partition to maintain religious stability and prevent sectarian conflict. The partition serves state control: Buddhism is institutionalized, kami practice is regulated, and the clear domain separation prevents theological disputes from destabilizing political order.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, state_authority_edo_period, agenda_setter,
    institutional, generational, arbitrage, national).

% Indigenous cosmologies that might challenge either kami-domain or buddha-domain monopolies on their respective territories are structurally excluded. The partition itself depends on maintaining the domains as exhaustive and non-overlapping; alternative frameworks that cross the boundary or propose a third domain are kept out by the institutional enforcement of the partition.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, competing_cosmologies, excluded,
    moderate, generational, trapped, national).

% Modern scholars and comparative religionists (external analytical seat) examine the partition from outside the system: they observe the functional coordination between kami and buddha domains, note its internal coherence as a practical arrangement, and track how the partition persists despite ideological pressure toward doctrinal merger or explicit rejection.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, doctrinal_rationalists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Partitions religious authority into separate but coexisting domains: kami govern this-world concerns (prosperity, health, protection, reproduction, harvest); buddhas govern otherworldly concerns (salvation, metaphysical meaning, post-mortem welfare). Solves the genuine collective-action problem of maintaining both indigenous religious practice and institutionalized Buddhism without constant theological conflict.
% TRANSFER_FUNCTION: Transfers religious authority, ritual expertise, and resource allocation: institutional Buddhism receives legitimacy and material support (temples, land, patronage) in exchange for handling soteriological and metaphysical functions; kami specialists and shrine networks retain authority over practical, this-world domains and their associated ritual economy. Practitioners allocate devotion, offerings, and ritual participation across both systems based on domain-specific needs.
% ABSENT_VOICES: Theological competitors who reject both kami and buddha authority (indigenous shamanic traditions, Daoist alternatives, later Christian and Islamic voices) are structurally excluded. Advocates for explicit ontological syncretism (honji suijaku philosophers, certain Tendai and Shingon lineages) are present but their position requires overturning the partition itself — they cannot coexist peacefully within the domain_partition_reading.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished — if kami and buddha authority were declared ontologically unified, hierarchically ordered, or mutually exclusive — the entire institutional structure would reorganize. Practitioners would face cognitive and practical demands to choose or merge frameworks. Temple and shrine networks would either consolidate or enter open competition. The state would lose a stable governance tool. Institutional Buddhism would have to defend its soteriological claims against direct competition with kami salvation narratives. The disappearance is not minor; it forces fundamental reordering of Japanese religious life.
% FOUNDING_PROBLEM: How can an indigenous polytheistic kami religion coexist with an institutionalized foreign soteriological religion (Buddhism) without one dominating or eliminating the other? How can a state maintain both without constant theological conflict between the two communities?
% FOUNDING_PROBLEM_CORROBORATION: Institutional Buddhism and shrine networks attest the founding problem is LIVE and the partition solves it — coexistence without merger remains functionally necessary for their institutional existence. State authorities (Heian court, Tokugawa shogunate) attest the founding problem is live from a governance perspective. Syncretic theologians and modern critics attest the founding problem was SOLVED by the partition, but argue the SOLUTION is theoretically incoherent or unjust and should be replaced with explicit fusion or explicit rejection. Modern religious historians (external observers) corroborate that the partition emerged as a practical solution to genuine conflict and persisted as long as state enforcement and institutional incentives supported it.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__domain_partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is measured at 0.38 (moderate, rising slightly over the interval from ~0.25 to 0.41 during Edo consolidation, then stabilizing around 0.38 by modernity). The measurement reflects: (1) Institutional Buddhism benefits from the partition without equivalent competitive cost — it gains doctrinal authority and material support in a bounded domain; (2) State authority extracts regulatory and legitimacy benefit by using the partition as a governance tool; (3) Yet the arrangement is ALSO genuinely coordinative — lay practitioners benefit from coexistence, kami specialists retain autonomy, and the partition prevents the zero-sum conflict that would arise if one domain tried to monopolize all religious authority. Suppression is low-moderate (0.42 because the partition is enforced, especially in Edo, but enforcement is STRUCTURAL not COERCIVE — the arrangement is stable enough that overt coercion is minimal). Theater_ratio is moderate-low (0.29) because the partition performs a real function (holding different domains apart) but increasingly performs theatrical function (post-Edo, the partition persists more by momentum than necessity). The measurement series show: extraction rising through consolidation (Heian→Edo) as the state weaponizes the partition for administrative control, then stabilizing (late Edo→Meiji) as the arrangement calcifies. Theater rises similarly, reflecting that performative maintenance increases as the original coordinative problem becomes less acute and extraction becomes more visible.
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival gap is between institutional beneficiaries (shrine/temple/state) who experience the partition as functional solution and theological competitors (syncretic advocates, monotheistic challengers) who experience it as imposed constraint foreclosing their position. Beneficiaries experience low extraction because they gain from the partition without running it (or they set its terms). Competitors experience high extraction because the partition denies them voice and forces them into silence or cooptation. The engine computes this from structural data (who benefits, who bears costs, who sets terms, what are the exit options). This story does not reconcile the gap; it names the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for institutional_buddhism (beneficiary): d ~0.15 (benefits from the partition without running it; exit via syncretism or rejection is costly but theoretically possible; state patronage sustains the arrangement, not internal necessity). Directionality for shrine_and_temple_networks (beneficiary + agenda_setter): d ~0.25 (set terms and run the partition through daily practice; could migrate to unified framework but institutional identity is fused with domain authority). Directionality for lay_practitioners: d ~0.50 (symmetric; genuine coordination benefit, trapped exit, no choice of framework). Directionality for state_authority: d ~0.20 (benefits from partition as governance tool; high arbitrage exit — state can redeploy the arrangement or abandon it). Directionality for syncretic_fusion_advocates: d ~0.75 (excluded from framework, philosophically at odds with the partition, cannot coexist peacefully within it, must work to overturn it). No directionality_overrides needed; the structural derivation captures the asymmetries.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is NOT manifesting mandatrophy in the domain_partition_reading. The founding problem (how to maintain coexisting kami and buddha authority without conflict) remains LIVE in institutional practice, and the partition continues to solve it functionally. Practitioners still need both domains; state authority still uses the partition for governance; shrine and temple networks still maintain dual practice. The partition is not a zombie constraint whose function has atrophied but whose enforcement persists by inertia. However, a sibling reading (incoherent_bundle_reading) would argue that mandatrophy IS present: the founding problem has been solved (kami and buddha authority are now routinely coexistent without conflict), but the partition persists as institutional drift and state enforcement rather than genuine necessity. This divergence between readings is itself the contested matter — that disagreement is what three separate constraint stories measure. In the domain_partition_reading authored here, mandatrophy is NOT present because the coordination function is active and sustained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_pragmatic_partition,
    'Is the kami-buddha domain partition ontologically real (kami and buddhas genuinely govern separate metaphysical territories) or pragmatically constructed (a functional accommodation that obscures underlying unity or incoherence)?',
    'Textual analysis of foundational religious authorities (Buddhist sutras, Shinto cosmologies, Edo-period theology) to establish whether the partition claims ontological status or pragmatic necessity. Comparison with other religious syncretisms (Daoism + Buddhism in China, Hinduism + Islam in South Asia) to measure whether domain partition is a universal solution or culturally specific accommodation.',
    'If pragmatic, the constraint is a genuine coordinative rope: it solves a real problem with minimal coercive overhead. If ontological, the constraint may be a false natural law (mountain falsely claimed as such) — beneficiaries (Buddhism, state) benefit from treating pragmatic accommodation as metaphysical fact. Classification would shift from rope toward tangled_rope or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontological_vs_pragmatic_partition, conceptual, 'The ontological status of the domain partition — whether it reflects metaphysical reality or practical accommodation.').

omega_variable(
    honji_suijaku_suppression,
    'Are honji suijaku advocates (syncretic fusion position) actively suppressed by the partition framework, or do they coexist as a live alternative reading held by different theological factions?',
    'Historical documentation of how honji suijaku doctrine was treated by state authorities and institutional Buddhism during Heian, Kamakura, Edo periods. Evidence of official censorship, institutional marginalization, or active theological debate — vs. peaceful coexistence of multiple readings within the same tradition.',
    'If actively suppressed: the partition is enforced coercively and extraction from suppressed advocates is real. Suppression metric should be higher; some victims/payers are present. If coexisting: the partition is an accepted working compromise that includes space for alternative readings. Suppression is primarily structural, not coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honji_suijaku_suppression, empirical, 'Whether syncretic fusion alternatives are suppressed or tolerated within the domain partition framework.').

omega_variable(
    internalization_of_partition_after_structural_removal,
    'If the institutional enforcement of the domain partition were removed (state registration abolished, shrine/temple networks autonomous), would practitioners continue to experience kami and buddha as separate domains, or would the suppression dissolve?',
    'Natural experiment: modern Japan since Meiji Restoration where formal state enforcement of the partition has diminished. Observation of whether practitioners maintain dual-domain practice, seek merger, or fragment. Post-exit interviews with individuals leaving rigid religious institutions.',
    'If practitioners maintain partition after enforcement removal: suppression is substantially internalized — practitioners have internalized the domain boundaries. Post-exit suppression persists (high internalization). If practitioners seek merger or abandonment after enforcement removal: suppression is primarily structural — it required active enforcement and dissolves when removed (low internalization). High internalization raises effective suppression (constraint persists through internal mechanism after external removal) and may shift classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalization_of_partition_after_structural_removal, empirical, 'Whether suppression is structural or internalized — measured by post-removal behavior.').

omega_variable(
    state_enforcement_as_extraction_mechanism,
    'Does state enforcement of the partition (Edo administrative machinery, registration systems, doctrinal monitoring) extract benefit for the state beyond the governance stability it provides, or is the suppression purely a side effect of coordination maintenance?',
    'Institutional analysis: does state authority collect direct rents from the partition (tax revenue from temples, political authority delegated through religious networks)? Or does suppression serve only to stabilize governance with no direct gain for the state?',
    'If the state extracts beyond governance benefit: the constraint carries asymmetric extraction from the state seat (state benefits more than it contributes to coordination). The constraint would shift toward tangled_rope or snare. If suppression is purely a coordination side effect: extraction is balanced and the constraint remains rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_as_extraction_mechanism, empirical, 'Whether state enforcement of the partition extracts independent benefit or merely maintains coordination.').

omega_variable(
    alternative_kernel_framings,
    'Could the shinbutsu_ontological_substrate kernel be coherently read as something OTHER than (1) domain partition, (2) syncretic fusion, or (3) incoherent bundle? Are there fourth and fifth readings this corpus has not captured?',
    'Systematic review of historical religious sources (Tendai, Shingon, folk practice, state policy documents, Edo-period theology) for novel framings. Cross-disciplinary comparison with other religious syncretisms. Consultation with religious historians and philosophers working in Japanese Buddhism.',
    'If additional readings exist: the three constraints in this kernel family are incomplete. The classification landscape changes; new vertices in the network emerge. If only three readings cohere: the kernel is adequately decomposed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_kernel_framings, conceptual, 'Whether the three constraint stories (partition, fusion, incoherent) exhaust the coherent readings of the kernel.').

omega_variable(
    reading_committer_frame_uncertainty,
    'Is the domain_partition_reading grounded in how Japanese practitioners and institutions ACTUALLY understood kami and buddha domains, or is it a retrospective rationalization imposed by modern scholarship that wants theological coherence?',
    'Primary source analysis: devotional texts, ritual manuals, state documents, theological commentaries from practitioners during Heian, Kamakura, Edo periods — do they themselves frame the partition as functional coexistence, or do they use different language (merger, hierarchy, contradiction, pragmatic compromise)? Do different periods frame it differently?',
    'If modern rationalization: this reading is a scholar-imposed coherence pattern, not a reading the constraint itself held. The actual historical readings (what practitioners believed) might cluster differently. Classification would shift from genuine rope toward false-summit (claimed coordination masking institutional power). If grounded in practice: this reading captures an actual historical understanding and remains valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_frame_uncertainty, conceptual, 'Whether the domain_partition_reading reflects historical agent understanding or modern scholarly rationalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__domain_partition_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 200, 0.18).
narrative_ontology:measurement(shin_tr_t400, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 400, 0.24).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 600, 0.29).
narrative_ontology:measurement(shin_tr_t800, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 800, 0.31).
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1000, 0.29).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(shin_be_t200, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 200, 0.32).
narrative_ontology:measurement(shin_be_t400, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 400, 0.38).
narrative_ontology:measurement(shin_be_t600, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 600, 0.41).
narrative_ontology:measurement(shin_be_t800, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 800, 0.39).
narrative_ontology:measurement(shin_be_t1000, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1000, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(shin_su_t200, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 200, 0.28).
narrative_ontology:measurement(shin_su_t400, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 400, 0.35).
narrative_ontology:measurement(shin_su_t600, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 600, 0.42).
narrative_ontology:measurement(shin_su_t800, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 800, 0.44).
narrative_ontology:measurement(shin_su_t1000, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1000, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__domain_partition_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% The shinbutsu_ontological_substrate kernel decomposes into three constraint stories, each a reading of the same contested commitment: (1) domain_partition_reading (this story) — kami and buddhas govern separate functional domains; genuine coordination; low extraction; coexistence is pragmatic. (2) syncretic_fusion_reading — kami and buddhas are ontologically unified through honji suijaku; extraction from suppressed alternatives; higher extractiveness. (3) incoherent_bundle_reading — no coherent kernel; syncretism is accumulated institutional drift under state enforcement; maximal extraction. These three stories are not three observations of one constraint — they are three different constraints generated by three different readings of one persisting kernel. The ε values differ substantially: domain_partition ε~0.38 (genuine coordination), fusion ε~0.62 (contested theology), incoherent ε~0.81 (institutional capture). Practitioners and historians disagree about which reading is true; that disagreement is what the three constraint stories measure. The network edges link them: partition influences both siblings by setting the functional baseline; fusion forecloses partition if honji suijaku is metaphysically true; incoherent coexists with both as a critical alternative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
