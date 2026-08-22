% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: shinbutsu_ontological_substrate__domain_partition_reading
 *   human_readable: Kami-Buddha Domain Partition: Functional Coexistence Reading
 *   domain: religious/institutional/commitment_system
 *
 * SUMMARY:
 *   In Japanese religious history, Shinto and Buddhism coexist within a
 *   single institutional landscape. The domain_partition_reading interprets
 *   this coexistence as a coherent theological arrangement: kami (Shinto)
 *   govern this-worldly domains (seasons, warfare, imperial legitimacy,
 *   present welfare) while buddhas and bodhisattvas (Buddhism) govern
 *   soteriological afterlife knowledge and salvation. This reading permits
 *   both priesthood systems to operate under a single state administrative
 *   apparatus without requiring theological merger. Kami need not be buddhas,
 *   and buddhas need not claim kami status — each serves a distinct domain.
 *   The reading is administratively elegant and theologically defensible,
 *   though it sits in contest with syncretic fusion readings (honji suijaku:
 *   buddhas manifest as kami, ontologically unified) and incoherent-bundle
 *   readings (no kernel exists; syncretism is accumulated institutional
 *   drift). This story instantiates the domain_partition reading as a single,
 *   ε-invariant constraint, independent of its sibling readings. The claim
 *   and metrics diverge intentionally: the constraint is claimed as rope
 *   (genuine coordination) while theater measurements document growing
 *   performativity in boundary maintenance, suggesting extractive structure
 *   layering onto coordination. The engine measures this divergence — it is
 *   not a defect.
 *
 * KEY AGENTS:
 *   - shrine_priesthood: administers Shinto ritual, claims kami govern this-world domains, depends on state recognition
 *   - temple_priesthood: administers Buddhist ritual, claims buddhas govern afterlife soteriological knowledge, depends on state patronage
 *   - state_administrative_apparatus: maintains separate administrative tracks, benefits from avoiding theological choice, uses domain partition to coordinate both systems
 *   - lay_population: participates in both shrine and temple, depends on the partition to avoid cognitive dissonance
 *   - syncretic_ideological_movement: excluded from official discourse, advocates honji suijaku (ontological fusion), suppressed in priesthood training
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, 0.31).
domain_priors:suppression_score(shinbutsu_ontological_substrate__domain_partition_reading, 0.28).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__domain_partition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, resistance, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__domain_partition_reading, "Kami-Buddha Domain Partition: Functional Coexistence Reading").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__domain_partition_reading, "religious/institutional/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__domain_partition_reading, 'cd121a16-0603-460c-96e5-b4dfeed423bd').
narrative_ontology:cs_kernel_codification('cd121a16-0603-460c-96e5-b4dfeed423bd', fixed_text).
narrative_ontology:cs_authority_grounding('cd121a16-0603-460c-96e5-b4dfeed423bd', lineage).
narrative_ontology:cs_interpretation_layer_present('cd121a16-0603-460c-96e5-b4dfeed423bd').
narrative_ontology:cs_reading_relation('cd121a16-0603-460c-96e5-b4dfeed423bd', shinbutsu_ontological_substrate__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd121a16-0603-460c-96e5-b4dfeed423bd', shinbutsu_ontological_substrate__incoherent_bundle_reading, influences).
narrative_ontology:cs_axiom('cd121a16-0603-460c-96e5-b4dfeed423bd', foundational, kami_this_world_domain_governance).
narrative_ontology:cs_axiom_status(kami_this_world_domain_governance, holdable).
narrative_ontology:cs_axiom_grounding('cd121a16-0603-460c-96e5-b4dfeed423bd', kami_this_world_domain_governance, conventional).
narrative_ontology:cs_axiom('cd121a16-0603-460c-96e5-b4dfeed423bd', foundational, buddha_afterlife_soteriological_knowledge).
narrative_ontology:cs_axiom_status(buddha_afterlife_soteriological_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('cd121a16-0603-460c-96e5-b4dfeed423bd', buddha_afterlife_soteriological_knowledge, conventional).
narrative_ontology:cs_reference_frame('cd121a16-0603-460c-96e5-b4dfeed423bd', functional_domain_theory).
narrative_ontology:cs_drift_state('cd121a16-0603-460c-96e5-b4dfeed423bd', contemporary_academic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cd121a16-0603-460c-96e5-b4dfeed423bd', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, shrine_system).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, temple_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, lay_population).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__domain_partition_reading, lay_population).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, functional_domain_theory_of_syncretism).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, institutional_autonomy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers Shinto ritual and maintains the propriety boundary: kami dwell in this-world domains (mountains, rivers, harvests, warfare, the imperial house), and shrine practice addresses present welfare and protection. They teach and defend the reading that kami are not buddhas and need no soteriological frame. They depend on state recognition and tax-exemption status to operate.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, shrine_priesthood, agenda_setter,
    organized, generational, constrained, national).

% Administers Buddhist ritual and maintains the complementary boundary: buddhas and bodhisattvas offer soteriological instruction and afterlife knowledge, accessed through temple practice, scripture study, and monastic discipline. They teach that kami are celestial beings trapped in samsara who benefit from Buddhist teaching, not beings governed by a separate domain. They depend on state recognition and patronage.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, temple_priesthood, agenda_setter,
    organized, generational, constrained, national).

% Maintains separate administrative tracks for shrine and temple systems, collects taxes from both, and uses the domain-partition reading to avoid choosing between them. The reading allows state ritual to invoke kami for imperial legitimacy and governance (Shinto register) while allowing subjects to pursue Buddhist salvation (personal register). The separation lets the state benefit from both without requiring ontological reconciliation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, state_administrative_apparatus, beneficiary,
    institutional, civilizational, arbitrage, national).

% Participate in both shrine (life-cycle rituals, seasonal protection, imperial rites) and temple (funerary rites, afterlife welfare, scriptural teaching) without requiring the two to cohere. They pay through donations, taxes, and labor obligations to maintain both systems. The domain-partition reading lets them hold both practices without cognitive dissonance and without needing to choose one tradition over another.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, lay_population, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__domain_partition_reading, lay_population, payer).

% Would argue that kami and buddhas are ontologically unified (honji suijaku doctrine: buddhas manifest as kami), that the partition is artificial institutional division, and that true integration requires recognizing the metaphysical fusion. This reading is marginalized in official discourse; priests of both traditions teach the domain-partition reading in state-approved curricula and discourage integration theology in training.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, syncretic_ideological_movement, excluded,
    moderate, biographical, trapped, national).

% Document that both syncretism (honji suijaku) and separation have historical precedent; they record when the reading shifts and under what state pressures. They occupy an analytical seat from which both the domain-partition and syncretic readings are visible as historical artifacts.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, historical_records_keepers, observer,
    organized, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__domain_partition_reading, state_administrative_apparatus).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__domain_partition_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a single state administrative apparatus to coordinate two independently organized religious systems (Shinto and Buddhism) without requiring the two to merge into a unified theological framework. Solves the problem: state needs both kami-based legitimacy for imperial governance and Buddhist institutional infrastructure for literacy/records/wealth management. The partition lets each priesthood govern their domain without interference.
% TRANSFER_FUNCTION: Moves tax revenue from lay population to both shrine and temple systems; moves authority to state apparatus to define and maintain the domain boundary; moves legitimacy to shrine priesthood for imperial and seasonal kami rituals; moves legitimacy to temple priesthood for afterlife soteriological instruction. The partition transfers administrative cost to state (maintaining two parallel systems) in exchange for political benefit (avoiding theological choice).
% ABSENT_VOICES: Syncretic ideologues (honji suijaku advocates) are systematically excluded from official priesthood training and state-approved curricula. They would argue that kami and buddhas are ontologically unified and that the partition is artificial institutional division. Their reading is suppressed in official discourse but lives in medieval texts and contemporary academic theology. Lay dissenters who hold syncretic views are not organized into a recognized seat and thus remain ambient background rather than named opposition.
% DISAPPEARANCE_RATIONALE: If the domain partition constraint disappeared overnight, the state administrative apparatus would face immediate pressure to choose an ontology: either enforce theological unity (syncretism) or explicit separation (mutual exclusion). Both priesthoods would immediately collide over domain boundaries, legitimacy, and tax allocation. The lay population would lose the institutional apparatus for simultaneous shrine-and-temple participation and would face cultural pressure to choose one tradition. Within a generation, a new constraint would emerge (either fusion or separation), but the current elegant coexistence arrangement would be gone. The partition is institutional work, not natural equilibrium.
% FOUNDING_PROBLEM: Early Heian state (8th–10th century) required coordination between indigenous kami-centered ritual system (necessary for imperial legitimacy, seasonal governance, military blessing) and Buddhist institutional infrastructure (necessary for writing system, record-keeping, wealth management, intellectual prestige). Directly merging kami and buddha theology created doctrinal incoherence. Keeping them entirely separate required maintaining two parallel bureaucratic systems. The domain partition reading solved this by assigning kami to this-world governance and buddhas to afterlife soteriological knowledge, making the two systems institutionally compatible without theological merger.
% FOUNDING_PROBLEM_CORROBORATION: State administrators and both priesthood systems attest the founding problem is STILL LIVE: they continue maintaining separate administrative tracks and curricula, suggesting the coordination problem persists. Syncretic ideologues and some medieval scholars attest the founding problem is DEAD: honji suijaku doctrine shows kami and buddhas can cohere, and the partition persists only as institutional inertia and state enforcement. Comparative religionists (studying Confucianism-Taoism-Buddhism in China, Hinduism-Islam in South Asia) note domain-partition readings are common administrative solutions adopted when states must coordinate competing religious systems, suggesting they are governance pragmatics rather than stable theological truths. Historical records show syncretic theology was more prominent in medieval periods and was later restricted through state pressure on curricula, suggesting the partition may require institutional suppression to maintain.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__domain_partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The domain_partition_reading instantiates a coordination mechanism (both priesthoods operate under one state) that is substantially extractive (suppresses alternative readings, requires active institutional boundaries, produces theater in later periods). Extractiveness is low-to-moderate (0.31 at interval end) because the constraint solves a genuine coordination problem (state cannot run two entirely separate bureaucracies) and because lay participants largely accept the reading without perceiving coercion. However, the theater ratio rises significantly over time (0.28→0.42 across the interval), indicating that boundary maintenance becomes increasingly performative rather than naturally maintained — priests must actively teach the partition in official curricula, state must actively exclude syncretic theology from recognition, suggesting the arrangement's naturalness erodes. Suppression_requirement (measured as the active institutional work required to maintain the boundary) rises over 0–600 years (0.15→0.27) then stabilizes, consistent with a constraint that becomes institutionalized and performative: initially the partition must be defended against syncretic challenges, but once institutional structures solidify, maintenance becomes routine rather than contested. Accessibility_collapse and resistance stay moderate throughout, indicating that alternative readings (syncretic, incoherent) remain conceptually available and are actively resisted by ideological movements, even if institutionally suppressed.
 *
 * PERSPECTIVAL GAP:
 *   The shrine and temple priesthoods experience the constraint differently from the state apparatus and the lay population. For priesthoods, the partition provides institutional autonomy: each operates within their assigned domain without the other's interference, and both benefit from state coordination without needing to merge. For the state, the partition solves a governance problem by avoiding the need to choose between kami and buddha authority. For the lay population, the partition provides cognitive and ritual ease: they can participate in both practices without experiencing contradiction. For syncretic ideologues, the same constraint is an extraction mechanism: they are excluded from official priesthood training, their reading is suppressed in state-approved curricula, and their attempt to integrate kami and buddhas is treated as heretical rather than legitimate theological interpretation. The engine computes these different seats' positions from the structural data (who benefits, who bears costs, what exit looks like from each position). The shrine priesthood sits near the beneficiary end (d lower): they gain autonomy and recognition from the partition. The syncretic movement sits near the target end (d higher): they are excluded and suppressed. The state sits near the orchestrator end (d middle): they benefit from avoiding theological choice but bear the cost of maintaining two administrative systems. The lay population sits near the moderate end: genuine coordination benefit (both practices available without conflict) but also indirect cost (taxed to support both systems).
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine priesthood (beneficiary): gains institutional autonomy, domain control, state funding, and teaching authority within their assigned domain. Exit is constrained but comfortable — they have structured power within the partition. d ≈ 0.25. Temple priesthood (beneficiary): parallel structure — gains institutional autonomy, domain control, state patronage, teaching authority within afterlife soteriological frame. Exit is constrained but comfortable. d ≈ 0.25. State apparatus (agenda-setter): orchestrates the boundary, benefits from avoiding theological choice, but bears costs of maintaining two systems. They have highest exit options (arbitrage: could choose kami-only or buddha-only ideology) but choose not to, suggesting genuine coordination value. d ≈ 0.40. Lay population (beneficiary with secondary payer role): collects genuine coordination benefit (both practices available) but also indirect cost (taxation, labor). Exit options are constrained by cultural embedding. d ≈ 0.55. Syncretic ideologues (excluded): face active suppression in curricula, exclusion from priesthood training, marginalization in official discourse. They perceive the constraint as extraction (suppressing their reading). d ≈ 0.75. The directed graph shows a coordination benefit for priesthoods and state, diffuse benefit for lay population, and extraction (suppression) for syncretic movements.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (early Heian state needs coordination between indigenous kami ritual and Buddhist infrastructure) shows contested status. State administrators and both priesthoods attest the problem is STILL LIVE: they continue maintaining separate administrative tracks. Syncretic ideologues attest it is DEAD: honji suijaku resolves the incoherence and the partition is institutional laziness. Comparative religionists note domain-partition readings are common administrative solutions across cultures (China, South Asia) and appear to be governance rather than theological truths. This ambiguity — whether the founding problem persists or has been displaced by institutional inertia — is the source of mandatrophy risk. If the partition's founding coordination problem is dead but the partition persists, the constraint becomes zombie-like: institutional theater masking atrophied function. The theater_ratio trajectory (rising 0.28→0.42 over the interval) is consistent with mandatrophy emergence: increasingly performative boundary maintenance rather than naturally maintained coexistence. However, the constraint has not yet fully atrophied: lay participation remains high, priesthoods have not merged, state continues to staff both systems. The constraint is at risk of mandatrophy (founding problem contested, theater rising) but not yet fully mandatrophied. The three-reading contest (domain_partition vs syncretic vs incoherent) creates classification instability: a syncretic reading would produce different mandatrophy dynamics (the fusion reading would claim the partition is the zombie constraint, not syncretism). This reading-dependence is documented in the cs_structure.axioms block.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_functional_reading_indeterminacy,
    'Is the domain partition (kami govern this-world, buddhas govern afterlife) a description of genuine metaphysical domains, or a functional administrative category created by state enforcement?',
    'Examine historical texts from pre-partition eras (Asuka-early Heian) to determine whether the partition was already implicit in ritual practice or was imposed by state administrative policy. Compare with syncretic readings from the same period to assess whether the partition required active suppression of alternative readings.',
    'If the partition is ancient and implicit in practice, it is a genuine spiritual truth (supports the domain_partition_reading''s core axiom). If the partition was state-imposed and required suppressing syncretic readings, it is revealed as an extractive institutional constraint (shifts classification toward snare, reduces efficacy of the rope frame).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontological_vs_functional_reading_indeterminacy, empirical, 'Whether the domain partition is ontologically real or administratively constructed.').

omega_variable(
    suppression_of_syncretic_reading,
    'To what extent does the domain_partition_reading require active suppression of the syncretic_fusion_reading (honji suijaku doctrine) to maintain institutional boundaries?',
    'Audit historical records for periods when syncretic theology was more prominent (e.g., medieval honji suijaku expansion) and periods when it was restricted; measure correlation between state pressure and doctrinal orthodoxy shifts; examine priesthood curricula for explicit exclusion of integration theology.',
    'High suppression indicates the partition is maintained by institutional coercion rather than natural theological agreement — reveals extractive structure and supports reclassification toward snare or tangled_rope. Low suppression indicates the partition has genuine adherents who prefer it without coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_syncretic_reading, empirical, 'How much the domain-partition reading depends on suppressing alternative readings.').

omega_variable(
    reading_contest_irreducibility,
    'Can the three sibling readings (domain_partition, syncretic_fusion, incoherent_bundle) be resolved by empirical evidence alone, or does the resolution fundamentally depend on prior theological commitments?',
    'Attempt to derive predictions from each reading that would be true under one reading and false under another; test predictions against historical evidence. If all three readings accommodate the evidence post-hoc, the contest is irreducible to empirical facts.',
    'If resolvable empirically: one reading is vindicated, the others are falsified, and the constraint can be definitively classified. If irreducible: the three readings coexist as incommensurable interpretive frames, and classification becomes reading-indexed (OQ-26 compatible — different readings produce different constraints).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_irreducibility, conceptual, 'Whether the kernel contest is empirically resolvable or fundamentally reading-dependent.').

omega_variable(
    lay_population_cognitive_coherence,
    'Do lay participants genuinely experience the domain partition as coherent, or do they hold the two traditions as separate practices that they don''t integrate?',
    'Ethnographic interview and observation of lay participants'' understanding; examine whether they are aware of potential theological contradiction and, if aware, how they resolve it. Test whether moving the boundary (e.g., saying shrine is also relevant to afterlife) produces cognitive dissonance or is transparently false to lived practice.',
    'If lay participants genuinely experience the partition as coherent theological truth, the constraint has deep legitimacy (supports rope frame). If lay participants experience the two traditions as separate ritual practices without worrying about coherence, the partition is a elite intellectual construct with limited lay uptake (supports piton or incoherent_bundle frame).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lay_population_cognitive_coherence, empirical, 'Whether lay participants experience the domain partition as genuine coherence or pragmatic separation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__domain_partition_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(shin_tr_t0, projected).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 200, 0.32).
narrative_ontology:measurement_basis(shin_tr_t200, observed).
narrative_ontology:measurement(shin_tr_t400, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 400, 0.37).
narrative_ontology:measurement_basis(shin_tr_t400, observed).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 600, 0.41).
narrative_ontology:measurement_basis(shin_tr_t600, observed).
narrative_ontology:measurement(shin_tr_t800, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 800, 0.44).
narrative_ontology:measurement_basis(shin_tr_t800, observed).
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1000, 0.43).
narrative_ontology:measurement_basis(shin_tr_t1000, observed).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1200, 0.42).
narrative_ontology:measurement_basis(shin_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(shin_be_t0, projected).
narrative_ontology:measurement(shin_be_t200, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 200, 0.22).
narrative_ontology:measurement_basis(shin_be_t200, observed).
narrative_ontology:measurement(shin_be_t400, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 400, 0.27).
narrative_ontology:measurement_basis(shin_be_t400, observed).
narrative_ontology:measurement(shin_be_t600, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 600, 0.31).
narrative_ontology:measurement_basis(shin_be_t600, observed).
narrative_ontology:measurement(shin_be_t800, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 800, 0.29).
narrative_ontology:measurement_basis(shin_be_t800, observed).
narrative_ontology:measurement(shin_be_t1000, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1000, 0.3).
narrative_ontology:measurement_basis(shin_be_t1000, observed).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1200, 0.31).
narrative_ontology:measurement_basis(shin_be_t1200, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(shin_su_t0, projected).
narrative_ontology:measurement(shin_su_t200, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 200, 0.19).
narrative_ontology:measurement_basis(shin_su_t200, observed).
narrative_ontology:measurement(shin_su_t400, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 400, 0.24).
narrative_ontology:measurement_basis(shin_su_t400, observed).
narrative_ontology:measurement(shin_su_t600, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 600, 0.27).
narrative_ontology:measurement_basis(shin_su_t600, observed).
narrative_ontology:measurement(shin_su_t800, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 800, 0.28).
narrative_ontology:measurement_basis(shin_su_t800, observed).
narrative_ontology:measurement(shin_su_t1000, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1000, 0.27).
narrative_ontology:measurement_basis(shin_su_t1000, observed).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 1200, 0.28).
narrative_ontology:measurement_basis(shin_su_t1200, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=1200
narrative_ontology:measurement(shin_grid_01, shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse(class), 0, 0.25).
narrative_ontology:measurement(shin_grid_02, shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse(class), 1200, 0.42).
narrative_ontology:measurement(shin_grid_03, shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse(individual), 0, 0.3).
narrative_ontology:measurement(shin_grid_04, shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse(individual), 1200, 0.4).
narrative_ontology:measurement(shin_grid_05, shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse(organizational), 0, 0.18).
narrative_ontology:measurement(shin_grid_06, shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse(organizational), 1200, 0.35).
narrative_ontology:measurement(shin_grid_07, shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse(structural), 0, 0.22).
narrative_ontology:measurement(shin_grid_08, shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse(structural), 1200, 0.38).
narrative_ontology:measurement(shin_grid_09, shinbutsu_ontological_substrate__domain_partition_reading, resistance(class), 0, 0.38).
narrative_ontology:measurement(shin_grid_10, shinbutsu_ontological_substrate__domain_partition_reading, resistance(class), 1200, 0.51).
narrative_ontology:measurement(shin_grid_11, shinbutsu_ontological_substrate__domain_partition_reading, resistance(individual), 0, 0.32).
narrative_ontology:measurement(shin_grid_12, shinbutsu_ontological_substrate__domain_partition_reading, resistance(individual), 1200, 0.45).
narrative_ontology:measurement(shin_grid_13, shinbutsu_ontological_substrate__domain_partition_reading, resistance(organizational), 0, 0.42).
narrative_ontology:measurement(shin_grid_14, shinbutsu_ontological_substrate__domain_partition_reading, resistance(organizational), 1200, 0.55).
narrative_ontology:measurement(shin_grid_15, shinbutsu_ontological_substrate__domain_partition_reading, resistance(structural), 0, 0.35).
narrative_ontology:measurement(shin_grid_16, shinbutsu_ontological_substrate__domain_partition_reading, resistance(structural), 1200, 0.48).
narrative_ontology:measurement(shin_grid_17, shinbutsu_ontological_substrate__domain_partition_reading, stakes_inflation(class), 0, 0.1).
narrative_ontology:measurement(shin_grid_18, shinbutsu_ontological_substrate__domain_partition_reading, stakes_inflation(class), 1200, 0.18).
narrative_ontology:measurement(shin_grid_19, shinbutsu_ontological_substrate__domain_partition_reading, stakes_inflation(individual), 0, 0.14).
narrative_ontology:measurement(shin_grid_20, shinbutsu_ontological_substrate__domain_partition_reading, stakes_inflation(individual), 1200, 0.22).
narrative_ontology:measurement(shin_grid_21, shinbutsu_ontological_substrate__domain_partition_reading, stakes_inflation(organizational), 0, 0.15).
narrative_ontology:measurement(shin_grid_22, shinbutsu_ontological_substrate__domain_partition_reading, stakes_inflation(organizational), 1200, 0.23).
narrative_ontology:measurement(shin_grid_23, shinbutsu_ontological_substrate__domain_partition_reading, stakes_inflation(structural), 0, 0.12).
narrative_ontology:measurement(shin_grid_24, shinbutsu_ontological_substrate__domain_partition_reading, stakes_inflation(structural), 1200, 0.2).
narrative_ontology:measurement(shin_grid_25, shinbutsu_ontological_substrate__domain_partition_reading, suppression(class), 0, 0.1).
narrative_ontology:measurement(shin_grid_26, shinbutsu_ontological_substrate__domain_partition_reading, suppression(class), 1200, 0.2).
narrative_ontology:measurement(shin_grid_27, shinbutsu_ontological_substrate__domain_partition_reading, suppression(individual), 0, 0.14).
narrative_ontology:measurement(shin_grid_28, shinbutsu_ontological_substrate__domain_partition_reading, suppression(individual), 1200, 0.24).
narrative_ontology:measurement(shin_grid_29, shinbutsu_ontological_substrate__domain_partition_reading, suppression(organizational), 0, 0.12).
narrative_ontology:measurement(shin_grid_30, shinbutsu_ontological_substrate__domain_partition_reading, suppression(organizational), 1200, 0.22).
narrative_ontology:measurement(shin_grid_31, shinbutsu_ontological_substrate__domain_partition_reading, suppression(structural), 0, 0.08).
narrative_ontology:measurement(shin_grid_32, shinbutsu_ontological_substrate__domain_partition_reading, suppression(structural), 1200, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__domain_partition_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__domain_partition_reading, 0.18).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, heian_state_religious_administration).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, priesthood_institutional_autonomy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel shinbutsu_ontological_substrate. The syncretic_fusion_reading and incoherent_bundle_reading are sibling constraints instantiating alternative readings of the same kernel. All three stories share the referent (the persistence of shrine and temple coexistence in Japanese institutional life) but author different ε values based on their reading's assessment of whether the coexistence is coherent (low extraction for domain_partition, moderate extraction for incoherent_bundle, ambiguous for syncretic_fusion). Network links to the kernel's siblings enable the corpus to model the contest itself: three differently-classified constraints arising from one institutional situation, differing only in how the reading answers the kernel question. The reading_relations block in cs_structure declares the structural relationships between readings: domain_partition coexists_with syncretic_fusion (both remain live positions held by different parties), and influences incoherent_bundle (if the partition is real and the fusion is real, then the bundle must be incoherent).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
