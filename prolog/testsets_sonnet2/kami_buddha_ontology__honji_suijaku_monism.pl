% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__honji_suijaku_monism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__honji_suijaku_monism, []).

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
 *   constraint_id: kami_buddha_ontology__honji_suijaku_monism
 *   human_readable: Honji Suijaku Monism: Kami as Traces of the Buddhist Ground
 *   domain: religious/philosophical/institutional
 *
 * SUMMARY:
 *   This story instantiates the honji-suijaku (original ground / phenomenal
 *   trace) reading of the kami-buddha relationship kernel: kami and buddhas
 *   are ontologically identical, with kami understood as local, phenomenal
 *   manifestations of an underlying Buddhist reality (buddhas and
 *   bodhisattvas as honji, kami as suijaku). This reading requires a single
 *   ultimate reality, a hierarchical ontology in which Buddhist entities are
 *   ontologically prior, kami incapable of independent existence, and an
 *   elaborate theoretical apparatus (correspondence tables, scholastic
 *   commentary) to systematize the identity claim across thousands of local
 *   kami. The reading is generated here as a stable, self-contained
 *   constraint with its own ε — it does not average over or hedge against the
 *   sibling readings (domain_partition, incoherent_bundle), which are
 *   separate constraint files.
 *
 * KEY AGENTS:
 *   - kenmitsu_temple_networks: agenda-setting institutional beneficiary administering the correspondence apparatus
 *   - buddhist_theoretician_clergy: organized beneficiary producing the doctrinal justification
 *   - independent_kami_priesthoods and local_shrine_cult_traditions: payers whose deities are ontologically subordinated
 *   - comparative_religion_scholars: analytical observer seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, 0.58).
domain_priors:suppression_score(kami_buddha_ontology__honji_suijaku_monism, 0.62).
domain_priors:theater_ratio(kami_buddha_ontology__honji_suijaku_monism, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, extractiveness, 0.58).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__honji_suijaku_monism, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__honji_suijaku_monism, "Honji Suijaku Monism: Kami as Traces of the Buddhist Ground").
narrative_ontology:topic_domain(kami_buddha_ontology__honji_suijaku_monism, "religious/philosophical/institutional").

domain_priors:requires_active_enforcement(kami_buddha_ontology__honji_suijaku_monism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__honji_suijaku_monism, 'b5b3f66f-5ed1-43f0-84a6-f778031ec523').
narrative_ontology:cs_kernel_codification('b5b3f66f-5ed1-43f0-84a6-f778031ec523', distributed).
narrative_ontology:cs_authority_grounding('b5b3f66f-5ed1-43f0-84a6-f778031ec523', lineage).
narrative_ontology:cs_interpretation_layer_present('b5b3f66f-5ed1-43f0-84a6-f778031ec523').
narrative_ontology:cs_reading_relation('b5b3f66f-5ed1-43f0-84a6-f778031ec523', kami_buddha_ontology__domain_partition, forecloses).
narrative_ontology:cs_reading_relation('b5b3f66f-5ed1-43f0-84a6-f778031ec523', kami_buddha_ontology__incoherent_bundle, influences).
narrative_ontology:cs_axiom('b5b3f66f-5ed1-43f0-84a6-f778031ec523', foundational, single_ultimate_reality_with_buddhist_priority).
narrative_ontology:cs_axiom_status(single_ultimate_reality_with_buddhist_priority, holdable).
narrative_ontology:cs_axiom_grounding('b5b3f66f-5ed1-43f0-84a6-f778031ec523', single_ultimate_reality_with_buddhist_priority, theological).
narrative_ontology:cs_axiom('b5b3f66f-5ed1-43f0-84a6-f778031ec523', foundational, kami_lack_independent_ontological_standing).
narrative_ontology:cs_axiom_status(kami_lack_independent_ontological_standing, holdable).
narrative_ontology:cs_axiom_grounding('b5b3f66f-5ed1-43f0-84a6-f778031ec523', kami_lack_independent_ontological_standing, theological).
narrative_ontology:cs_axiom('b5b3f66f-5ed1-43f0-84a6-f778031ec523', secondary, theoretical_systematization_is_required_for_valid_correspondence).
narrative_ontology:cs_axiom_status(theoretical_systematization_is_required_for_valid_correspondence, holdable).
narrative_ontology:cs_axiom_grounding('b5b3f66f-5ed1-43f0-84a6-f778031ec523', theoretical_systematization_is_required_for_valid_correspondence, conventional).
narrative_ontology:cs_reference_frame('b5b3f66f-5ed1-43f0-84a6-f778031ec523', heian_kamakura_kenmitsu_systematization).
narrative_ontology:cs_drift_state('b5b3f66f-5ed1-43f0-84a6-f778031ec523', meiji_shinbutsu_bunri_and_after, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('b5b3f66f-5ed1-43f0-84a6-f778031ec523', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, kenmitsu_temple_networks).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, buddhist_theoretician_clergy).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, shrine_temple_multiplexes).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, independent_kami_priesthoods).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, local_shrine_cult_traditions).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, lay_devotees_of_place_specific_kami).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, lay_devotees_of_place_specific_kami).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, imperial_court_and_aristocracy).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, shrine_temple_multiplexes).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, single_ultimate_reality_doctrine).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, buddhist_ontological_priority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The great exoteric-esoteric (kenmitsu) temple-shrine complexes administer the theoretical apparatus that ranks kami as manifestations of buddhas and bodhisattvas. They control the doctrinal schools, the ordination of interpreters, and the land and ritual revenue that flow from shrine-temple multiplexes (jingūji) built on the honji-suijaku premise. They set which correspondences (which kami = which buddha) are canonical and enforce this through institutional authority over affiliated shrines.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, kenmitsu_temple_networks, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, kenmitsu_temple_networks, beneficiary).

% Tendai and Shingon scholar-monks produce the systematic correspondence tables and metaphysical justifications (honji-suijaku theory proper) that make the identity claim intellectually authoritative. Their professional standing, temple appointments, and doctrinal prestige depend on the theory's continued acceptance as the correct account of kami; their expertise is the interpretive layer that absorbs local variation into the systematized hierarchy.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, buddhist_theoretician_clergy, beneficiary,
    organized, generational, arbitrage, national).

% Jingūji institutions (shrine-temple combines) gain legitimacy, patronage, and administrative reach by adopting the monist framework, but they also become dependent on the doctrinal hierarchy for their institutional identity and must maintain the correspondences to justify their combined structure — losing the framework would dissolve the rationale for their existence as combined institutions.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, shrine_temple_multiplexes, beneficiary,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, shrine_temple_multiplexes, payer).

% Hereditary shrine priests (shake) whose kami were not previously understood as derivative of anything now find their deity's status redefined as a secondary emanation (suijaku) of a Buddhist honji. They can accept subordinate integration into the temple network's ritual economy, resist and risk marginalization from state and aristocratic patronage that increasingly flows through Buddhist-affiliated institutions, or maintain separate practice at reduced prestige and funding.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, independent_kami_priesthoods, payer,
    moderate, generational, constrained, regional).

% Village and regional kami cults with their own etiological narratives, taboos, and ritual calendars have their kami's independent standing absorbed into a hierarchy where the kami is explained as merely the visible trace of an unseen Buddhist ground. They have no institutional mechanism to contest the reclassification and no comparable theoretical apparatus of their own to assert ontological independence within the literate discourse that decides such questions.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, local_shrine_cult_traditions, payer,
    powerless, generational, trapped, local).

% Ordinary worshippers continue to pray to their local kami for concrete, place-bound concerns (harvest, childbirth, protection) largely unaffected in daily practice, but the elite theological reclassification of their kami as a lesser manifestation of a Buddhist entity shapes which rituals receive court sponsorship, which shrines are rebuilt with state funds, and how their kami is represented in official chronicles and iconography.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, lay_devotees_of_place_specific_kami, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, lay_devotees_of_place_specific_kami, beneficiary).

% The court finds honji-suijaku theory politically useful: it provides a unified cosmology that can legitimate the emperor's descent from kami while also linking the polity to the prestige and universalist claims of Buddhism, and it gives the court a single interpretive framework through which to allocate patronage across shrine-temple institutions.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, imperial_court_and_aristocracy, beneficiary,
    institutional, generational, mobile, national).

% Not present as a voice within the honji-suijaku framework's own operative period, later kokugaku and Restoration Shinto scholars would object that the theory subordinates kami to a foreign ontology and erases indigenous priority; they are absent from the medieval institutional conversation that settles the correspondence tables and only retroactively contest the settlement centuries later.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, later_shinto_revivalist_scholars, excluded,
    organized, civilizational, analytical, national).

% Historians of Japanese religion analyze honji-suijaku as one of several competing medieval accounts of the kami-buddha relationship, examining temple records, correspondence tables, and shrine-temple economic arrangements to reconstruct how the doctrine was produced, contested, and institutionally maintained.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__honji_suijaku_monism, kenmitsu_temple_networks).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__honji_suijaku_monism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, systematized cosmology that lets shrine-temple institutions, court patronage, and ritual calendars operate under one coherent framework instead of maintaining separate, potentially conflicting kami cosmologies and Buddhist soteriology side by side — this genuinely reduces doctrinal friction and enables combined shrine-temple administration.
% TRANSFER_FUNCTION: Moves interpretive authority, ritual prestige, and the patronage/land revenue that follows official recognition from independent kami priesthoods and local cult traditions to the Buddhist theoretical clergy and the temple networks that administer the correspondence tables; it also moves the terms of representation for a local kami from its own community's account to the vocabulary of Buddhist ontology.
% ABSENT_VOICES: Local shrine cult traditions and their lay devotees have no comparable literate theoretical apparatus to contest the reclassification in the terms the debate is conducted (Buddhist scholastic metaphysics); later Shinto revivalist scholars who will explicitly reject the subordination are not yet organized as a counter-voice during the doctrine's formative and dominant period.
% DISAPPEARANCE_RATIONALE: If the honji-suijaku identity claim were to disappear, shrine-temple multiplexes would lose their doctrinal rationale for combined administration, correspondence-table scholarship would become obsolete, patronage allocation would need a new organizing principle, and independent kami priesthoods would regain an uncontested claim to their deities' ontological standing — the institutional and ritual landscape would substantially reorganize.
% FOUNDING_PROBLEM: Buddhism, arriving in a land already saturated with local kami cults, needed a way to explain the kami's continued efficacy and reality without conceding that Buddhism's universalist claims were false or that the kami were simply outside its cosmology; honji-suijaku theory solved this by making kami intelligible as authentic (if provisional) manifestations of Buddhist truth rather than either demons to be suppressed or rivals to be ignored.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist theoretician clergy and temple networks (the beneficiary parties) attest the framework remains the correct and living account of the kami-buddha relationship. Independent priestly lineages and, much later, kokugaku/Restoration Shinto scholars — writing from outside the beneficiary institutions — attest that the founding accommodation was historically contingent and that the ontological subordination it encodes was never accepted by all kami traditions, only imposed unevenly through institutional leverage; comparative historians of religion corroborate that the doctrine's dominance tracked temple-network institutional power rather than universal theological consensus.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__honji_suijaku_monism, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__honji_suijaku_monism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__honji_suijaku_monism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kami_buddha_ontology__honji_suijaku_monism, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__honji_suijaku_monism, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-substantial (0.58 at interval end) rather than extreme: the doctrine performs real coordination work (a workable shared cosmology for shrine-temple administration) alongside a genuine asymmetric transfer of interpretive authority and patronage toward the Buddhist theoretical apparatus. Suppression (0.62) reflects that the doctrine's dominance depended on active institutional leverage — temple control of ordination, court patronage channels, and land revenue tied to jingūji status — not merely on its persuasive force as metaphysics. Accessibility collapse (0.6) is moderate: local devotional practice toward specific kami persisted largely unchanged even as the elite theoretical framing collapsed the possibility of asserting the kami's independent ontological standing in official/literate contexts. Resistance (0.55) reflects that priestly lineages and local cult traditions did contest or quietly resist absorption, even without a comparable theoretical vocabulary to argue back in.
 *
 * DIRECTIONALITY LOGIC:
 *   Kenmitsu temple networks and Buddhist theoretician clergy sit near the full-beneficiary end: they set the correspondence tables, control ordination into the interpretive tradition, and collect the institutional and ritual prestige that follows official doctrinal status. Independent kami priesthoods and local shrine cult traditions sit near the full-target end: their deity's ontological status is redefined by an external apparatus they cannot contest on its own terms, and their exit options are constrained (accept subordinate integration) or trapped (no comparable literate theoretical resource). Lay devotees are structurally mixed — largely unaffected in daily devotional practice (partial beneficiary of continuity) but affected in how their kami is represented and funded at the institutional level (partial payer).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling Buddhism's universalist truth-claims with the undeniable local efficacy of kami cults — was live and genuinely unresolved at the doctrine's formation. Its status is now contested: the theoretical clergy and temple networks that administer the framework attest it remains correct and functionally necessary; independent priestly lineages and, later, explicitly, kokugaku/Restoration Shinto scholarship attest that the accommodation was historically contingent and imposed through institutional leverage rather than settled by argument. Classifying this as tangled_rope rather than snare or mountain prevents two errors: treating the doctrine as pure coercive extraction with no coordination function (it did solve a real cosmological-administrative problem for combined institutions) and treating it as an inevitable natural fact about the world (it required, and continues to require, active institutional maintenance — ordination control, correspondence-table orthodoxy, patronage gatekeeping — to hold against alternative readings of the same kami traditions).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_metaphysics_vs_institutional_convenience,
    'Was honji-suijaku theory adopted by temple networks because its theologians found it independently compelling as metaphysics, or because it was institutionally convenient for combining shrine and temple administration and revenue streams under one hierarchy?',
    'Comparative study of correspondence-table variation across regions and periods: if correspondences track theological argument consistently, the metaphysical-conviction reading is supported; if correspondences track shifts in patronage and land-holding arrangements, the institutional-convenience reading is supported.',
    'If primarily institutional convenience, the coordination function is closer to a cover story for extraction and the constraint sits closer to snare; if primarily genuine theological conviction with institutional benefits as a side effect, the tangled_rope classification (real coordination plus real asymmetric extraction) is more secure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_metaphysics_vs_institutional_convenience, conceptual, 'Whether honji-suijaku''s adoption was driven by theological conviction or institutional-administrative convenience.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the honji-suijaku identity claim best modeled as a distinct ontological position genuinely competing with domain_partition and incoherent_bundle, or as one rhetorical register within a single unsystematized practice that different actors invoked situationally alongside partition-style and even contradictory claims?',
    'Textual analysis of whether individual medieval sources deploy honji-suijaku consistently as their operative ontology, or invoke it in some contexts (elite doctrinal writing, temple charters) while operating on domain-partition or unsystematized assumptions in others (local ritual calendars, purity taboos).',
    'If sources are internally consistent, honji_suijaku_monism, domain_partition, and incoherent_bundle are genuinely competing kernel readings as modeled. If the same actors mix registers situationally, the incoherent_bundle reading may better describe the actual historical practice, and honji_suijaku_monism should be understood as the elite theoretical layer''s self-description rather than the operative ontology across all institutional levels — this is exactly the framing ambiguity Rule 2 routes to omega rather than resolving inside this story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the three kernel readings are genuinely competing total ontologies or coexisting registers invoked situationally by the same historical actors.').

omega_variable(
    natural_theological_development_vs_constructed_hierarchy,
    'Is the honji-suijaku hierarchy a natural theological development that any universalist tradition encountering local cults would eventually produce, or a constructed hierarchy whose specific ranking (which kami maps to which buddha) reflects contingent institutional power at the time of systematization?',
    'Cross-cultural comparison with other Buddhist encounters with local deity cults (e.g., in Tibet, Southeast Asia) to see whether comparable identity-hierarchies emerge independent of the specific institutional configuration present in Heian/Kamakura Japan.',
    'If the hierarchy pattern recurs cross-culturally regardless of institutional configuration, it supports treating the coordination function as closer to structurally necessary; if the specific correspondences are Japan-specific and track local temple power, it supports the institutional-convenience reading and a higher extraction weighting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_theological_development_vs_constructed_hierarchy, empirical, 'Whether the specific honji-suijaku hierarchy reflects a general pattern of universalist-local encounter or contingent local institutional power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__honji_suijaku_monism, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0, 0.2).
narrative_ontology:measurement(kami_tr_t16, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 16, 0.26).
narrative_ontology:measurement(kami_tr_t33, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 33, 0.31).
narrative_ontology:measurement(kami_tr_t50, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 50, 0.35).
narrative_ontology:measurement(kami_tr_t66, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 66, 0.38).
narrative_ontology:measurement(kami_tr_t83, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 83, 0.39).
narrative_ontology:measurement(kami_tr_t100, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(kami_be_t16, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(kami_be_t33, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 33, 0.48).
narrative_ontology:measurement(kami_be_t50, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 50, 0.53).
narrative_ontology:measurement(kami_be_t66, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 66, 0.56).
narrative_ontology:measurement(kami_be_t83, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 83, 0.58).
narrative_ontology:measurement(kami_be_t100, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(kami_su_t16, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(kami_su_t33, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 33, 0.53).
narrative_ontology:measurement(kami_su_t50, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 50, 0.57).
narrative_ontology:measurement(kami_su_t66, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 66, 0.6).
narrative_ontology:measurement(kami_su_t83, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 83, 0.61).
narrative_ontology:measurement(kami_su_t100, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__honji_suijaku_monism, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__incoherent_bundle).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the kami_buddha_ontology kernel. honji_suijaku_monism (this file) claims ontological identity with Buddhist priority; domain_partition claims ontological distinctness with functional domain separation (purity/life vs. impurity/death); incoherent_bundle denies the kernel is a coherent single commitment at all, describing shinbutsu-shugo as an institutionally sustained bundle of contradictory commitments (simultaneous fusion and separation, hierarchy and reciprocity, systematization and its absence). Each reading has its own ε, beneficiary/victim structure, and classification; they are linked here rather than merged because they make structurally incompatible claims about what kind of thing the kami-buddha relationship is.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
