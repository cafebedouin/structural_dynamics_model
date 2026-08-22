% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Shinbutsu Ontological Substrate — Syncretic Fusion Reading
 *   domain: religious/historical/commitment_system
 *
 * SUMMARY:
 *   The syncretic_fusion_reading asserts that kami and buddhas are
 *   ontologically identical — honji suijaku ('original ground, trace
 *   manifestation') describes metaphysical truth: buddhas are the true
 *   reality (honji) and kami are their compassionate manifestations (suijaku)
 *   in the Japanese archipelago. This is not merely institutional coexistence
 *   but a single ontological substrate. The constraint operated from early
 *   Heian (c. 800 CE) through the Tokugawa period (c. 1868), structuring
 *   temple-shrine complexes (jingū-ji), unified priesthoods, and imperial
 *   ritual. The Meiji separation (shinbutsu bunri, 1868) was experienced as
 *   ontological violence by adherents of this reading, not mere institutional
 *   reorganization. The claimed_type is tangled_rope: genuine coordination
 *   (unified ritual economy, shared cosmological framework) combined with
 *   asymmetric extraction (institutional rents, forced dual practice,
 *   suppression of pure traditions).
 *
 * KEY AGENTS:
 *   - syncretic_temple_institutions: Primary beneficiary (institutional/arbitrage) — collects ritual economy rents, controls unified clergy
 *   - unified_priesthood_lineages: Primary beneficiary (organized/identity_locked) — hereditary positions in merged institutions
 *   - imperial_court_ritual_bureaucracy: Agenda setter (institutional/generational) — authorizes and regulates the fusion
 *   - pure_land_buddhist_movements: Victim (organized/constrained) — doctrinal purity requires exclusive buddha-devotion; forced into syncretic frames
 *   - shinto_purist_lineages: Victim (moderate/trapped) — kami-centric traditions suppressed as 'incomplete' without buddha ground
 *   - lay_practitioners_forced_into_dual_practice: Victim (powerless/trapped) — required to support both systems materially and ritually
 *   - modern_shinto_nationalist_reformers: Excluded (powerful/trapped) — Meiji-era actors who would reject the kernel entirely
 *   - comparative_religion_scholars: Observer (analytical/analytical) — analyzes the kernel from outside the commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.68).
domain_priors:suppression_score(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.72).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__syncretic_fusion_reading, "Shinbutsu Ontological Substrate — Syncretic Fusion Reading").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__syncretic_fusion_reading, "religious/historical/commitment_system").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'd513bb95-1b89-4da6-b71c-cf7950aca191').
narrative_ontology:cs_kernel_codification('d513bb95-1b89-4da6-b71c-cf7950aca191', distributed).
narrative_ontology:cs_authority_grounding('d513bb95-1b89-4da6-b71c-cf7950aca191', lineage).
narrative_ontology:cs_interpretation_layer_present('d513bb95-1b89-4da6-b71c-cf7950aca191').
narrative_ontology:cs_reading_relation('d513bb95-1b89-4da6-b71c-cf7950aca191', shinbutsu_ontological_substrate__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('d513bb95-1b89-4da6-b71c-cf7950aca191', shinbutsu_ontological_substrate__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('d513bb95-1b89-4da6-b71c-cf7950aca191', foundational, kami_buddha_ontological_identity).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_identity, holdable).
narrative_ontology:cs_axiom_grounding('d513bb95-1b89-4da6-b71c-cf7950aca191', kami_buddha_ontological_identity, deontological).
narrative_ontology:cs_axiom('d513bb95-1b89-4da6-b71c-cf7950aca191', foundational, honji_suijaku_as_metaphysical_realism).
narrative_ontology:cs_axiom_status(honji_suijaku_as_metaphysical_realism, holdable).
narrative_ontology:cs_axiom_grounding('d513bb95-1b89-4da6-b71c-cf7950aca191', honji_suijaku_as_metaphysical_realism, deontological).
narrative_ontology:cs_reference_frame('d513bb95-1b89-4da6-b71c-cf7950aca191', heian_honji_suijaku_formation).
narrative_ontology:cs_drift_state('d513bb95-1b89-4da6-b71c-cf7950aca191', tokugawa_terauke_maturation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d513bb95-1b89-4da6-b71c-cf7950aca191', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretic_temple_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, unified_priesthood_lineages).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, imperial_court_ritual_bureaucracy).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, pure_land_buddhist_movements).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinto_purist_lineages).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, lay_practitioners_forced_into_dual_practice).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, honji_suijaku_metaphysical_realism).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, kami_buddha_ontological_identity).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretic_ritual_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the merged temple-shrine complexes (jingū-ji) that dominate local ritual economies. Collects land revenues, parishioner contributions, and state patronage. Can shift between Buddhist and Shinto frames as politically advantageous. Exit means disestablishment — but they hold the institutional capital to negotiate any transition.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretic_temple_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Hereditary priestly families serving both kami and buddha in the same complexes. Their professional identity is fused with the syncretic system — they are 'shasō' (shrine monks) or 'bettō' (temple administrators). Exit requires abandoning hereditary vocation and communal identity; the doctrinal framework makes pure alternatives appear as self-betrayal.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, unified_priesthood_lineages, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, unified_priesthood_lineages, agenda_setter).

% Authorizes the honji suijaku framework through the Jingikan (Department of Divinities) and court ritual calendar. Gains legitimacy as the cosmic center mediating kami and buddha. Can pivot to pure Shinto (as in Meiji) when the fusion no longer serves imperial authority — the bureaucracy survives the constraint.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, imperial_court_ritual_bureaucracy, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Jōdo Shinshū and related movements teach exclusive reliance on Amida Buddha (tariki). The syncretic system forces them to register as temple parishioners (danka), support kami rites, and suppress their doctrinal distinctiveness. Their exit is constrained by doctrinal commitment (they cannot pretend the fusion is true) and by state enforcement (terauke system). They persist as underground or semi-tolerated movements.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, pure_land_buddhist_movements, payer,
    organized, generational, constrained, national).

% Yoshida and Shirakawa Shintō lineages assert kami autonomy and superiority. The honji suijaku framework subordinates kami to buddhas — their traditions are framed as 'traces' of a buddha ground. Their texts are reinterpreted, their rituals absorbed, their authority marginalized. Exit is trapped: the institutional structure (Jingikan, temple registration) leaves no recognized space for pure kami practice.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinto_purist_lineages, payer,
    moderate, biographical, trapped, regional).

% Village households registered to a temple-shrine complex (terauke). Required to fund both Buddhist funerary rites and Shinto tutelary festivals. No recognized alternative — non-registration means outlaw status. Their practice is syncretic by compulsion, not conviction. Exit is geographically and legally blocked.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, lay_practitioners_forced_into_dual_practice, payer,
    powerless, biographical, trapped, local).

% Meiji-era kokugaku scholars and imperial loyalists who reject the fusion as Buddhist corruption of native kami. They are excluded from the syncretic system's conversation — their frame denies the kernel. Their exit from the constraint is the Meiji Restoration itself (1868), which imposes shinbutsu bunri by state force. They are 'trapped' within the fusion until the rupture.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, modern_shinto_nationalist_reformers, excluded,
    powerful, biographical, trapped, national).

% Analyze the kernel from outside any commitment. See the structural isomorphism with other syncretic systems (Hindu-Buddhist, Christian-pagan). Their seat computes the constraint's type without doctrinal commitment — the engine's analytical seat.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, archipelago-wide ritual order: shared calendar, unified priesthood, common cosmological framework for court and village alike. Solves the problem of fragmentary local cults by integrating kami and buddha into one metaphysical system — one ritual economy, one sacred geography, one legitimating theology for imperial authority.
% TRANSFER_FUNCTION: Moves material resources (land, labor, parishioner contributions) and symbolic capital (doctrinal authority, ritual legitimacy) from pure traditions (exclusive Buddhist or kami movements) and lay practitioners to the syncretic temple institutions and unified priesthood. The imperial court extracts legitimacy as the cosmic center.
% ABSENT_VOICES: Pure Land movements (Jōdo Shinshū) and Shinto purist lineages (Yoshida, Shirakawa) are structurally excluded from the syncretic conversation — their doctrinal commitments make the fusion incoherent, so they are silenced or marginalized. Lay practitioners have no voice in the institutional structure. Modern Shinto nationalists are excluded by definition — their frame denies the kernel. These absent voices pair with the 'excluded' and 'payer' stakeholders.
% DISAPPEARANCE_RATIONALE: If the ontological fusion vanished overnight, the temple-shrine complexes would dissolve, the unified priesthood would lose its rationale, the imperial ritual calendar would lose its integrative theology, and pure traditions would erupt into open competition. The Japanese ritual economy would reorganize around separate Buddhist and Shinto institutions — as it did, violently, in 1868.
% FOUNDING_PROBLEM: Early Heian Japan faced a fragmentation of ritual authority: local kami cults operated independently of the continental Buddhist system imported by the court. The court needed a unified cosmic order to legitimate its rule over the archipelago. Honji suijaku provided the metaphysical glue — buddhas as universal ground, kami as local manifestations — creating one ritual system for center and periphery.
% FOUNDING_PROBLEM_CORROBORATION: The imperial court and unified priesthood attest the problem is live (the archipelago still needs ritual unity). Pure Land and Shinto purist traditions attest the problem is dead (viable pure alternatives existed; the fusion was institutional capture). Modern scholars (Kuroda Toshio, Fabio Rambelli) corroborate the shifted-function reading: the fusion solved a Heian coordination problem but became extraction machinery by the Tokugawa period. No single party's testimony is definitive — the status is genuinely contested.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__syncretic_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction (0.68) is substantial: the unified system extracts labor, land, and doctrinal conformity from movements that would otherwise be pure (Jōdo Shinshū, Yoshida Shintō). Suppression (0.72) is high because the constraint's persistence required active enforcement — hereditary priesthood controls, temple registration (terauke), and imperial sanction against doctrinal deviation. Theater ratio (0.38) is moderate: the ritual coordination function was real (shared calendar, unified祭祀), but a growing fraction of enforcement served institutional rent-protection rather than cosmological coherence. Accessibility collapse (0.75) is high: once the fusion is accepted as metaphysical truth, pure alternatives appear not just wrong but incoherent. Resistance (0.55) is moderate: pure movements persisted underground and erupted at Meiji, but were contained for centuries.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (imperial court, unified priesthood), the arrangement is a Mountain — the ontological structure of reality. From the pure_land_buddhist and shinto_purist victim seats, it is a Snare — their traditions are structurally suppressed. From the syncretic_temple_institution beneficiary seat, it is a Rope — they coordinate a genuine ritual economy. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (syncretic_temple_institutions, unified_priesthood_lineages, imperial_court_ritual_bureaucracy) collect material and symbolic rents from the fusion; their d-values are low (near beneficiary end). Victims (pure_land_buddhist_movements, shinto_purist_lineages, lay_practitioners) bear doctrinal and material costs; their d-values are high (near target end). The imperial_court_ritual_bureaucracy is both agenda_setter and beneficiary — it authors the fusion and collects its legitimacy. Lay practitioners are powerless and trapped (terauke system bound households to temple-shrine complexes). Pure Land movements are organized but constrained (doctrinal commitment prevents exit; institutional suppression prevents open organization). Shinto purist lineages are moderate but trapped — their texts and practices were absorbed or banned.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unified ritual order for the Japanese archipelago) was live through the medieval period but became contested in early modern times as pure traditions demonstrated viable alternatives. The arrangement persisted past its coordination peak because the beneficiary institutions captured the enforcement machinery — classic mandatrophy. The Meiji separation was not the constraint's natural death but a state-imposed rupture; the reading's adherents experienced it as ongoing (hence contemporary revival movements). The founding problem status is 'contested' — some argue the archipelago still requires a unified ritual substrate; others hold the problem was solved by the separation itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_institutional_construction,
    'Is the kami-buddha ontological unity a genuine metaphysical discovery (mountain-like) or an institutional construction that benefits identifiable actors (false summit)?',
    'Comparative analysis of whether the claim persists under disestablishment of the supporting institutions; cross-cultural comparison with non-institutionalized syncretisms.',
    'If institutional construction, FSM triggers reclassification to tangled_rope; if genuine metaphysical limit, mountain certification holds despite beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_construction, conceptual, 'Whether ontological unity is natural law or constructed constraint').

omega_variable(
    committer_kernel_framing,
    'This constraint is the syncretic_fusion_reading of the shinbutsu_ontological_substrate kernel. How does the sibling domain_partition_reading (separate domains) or incoherent_bundle_reading (no coherent kernel) structurally relate to this reading?',
    'Map the structural deltas: domain_partition removes ontological identity but keeps functional coexistence; incoherent_bundle denies any kernel. The relations (coexists_with, forecloses, influences) are authored in cs_structure.reading_relations.',
    'Classification of this reading depends on which sibling framings are live; foreclosure would eliminate alternatives from the same framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_framing, conceptual, 'Commitment-system framing under-determination across kernel readings').

omega_variable(
    separation_resistance_mechanism,
    'Is the high resistance to Meiji-era separation (shinbutsu bunri) evidence of deep ontological commitment (mountain-like) or institutional capture of a constructed arrangement (snare/tangled_rope)?',
    'Analyze whether resistance came from doctrinal conviction vs. institutional self-preservation; track which actors resisted and what they stood to lose.',
    'If institutional self-preservation, extraction is higher than doctrinal conviction alone would predict; if doctrinal, the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separation_resistance_mechanism, empirical, 'Mechanism of resistance to state-imposed separation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0, 900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_fusion_tr_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(shinbutsu_fusion_tr_t300, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 300, 0.22).
narrative_ontology:measurement(shinbutsu_fusion_tr_t600, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 600, 0.31).
narrative_ontology:measurement(shinbutsu_fusion_tr_t900, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 900, 0.38).

% Extraction over time
narrative_ontology:measurement(shinbutsu_fusion_be_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(shinbutsu_fusion_be_t300, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 300, 0.52).
narrative_ontology:measurement(shinbutsu_fusion_be_t600, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 600, 0.61).
narrative_ontology:measurement(shinbutsu_fusion_be_t900, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 900, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_fusion_su_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(shinbutsu_fusion_su_t300, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 300, 0.52).
narrative_ontology:measurement(shinbutsu_fusion_su_t600, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 600, 0.68).
narrative_ontology:measurement(shinbutsu_fusion_su_t900, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 900, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__syncretic_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.08).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, meiji_shinbutsu_bunri_enforcement).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, terauke_registration_system).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, jinguji_temple_shrine_complexes).

% DUAL FORMULATION NOTE:
% This reading (syncretic_fusion) and domain_partition_reading decompose the 'honji suijaku' label into ontological identity vs. functional complementarity. The incoherent_bundle_reading denies the kernel. All three are linked via network.affects_constraints. The ε values differ substantially: this reading has ε=0.68 (substantial extraction), domain_partition likely ε~0.35 (coordination with modest extraction), incoherent_bundle likely ε~0.75 (pure extraction). This is the BGS pattern: one label, multiple constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_substrate__syncretic_fusion_reading, institutional, 0.15).
constraint_indexing:directionality_override(shinbutsu_ontological_substrate__syncretic_fusion_reading, organized, 0.75).
constraint_indexing:directionality_override(shinbutsu_ontological_substrate__syncretic_fusion_reading, moderate, 0.8).
constraint_indexing:directionality_override(shinbutsu_ontological_substrate__syncretic_fusion_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
