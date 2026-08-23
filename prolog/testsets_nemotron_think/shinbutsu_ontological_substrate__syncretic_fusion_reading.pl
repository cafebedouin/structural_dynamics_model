% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: shinbutsu_ontological_substrate__syncretic_fusion_reading
 *   human_readable: Honji Suijaku Ontological Unity as Structural Constraint
 *   domain: religious/historical/institutional
 *
 * SUMMARY:
 *   The honji suijaku (original ground, manifest trace) doctrine asserts that
 *   kami (Shinto deities) are local manifestations of universal
 *   buddhas/bodhisattvas — an ontological unity, not mere functional
 *   coexistence. This reading instantiates the constraint that this unity is
 *   METAPHYSICAL TRUTH, structuring Japanese religious life for over a
 *   millennium (c. 750–1868). The constraint operates through merged
 *   institutions (jingū-ji shrine-temple complexes), shared ritual calendars,
 *   unified pilgrimage networks, and doctrinal enforcement that made
 *   exclusivist positions (e.g., Nichiren's 'only the Lotus Sutra,' Yoshida
 *   Shinto's 'kami supremacy') structurally deviant. The Meiji shinbutsu
 *   bunri (separation edicts) violently dismantled the institutional form,
 *   but the reading holds the ontological claim as eternal — the constraint's
 *   reference frame is the honji suijaku cosmology itself.
 *
 * KEY AGENTS:
 *   - buddhist_institutions: Primary agenda_setters and beneficiaries (institutional/biographical/arbitrage/national) — control doctrinal interpretation, land, revenue
 *   - shinto_institutions: Co-agenda_setters and beneficiaries (organized/biographical/constrained/national) — gain Buddhist legitimacy and institutional infrastructure
 *   - imperial_court_aristocracy: Agenda_setter (institutional/generational/arbitrage/national) — uses syncretic cosmology for political legitimation
 *   - exclusivist_buddhist_practitioners: Payers (moderate/biographical/trapped/national) — doctrinal purity demands rejected as schismatic
 *   - anti_syncretic_shinto_reformers: Payers (moderate/biographical/constrained/national) — native purity claims suppressed as heretical
 *   - lay_population_bearing_dual_obligations: Payers/beneficiaries (powerless/immediate/trapped/local) — bear costs of dual support, gain cosmological coherence
 *   - modern_scholars: Observers (analytical/civilizational/analytical/universal) — analyze the constraint from outside its operative horizon
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.68).
domain_priors:suppression_score(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.72).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__syncretic_fusion_reading, "Honji Suijaku Ontological Unity as Structural Constraint").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__syncretic_fusion_reading, "religious/historical/institutional").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__syncretic_fusion_reading, '129f16eb-8e53-4c6f-b15b-d1616fe7f257').
narrative_ontology:cs_kernel_codification('129f16eb-8e53-4c6f-b15b-d1616fe7f257', fixed_text).
narrative_ontology:cs_authority_grounding('129f16eb-8e53-4c6f-b15b-d1616fe7f257', lineage).
narrative_ontology:cs_interpretation_layer_present('129f16eb-8e53-4c6f-b15b-d1616fe7f257').
narrative_ontology:cs_reading_relation('129f16eb-8e53-4c6f-b15b-d1616fe7f257', shinbutsu_ontological_substrate__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('129f16eb-8e53-4c6f-b15b-d1616fe7f257', shinbutsu_ontological_substrate__incoherent_bundle_reading, influences).
narrative_ontology:cs_axiom('129f16eb-8e53-4c6f-b15b-d1616fe7f257', foundational, kami_buddha_ontological_identity).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_identity, holdable).
narrative_ontology:cs_axiom_grounding('129f16eb-8e53-4c6f-b15b-d1616fe7f257', kami_buddha_ontological_identity, theological).
narrative_ontology:cs_axiom('129f16eb-8e53-4c6f-b15b-d1616fe7f257', secondary, institutional_unity_reflects_metaphysical_unity).
narrative_ontology:cs_axiom_status(institutional_unity_reflects_metaphysical_unity, holdable).
narrative_ontology:cs_axiom_grounding('129f16eb-8e53-4c6f-b15b-d1616fe7f257', institutional_unity_reflects_metaphysical_unity, theological).
narrative_ontology:cs_reference_frame('129f16eb-8e53-4c6f-b15b-d1616fe7f257', honji_suijaku_cosmology).
narrative_ontology:cs_drift_state('129f16eb-8e53-4c6f-b15b-d1616fe7f257', meiji_separation_edicts, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('129f16eb-8e53-4c6f-b15b-d1616fe7f257', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, buddhist_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinto_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, imperial_court_aristocracy).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, exclusivist_buddhist_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, anti_syncretic_shinto_reformers).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, lay_population_bearing_dual_obligations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, lay_population_bearing_dual_obligations).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, honji_suijaku_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, kami_buddha_ontological_identity).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretic_cosmology_as_truth).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Monastic complexes (especially Tendai, Shingon) control honji suijaku doctrinal interpretation, manage merged shrine-temple estates (jingū-ji), collect land revenue and pilgrim donations. They set the ritual calendar and authenticate kami-buddha correspondences. Exit means schism — historically suppressed. They can arbitrage by shifting emphasis between esoteric Buddhism and kami cults.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, buddhist_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, buddhist_institutions, beneficiary).

% Shrine priesthoods (especially Ise, Hachiman, Inari networks) gain Buddhist philosophical legitimacy, architectural patronage, and institutional infrastructure through the unity claim. They perform Buddhist rites for kami and manage shared festivals. Exit is constrained: pure Shinto positions (Yoshida, later Hirata) are marginalized as heretical. They depend on Buddhist doctrinal cover for their elevated status.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinto_institutions, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinto_institutions, beneficiary).

% The court uses honji suijaku as a cosmological charter: the emperor as descendant of Amaterasu (kami) and patron of Buddhism embodies the unity. Court ritual fuses both traditions. They arbitrate disputes between Buddhist and Shinto institutions. Exit is arbitrage-grade: they can shift patronage, and ultimately impose the Meiji separation when the constraint no longer serves state-building.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, imperial_court_aristocracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Nichiren, Pure Land exclusivists, and other single-practice advocates face institutional censorship: their texts banned, teachers exiled, temples converted. They cannot exit the honji suijaku framework without leaving the institutional Buddhist sangha entirely — which means losing monastic status, patronage, and legal protection. Their doctrinal conviction makes exit identity-locked in practice, but structurally they are trapped by institutional power.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, exclusivist_buddhist_practitioners, payer,
    moderate, biographical, trapped, national).

% Yoshida Shinto (15th c.), Hirata Atsutane (19th c.) and kokugaku scholars assert kami supremacy and reject buddha-as-honji. They are suppressed by the combined Buddhist-Shinto institutional apparatus with court backing. Their exit is constrained: they can publish privately, teach disciples, but cannot access shrine networks or state recognition. Some (Hirata) influence Meiji restorationists — exit becomes possible only when the state switches sides.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, anti_syncretic_shinto_reformers, payer,
    moderate, biographical, constrained, national).

% Villagers support both shrine and temple: labor for festivals, donations for both, funerary fees to temples, tutelary rites at shrines. They gain a coherent cosmology where this-world (kami) and afterlife (buddhas) are unified — no existential gap. But they bear the material cost of dual institutional maintenance. Exit is trapped: no alternative religious infrastructure exists; the framework structures their entire lifeworld.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, lay_population_bearing_dual_obligations, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, lay_population_bearing_dual_obligations, beneficiary).

% Historians of religion, Japanese studies scholars, and philosophers of religion analyze honji suijaku from outside its operative horizon. They do not bear its costs or collect its benefits. Their classifications (syncretism, combinatory religion, non-duality) are analytical constructs. They provide the corroboration for the founding problem and its status.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, modern_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__syncretic_fusion_reading, buddhist_institutions).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__syncretic_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of religious pluralism in a polytheistic culture encountering universalist Buddhism: provides a single cosmological framework where kami (this-world powers) and buddhas (liberation) are ontologically continuous, enabling unified ritual governance, shared pilgrimage economies, and social cohesion across the archipelago.
% TRANSFER_FUNCTION: Moves land revenue, labor, tax exemption, and interpretive authority from the lay population and exclusivist dissenters to the merged Buddhist-Shinto institutional complex (jingū-ji networks), with the imperial court as ultimate legitimator. The commission is cosmological coherence; the cost is dual institutional support and doctrinal conformity.
% ABSENT_VOICES: Exclusivist Buddhist practitioners (Nichiren, Pure Land hardliners) and anti-syncretic Shinto reformers (Yoshida, kokugaku) are structurally excluded — their positions are defined as heresy/schism. They would object that the unity claim falsifies their soteriological commitments and that the merged institutions extract resources under false pretenses. They are absent because the constraint's enforcement machinery (institutional censorship, state backing) keeps them out.
% DISAPPEARANCE_RATIONALE: If honji suijaku vanished overnight (as it effectively did in 1868), the merged institutions fracture: temples lose shrine estates, shrines lose Buddhist ritual infrastructure, pilgrimage networks reconfigure, funerary Buddhism separates from tutelary kami worship. The Meiji separation demonstrates this: the world rearranged violently — haibutsu kishaku (destroy Buddhism), shrine reconstruction, new State Shinto. The ontological claim persists in folk practice, but the institutional constraint's disappearance rearranged the religious field entirely.
% FOUNDING_PROBLEM: How to incorporate Buddhism (universalist, soteriological, foreign) into a Japanese cosmology centered on kami (particularist, this-worldly, ancestral) without either rejecting Buddhism or subordinating kami to foreign categories?
% FOUNDING_PROBLEM_CORROBORATION: Buddhist institutions attest the problem is live (ongoing need for unified cosmology). Meiji state architects attest it is dead (separation solved it by fiat). Modern scholars (e.g., Kuroda Toshio, Fabio Rambelli) attest it is contested: the 'problem' was never a single problem but a shifting field of political-theological negotiation; the unity claim solved some problems (legitimation) while creating others (extraction). No single corroborating source outside the beneficiary set affirms the founding problem as unitary and live.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__syncretic_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
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
 *   Extractiveness (0.68) is substantial: the merged institutions extracted land, labor, tax revenue, and doctrinal monopoly through the unity claim. Suppression (0.72) is high: exclusivist movements faced institutional censorship, exile, and state violence. Theater ratio (0.42) is moderate: genuine devotional coordination and shared cosmological meaning coexisted with institutional performance. Accessibility collapse (0.78) is high: within the honji suijaku framework, conceiving kami and buddhas as truly separate was conceptually difficult — the framework structured perception itself. Resistance (0.45) is moderate: periodic resistance existed but was contained until the Meiji state's exogenous shock. The constraint is a TANGLED ROPE: genuine coordination (shared cosmology, ritual economy, social cohesion) AND asymmetric extraction (institutional resource capture, doctrinal monopoly). Active enforcement required (requires_active_enforcement: true).
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist-Shinto institutional seats, the constraint is genuine coordination — a shared cosmology that solves the problem of religious pluralism and enables unified ritual governance. From the exclusivist practitioner seats, the same structure operates as enforced extraction — their doctrinal commitments are ruled out by fiat. From the lay seat, the constraint provides cosmological coherence at the cost of dual obligation. The engine computes this seat divergence from the structural data; the claimed_type (tangled_rope) reflects the authoring seat's assessment that both coordination and extraction are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist and Shinto institutions are structural beneficiaries (d ~ 0.15-0.25): they collect resources, legitimacy, and interpretive authority. The imperial court is a beneficiary-agenda_setter (d ~ 0.10). Exclusivist practitioners and anti-syncretic reformers are targets (d ~ 0.85-0.95): they bear the cost of suppression, constrained exit (trapped/constrained). Lay population sits near symmetric (d ~ 0.5): genuine cosmological benefit, diffuse material cost. Modern scholars are analytical observers (d = 0.5). The directionality derivation from beneficiary/victim declarations + exit options captures this gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (religious pluralism in a polytheistic/Buddhist context) was live for centuries. By the late Edo period, the founding problem was contested: the unity claim had become institutional inertia for some, genuine conviction for others. The Meiji separation did not resolve the mandatrophy — it destroyed the institutional form while the ontological claim persists in folk practice and certain lineages. The constraint's mandatrophy is unresolved: the coordination function (shared cosmology) and extraction function (institutional capture) remain entangled in historical memory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel the honji suijaku doctrine itself, the broader shinbutsu relational field, or the merged institutional complex (jingū-ji, shrine-temple multiplexes)?',
    'Comparative analysis of which element siblings treat as the stabilized commitment: domain_partition_reading centers domain assignment rules; incoherent_bundle_reading centers the institutional accretion; this reading centers the doctrinal unity claim.',
    'If the kernel is the doctrine, this reading''s forecloses relation to domain_partition_reading holds. If the kernel is the institutional complex, the readings may coexist as descriptions of different strata.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the kernel''s boundaries are doctrinal, institutional, or relational determines the structural relations among sibling readings.').

omega_variable(
    metaphysical_conviction_vs_institutional_cover,
    'Does the ontological unity claim reflect genuine metaphysical conviction across the institutional field, or does it function as a cover story for resource consolidation and political legitimation?',
    'Diachronic analysis of doctrinal elaboration vs. land/tax/revenue records; comparison of elite vs. popular discourse; examination of moments when unity was asserted against institutional interest.',
    'If cover story, the constraint reclassifies toward snare (extraction masked as coordination). If genuine conviction, the coordination function is authentic and the extraction is the cost of maintaining a shared cosmology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metaphysical_conviction_vs_institutional_cover, preference, 'Whether the ontological claim is a sincere metaphysical commitment or an ideological superstructure for institutional extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of exclusivist positions (e.g., Nichiren, Yoshida Shinto, Hirata Atsutane) structural (state/institutional enforcement) or internalized (devotional conviction making alternatives unthinkable)?',
    'Post-Meiji trajectory: if exclusivist positions re-emerge rapidly after state enforcement ends, suppression was primarily structural. If syncretic habits persist in popular practice despite institutional collapse, internalized component is significant.',
    'If internalized, effective suppression exceeds the structural measure — the constraint''s grip persists after its enforcement machinery is removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the honji suijaku framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0, 1118).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_syncretic_tr_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t280, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 280, 0.28).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t560, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 560, 0.35).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t840, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 840, 0.38).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t1118, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1118, 0.42).

% Extraction over time
narrative_ontology:measurement(shinbutsu_syncretic_be_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(shinbutsu_syncretic_be_t280, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 280, 0.48).
narrative_ontology:measurement(shinbutsu_syncretic_be_t560, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 560, 0.58).
narrative_ontology:measurement(shinbutsu_syncretic_be_t840, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 840, 0.65).
narrative_ontology:measurement(shinbutsu_syncretic_be_t1118, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1118, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_syncretic_su_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(shinbutsu_syncretic_su_t280, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 280, 0.55).
narrative_ontology:measurement(shinbutsu_syncretic_su_t560, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 560, 0.62).
narrative_ontology:measurement(shinbutsu_syncretic_su_t840, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 840, 0.68).
narrative_ontology:measurement(shinbutsu_syncretic_su_t1118, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1118, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=1118
narrative_ontology:measurement(shinbutsu_syncretic_grid_01, shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse(class), 0, 0.6).
narrative_ontology:measurement(shinbutsu_syncretic_grid_02, shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse(class), 1118, 0.75).
narrative_ontology:measurement(shinbutsu_syncretic_grid_03, shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse(individual), 0, 0.55).
narrative_ontology:measurement(shinbutsu_syncretic_grid_04, shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse(individual), 1118, 0.82).
narrative_ontology:measurement(shinbutsu_syncretic_grid_05, shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse(organizational), 0, 0.65).
narrative_ontology:measurement(shinbutsu_syncretic_grid_06, shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse(organizational), 1118, 0.88).
narrative_ontology:measurement(shinbutsu_syncretic_grid_07, shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse(structural), 0, 0.7).
narrative_ontology:measurement(shinbutsu_syncretic_grid_08, shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse(structural), 1118, 0.9).
narrative_ontology:measurement(shinbutsu_syncretic_grid_09, shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance(class), 0, 0.3).
narrative_ontology:measurement(shinbutsu_syncretic_grid_10, shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance(class), 1118, 0.5).
narrative_ontology:measurement(shinbutsu_syncretic_grid_11, shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance(individual), 0, 0.25).
narrative_ontology:measurement(shinbutsu_syncretic_grid_12, shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance(individual), 1118, 0.4).
narrative_ontology:measurement(shinbutsu_syncretic_grid_13, shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance(organizational), 0, 0.35).
narrative_ontology:measurement(shinbutsu_syncretic_grid_14, shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance(organizational), 1118, 0.65).
narrative_ontology:measurement(shinbutsu_syncretic_grid_15, shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance(structural), 0, 0.2).
narrative_ontology:measurement(shinbutsu_syncretic_grid_16, shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance(structural), 1118, 0.35).
narrative_ontology:measurement(shinbutsu_syncretic_grid_17, shinbutsu_ontological_substrate__syncretic_fusion_reading, stakes_inflation(class), 0, 0.35).
narrative_ontology:measurement(shinbutsu_syncretic_grid_18, shinbutsu_ontological_substrate__syncretic_fusion_reading, stakes_inflation(class), 1118, 0.55).
narrative_ontology:measurement(shinbutsu_syncretic_grid_19, shinbutsu_ontological_substrate__syncretic_fusion_reading, stakes_inflation(individual), 0, 0.3).
narrative_ontology:measurement(shinbutsu_syncretic_grid_20, shinbutsu_ontological_substrate__syncretic_fusion_reading, stakes_inflation(individual), 1118, 0.65).
narrative_ontology:measurement(shinbutsu_syncretic_grid_21, shinbutsu_ontological_substrate__syncretic_fusion_reading, stakes_inflation(organizational), 0, 0.4).
narrative_ontology:measurement(shinbutsu_syncretic_grid_22, shinbutsu_ontological_substrate__syncretic_fusion_reading, stakes_inflation(organizational), 1118, 0.78).
narrative_ontology:measurement(shinbutsu_syncretic_grid_23, shinbutsu_ontological_substrate__syncretic_fusion_reading, stakes_inflation(structural), 0, 0.5).
narrative_ontology:measurement(shinbutsu_syncretic_grid_24, shinbutsu_ontological_substrate__syncretic_fusion_reading, stakes_inflation(structural), 1118, 0.85).
narrative_ontology:measurement(shinbutsu_syncretic_grid_25, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression(class), 0, 0.45).
narrative_ontology:measurement(shinbutsu_syncretic_grid_26, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression(class), 1118, 0.7).
narrative_ontology:measurement(shinbutsu_syncretic_grid_27, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression(individual), 0, 0.35).
narrative_ontology:measurement(shinbutsu_syncretic_grid_28, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression(individual), 1118, 0.55).
narrative_ontology:measurement(shinbutsu_syncretic_grid_29, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression(organizational), 0, 0.55).
narrative_ontology:measurement(shinbutsu_syncretic_grid_30, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression(organizational), 1118, 0.82).
narrative_ontology:measurement(shinbutsu_syncretic_grid_31, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression(structural), 0, 0.6).
narrative_ontology:measurement(shinbutsu_syncretic_grid_32, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression(structural), 1118, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__syncretic_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.08).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, meiji_shinbutsu_bunri_separation).

% DUAL FORMULATION NOTE:
% This reading and domain_partition_reading decompose the 'honji suijaku' label into ontological unity vs. functional domain partition. The incoherent_bundle_reading treats the kernel itself as a retrospective imposition. All three share the kernel_id shinbutsu_ontological_substrate. The ε values differ sharply: this reading (0.68, tangled_rope), domain_partition (lower ε, rope/scaffold), incoherent_bundle (high ε, snare/piton).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_substrate__syncretic_fusion_reading, institutional, 0.15).
constraint_indexing:directionality_override(shinbutsu_ontological_substrate__syncretic_fusion_reading, organized, 0.25).
constraint_indexing:directionality_override(shinbutsu_ontological_substrate__syncretic_fusion_reading, moderate, 0.85).
constraint_indexing:directionality_override(shinbutsu_ontological_substrate__syncretic_fusion_reading, powerless, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
