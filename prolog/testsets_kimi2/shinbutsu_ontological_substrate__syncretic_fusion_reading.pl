% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Honji Suijaku Ontological Fusion (Syncretic Reading)
 *   domain: religious_studies/japanese_history
 *
 * SUMMARY:
 *   The honji suijaku (original nature, trace manifestation) framework
 *   asserted that Japanese kami are local manifestations of universal
 *   Buddhist buddhas, producing an ontologically unified cosmos. This
 *   constraint story instantiates the syncretic_fusion_reading of the
 *   shinbutsu_ontological_substrate kernel, which treats this unity as
 *   metaphysical truth rather than institutional convenience. Historically,
 *   the doctrine allowed Buddhist temple complexes to absorb shrine networks
 *   into hierarchical ritual economies, extracting subordination and economic
 *   tribute while providing a coordinated cosmological order for courtly and
 *   lay patronage. The story is authored as a tangled_rope: genuine
 *   coordination (inter-cult integration, shared ritual vocabulary) is
 *   inseparable from asymmetric extraction (Buddhist institutional
 *   dominance). Key agents include the Buddhist temple networks that
 *   administered the doctrine, the shrine priesthoods that were structurally
 *   subordinated, the court aristocracy that gained political-religious
 *   stability, and later Shinto independence advocates excluded from orthodox
 *   discourse. This is one reading of a three-way contested kernel; the
 *   sibling readings (domain_partition_reading, incoherent_bundle_reading)
 *   are modeled as separate constraints.
 *
 * KEY AGENTS:
 *   - Buddhist temple complexes (agenda_setter/institutional/identity_locked) â administer doctrine, collect extraction
 *   - Subordinated shrine priesthood (payer/organized/identity_locked) â bear ritual and economic subordination
 *   - Court aristocracy (beneficiary/powerful/mobile) â receive legitimation and reduced pluralistic friction
 *   - Syncretic lay communities (beneficiary/moderate/constrained) â receive integrated ritual access
 *   - Shinto independence advocates (excluded/moderate/constrained) â excluded from orthodox discourse
 *   - Modern academic observers (observer/analytical/analytical) â analyze from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.62).
domain_priors:suppression_score(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.58).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__syncretic_fusion_reading, "Honji Suijaku Ontological Fusion (Syncretic Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__syncretic_fusion_reading, "religious_studies/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'd0a215da-49dc-4479-90de-577356e7ebf2').
narrative_ontology:cs_kernel_codification('d0a215da-49dc-4479-90de-577356e7ebf2', fixed_text).
narrative_ontology:cs_authority_grounding('d0a215da-49dc-4479-90de-577356e7ebf2', lineage).
narrative_ontology:cs_interpretation_layer_present('d0a215da-49dc-4479-90de-577356e7ebf2').
narrative_ontology:cs_reading_relation('d0a215da-49dc-4479-90de-577356e7ebf2', shinbutsu_ontological_substrate__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('d0a215da-49dc-4479-90de-577356e7ebf2', shinbutsu_ontological_substrate__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('d0a215da-49dc-4479-90de-577356e7ebf2', foundational, honji_suijaku_ontological_unity).
narrative_ontology:cs_axiom_status(honji_suijaku_ontological_unity, holdable).
narrative_ontology:cs_axiom_grounding('d0a215da-49dc-4479-90de-577356e7ebf2', honji_suijaku_ontological_unity, theological).
narrative_ontology:cs_axiom('d0a215da-49dc-4479-90de-577356e7ebf2', secondary, buddhist_ritual_supremacy_over_kami).
narrative_ontology:cs_axiom_status(buddhist_ritual_supremacy_over_kami, holdable).
narrative_ontology:cs_axiom_grounding('d0a215da-49dc-4479-90de-577356e7ebf2', buddhist_ritual_supremacy_over_kami, theological).
narrative_ontology:cs_reference_frame('d0a215da-49dc-4479-90de-577356e7ebf2', heian_cosmological_unity).
narrative_ontology:cs_drift_state('d0a215da-49dc-4479-90de-577356e7ebf2', meiji_restoration_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('d0a215da-49dc-4479-90de-577356e7ebf2', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, buddhist_temple_complexes).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, court_aristocracy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretic_lay_communities).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, subordinated_shrine_priesthood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the honji suijaku doctrinal framework, interpreting kami as manifestations of buddhas and integrating shrine rituals into Buddhist liturgical calendars. Collects ritual fees, land patronage, and subordinate labor from shrine communities. Exit would require relinquishing centuries of accumulated authority over the shrines and reconstituting their legitimacy on purely Buddhist grounds without the kami-buddha fusion.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, buddhist_temple_complexes, agenda_setter,
    institutional, generational, identity_locked, national).

% Performs kami rites under Buddhist institutional supervision, often with Buddhist priests installed as abbots over shrine estates. Their priestly identity is fused with the syncretic framework; asserting shrine autonomy requires rejecting the theological vocabulary in which their own cult has been expressed for centuries. Bears the cost of lost ritual independence and redirected patronage.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, subordinated_shrine_priesthood, payer,
    organized, generational, identity_locked, national).

% Patronizes integrated temple-shrine complexes for court rituals, rain-making, and ancestral protection. Benefits from a single hierarchical religious field that reduces the number of competing claims on aristocratic largesse and political allegiance. Can shift patronage among institutions but does not directly administer the doctrine.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, court_aristocracy, beneficiary,
    powerful, generational, mobile, national).

% Accesses a unified ritual economy where Buddhist and kami rites are available through a single institutional network, reducing search and coordination costs. Their local religious identity is shaped by the fused cosmology; exiting would mean finding or creating purely sectarian communities that are socially and geographically sparse.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretic_lay_communities, beneficiary,
    moderate, biographical, constrained, local).

% Advocate for an autonomous Shinto theology independent of Buddhist framing, arguing that kami are not manifestations of foreign buddhas. Excluded from orthodox doctrinal councils and aristocratic patronage networks; their voices appear mainly in marginal texts or later popular movements rather than in the mainstream religious establishment.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinto_independence_advocates, excluded,
    moderate, generational, constrained, national).

% Study the historical shinbutsu complex from outside its ritual economy, analyzing land records, doctrinal texts, and political archives to assess whether the fusion was theological conviction or institutional strategy. Neither collect from nor pay into the constraint.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, modern_academic_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__syncretic_fusion_reading, buddhist_temple_complexes).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates Buddhist and Shinto cosmologies into a single hierarchical framework, reducing inter-cult competition and enabling joint ritual patronage across religious estates under one conceptual order.
% TRANSFER_FUNCTION: Moves ritual authority, institutional subordination, land tenure, and economic patronage from independent shrine communities to Buddhist temple networks, justified by the doctrinal claim that kami are local manifestations of universal buddhas.
% ABSENT_VOICES: Shrine-priesthood advocates for ontological independence and strict sectarian Buddhist reformers who reject kami veneration as doctrinal deviation; they were excluded from orthodox councils and aristocratic curricula.
% DISAPPEARANCE_RATIONALE: If the ontological fusion claim vanished, the legitimating cosmology of the temple-shrine complex would collapse; shrine communities would reassert autonomous priestly lineages, Buddhist institutions would lose subsidiary ritual income and land control, and the aristocratic patronage system would have to renegotiate sacred legitimation across separate, potentially competing cults.
% FOUNDING_PROBLEM: Religious pluralism in early Heian Japan generated competition for patronage, land, and ritual authority among Buddhist schools, imported continental cults, and indigenous kami worship; the court needed a cosmological framework to integrate these without abolishing any.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of Japanese religion attest that the medieval institutional kami-buddha matrix persisted centuries after the original patronage competition was substantially resolved by political centralization and land-tenure systems. Temple authorities assert the problem remains live; the corroboration from outside the benefiting parties supports the view that the founding problem was at least partially superseded.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__syncretic_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) reflects the substantial transfer of ritual authority, land, and patronage from shrines to temples under the doctrinal umbrella. Suppression (0.58) captures the marginalization of shrine-autonomy movements and the difficulty of articulating independent Shinto theology within a Buddhist-dominated discursive field; it is a raw structural property, not scaled. Theater_ratio (0.40) registers that while the cosmology performed genuine integrating work, a significant share of later activity was performative maintenance of temple dominance. Accessibility_collapse (0.72) is high because the syncretic idiom became the default conceptual vocabulary, making independent shrine identity hard to articulate without rejecting the entire linguistic-religious framework. Resistance (0.48) registers persistent shrine independence movements (Yoshida, Ise, etc.) and later kokugaku. The metrics and the claimed type are authored independently: the reading presents itself as mountain-like metaphysical truth, but the structural data (beneficiaries, victims, active enforcement) support tangled_rope classification.
 *
 * PERSPECTIVAL GAP:
 *   From the Buddhist temple seat, the constraint is necessary cosmic order and pastoral coordination; from the subordinated shrine seat, it is doctrinal capture that absorbs their cult into an alien hierarchy; from the court seat, it is a pragmatic political theology. The engine will compute these divergent classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist temple complexes are the primary beneficiary/agenda_setter (low d, subsidized by the constraint); the subordinated shrine priesthood is the primary payer (high d, extraction amplified by identity-locked exit and national scope). The court aristocracy and lay communities occupy intermediate positions, receiving coordination benefits but paying diffuse costs of reduced religious pluralism. Modern scholarly observers sit at analytical scope with arbitrage-grade exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The genuine coordination functionâreducing inter-cult conflict, enabling joint ritual patronage, and stabilizing a plural religious fieldâprevents classification as pure snare. However, the asymmetric extraction (Buddhist institutional dominance, shrine subordination) and the requirement of active doctrinal enforcement prevent classification as rope or mountain. The founding problem (Heian religious pluralism) is contested in status: temples claim it remains live, while historical analysis suggests it was solved by political centralization long before the doctrinal framework atrophied. This tension is captured by founding_problem_status: contested rather than dead, avoiding premature piton classification while allowing the engine to register mandatrophy risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_theology_ambiguity,
    'Is the syncretic fusion constraint a sincere theological ontology or a retroactive institutional legitimation strategy naturalized as metaphysics?',
    'Comparative analysis of doctrinal texts against land-tenure and ritual-income records; if economic extraction tracks doctrinal enforcement tightly, the institutional reading strengthens.',
    'Would shift epsilon upward toward snare if purely instrumental, or toward rope if purely theological; current tangled_rope assumes irreducible ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_theology_ambiguity, conceptual, 'Whether the constraint is theology or institutional cover').

omega_variable(
    shrine_subordination_internalization,
    'Was shrine priesthood subordination enforced primarily by Buddhist institutional power or by internalized theological conviction that kami are properly served through Buddhist ritual?',
    'Post-Meiji exit trajectory: if shrine independence surged immediately upon state decree, suppression was structural; if it persisted only after generational turnover, it was partly internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint operated partly as cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shrine_subordination_internalization, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    state_mandate_vs_voluntary_adoption,
    'To what extent did state mandate enforce the honji suijaku framework versus voluntary adoption by religious institutions?',
    'Archival analysis of court edicts versus temple-founded shrine networks to separate coercive enforcement from organic diffusion.',
    'High state mandate would support the incoherent_bundle reading''s claim of external enforcement; low mandate supports this reading''s claim of organic doctrinal commitment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_mandate_vs_voluntary_adoption, empirical, 'State coercion versus voluntary doctrinal adoption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0, 900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(shin_tr_t150, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 150, 0.22).
narrative_ontology:measurement(shin_tr_t300, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 300, 0.3).
narrative_ontology:measurement(shin_tr_t500, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 500, 0.38).
narrative_ontology:measurement(shin_tr_t700, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 700, 0.42).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 900, 0.45).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(shin_be_t150, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 150, 0.3).
narrative_ontology:measurement(shin_be_t300, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 300, 0.45).
narrative_ontology:measurement(shin_be_t500, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 500, 0.58).
narrative_ontology:measurement(shin_be_t700, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 700, 0.62).
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 900, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(shin_su_t150, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 150, 0.45).
narrative_ontology:measurement(shin_su_t300, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 300, 0.6).
narrative_ontology:measurement(shin_su_t500, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 500, 0.65).
narrative_ontology:measurement(shin_su_t700, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 700, 0.62).
narrative_ontology:measurement(shin_su_t900, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 900, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the shinbutsu_ontological_substrate kernel, decomposed per the epsilon-invariance principle because the natural-language label 'honji suijaku' conflates ontological, functional, and skeptical readings that have distinct epsilon profiles and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
