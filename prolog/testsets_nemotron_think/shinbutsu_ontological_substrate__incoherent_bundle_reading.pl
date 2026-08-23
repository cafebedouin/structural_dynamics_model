% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: shinbutsu_ontological_substrate__incoherent_bundle_reading
 *   human_readable: Shinbutsu Syncretism as State-Enforced Incoherent Bundle
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   The shinbutsu (kami-buddha) syncretism that dominated Japanese religious
 *   life from the Heian through Edo periods is read here not as a genuine
 *   ontological fusion (syncretic_fusion_reading) nor as a stable functional
 *   partition (domain_partition_reading), but as an incoherent bundle of
 *   locally divergent practices forcibly harmonized by successive state
 *   regimes — imperial court, then bakufu — to extract legitimacy, land
 *   revenue, and population registration compliance. The 'honji suijaku'
 *   (original ground / trace manifestation) doctrine was a bureaucratic
 *   overlay on wildly heterogeneous local cults; practitioners navigated
 *   contradictory ritual demands without doctrinal resolution. The
 *   constraint's extraction is the state's harvest of ritual compliance as
 *   political loyalty; its suppression is the prohibition of public doctrinal
 *   dissent and the forced integration of shrine-temple complexes (jingū-ji).
 *   Theater ratio rises as the fused system's coordination function
 *   (integrating local cults into state registry) atrophies while
 *   performative adherence to honji suijaku orthodoxy becomes the loyalty
 *   test.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.78).
domain_priors:suppression_score(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.82).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__incoherent_bundle_reading, snare).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__incoherent_bundle_reading, "Shinbutsu Syncretism as State-Enforced Incoherent Bundle").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__incoherent_bundle_reading, "religious_studies/japanese_history/commitment_systems").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__incoherent_bundle_reading, '14daf25f-baea-47a9-a5e1-07e33cf3c59b').
narrative_ontology:cs_kernel_codification('14daf25f-baea-47a9-a5e1-07e33cf3c59b', distributed).
narrative_ontology:cs_authority_grounding('14daf25f-baea-47a9-a5e1-07e33cf3c59b', extraction).
narrative_ontology:cs_interpretation_layer_present('14daf25f-baea-47a9-a5e1-07e33cf3c59b').
narrative_ontology:cs_reading_relation('14daf25f-baea-47a9-a5e1-07e33cf3c59b', shinbutsu_ontological_substrate__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('14daf25f-baea-47a9-a5e1-07e33cf3c59b', shinbutsu_ontological_substrate__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('14daf25f-baea-47a9-a5e1-07e33cf3c59b', foundational, no_coherent_ontological_kernel).
narrative_ontology:cs_axiom_status(no_coherent_ontological_kernel, holdable).
narrative_ontology:cs_axiom_grounding('14daf25f-baea-47a9-a5e1-07e33cf3c59b', no_coherent_ontological_kernel, empirically_contingent).
narrative_ontology:cs_axiom('14daf25f-baea-47a9-a5e1-07e33cf3c59b', foundational, syncretism_as_state_enforced_drift).
narrative_ontology:cs_axiom_status(syncretism_as_state_enforced_drift, holdable).
narrative_ontology:cs_axiom_grounding('14daf25f-baea-47a9-a5e1-07e33cf3c59b', syncretism_as_state_enforced_drift, empirically_contingent).
narrative_ontology:cs_reference_frame('14daf25f-baea-47a9-a5e1-07e33cf3c59b', pre_state_local_cult_autonomy).
narrative_ontology:cs_drift_state('14daf25f-baea-47a9-a5e1-07e33cf3c59b', tokugawa_terauke_peak, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('14daf25f-baea-47a9-a5e1-07e33cf3c59b', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, imperial_bureaucracy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_ritual_officials).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, local_shrine_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, buddhist_monastic_communities).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, lay_devotees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, buddhist_monastic_communities).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_centralization_through_ritual_unification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues edicts regulating shrine-temple relations, appoints ritual officials, collects land taxes from fused complexes. Uses honji suijaku doctrine to legitimize imperial authority over both kami and buddha cults. Can shift policy (e.g., early Meiji separation) but inherits the fused system as administrative infrastructure.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, imperial_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Hold hereditary or appointed positions in the Jingikan (Dept of Kami Affairs) and later Jisha-bugyō (Shrine-Temple Magistrate). Collect stipends and status from administering the fused system. Their careers depend on the fused hierarchy; exit means losing ritual authority.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, state_ritual_officials, beneficiary,
    organized, biographical, constrained, national).

% Maintain shrine rituals while hosting Buddhist clerics in jingū-ji complexes. Forced to perform Buddhist rites for kami (honji suijaku) and surrender land revenues to temple partners. Cannot legally operate a standalone shrine; must accept Buddhist oversight. Resistance appears as cryptic preservation of 'pure' kami rites.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, local_shrine_practitioners, payer,
    organized, biographical, constrained, local).

% Gain landholdings and parishioners (danka) through fused complexes but must perform kami rites and submit to state ritual hierarchy. Powerful monasteries (Tendai, Shingon) negotiate terms; smaller temples are fully subordinated. Dual role: they extract from lay devotees via terauke while paying symbolic submission to the state's kami-centered rhetoric.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, buddhist_monastic_communities, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, buddhist_monastic_communities, beneficiary).

% Registered to a temple (terauke) for anti-Christian certification; must participate in both shrine and temple rites. Bear contradictory beliefs (kami as buddha's trace vs. buddha as kami's manifestation) without doctrinal resolution. Exit means social death (no funeral, no marriage certification, suspected Christianity). Identity locked through household registration and ancestor worship obligations.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, lay_devotees, payer,
    moderate, biographical, identity_locked, local).

% Yoshida Shinto, Ise Shinto, Hirata kokugaku — groups asserting kami autonomy from Buddhism. Their texts circulated clandestinely; leaders monitored/suppressed. They would dismantle the fused system if empowered. Excluded from official ritual hierarchy; their exclusion is the enforcement object.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, separatist_shinto_factions, excluded,
    moderate, biographical, trapped, regional).

% Analyze the constraint from outside the historical moment. Their classification (syncretism vs. fusion vs. partition) shapes contemporary understanding but does not affect the historical constraint's operation. They inherit the epistemic confusion the constraint produced.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, modern_historians_of_japanese_religion, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrated disparate local cults into a legible, state-administered ritual registry enabling population registration (terauke), land taxation, and anti-Christian policing. The fusion solved the state's problem of governing a religious landscape too fragmented for direct administration.
% TRANSFER_FUNCTION: Moves ritual legitimacy, land revenue, and population compliance from local cults and monastic communities to the imperial/bakufu center. Moves contradictory doctrinal demands (kami-as-buddha-trace, buddha-as-kami-manifestation) onto practitioners without resolution.
% ABSENT_VOICES: Separatist Shinto factions (Yoshida, Ise, Hirata) and crypto-Christian communities — both structurally excluded by the terauke system. Local cults that resisted fusion (e.g., mountain ascetic groups, folk practitioners) were absorbed or suppressed. Their objection would be: 'The fusion is a state fiction; our kami/buddha practices are distinct.'
% DISAPPEARANCE_RATIONALE: If the fused system vanished overnight (as it did in 1868 shinbutsu bunri), shrine lands were seized, Buddhist temples lost parishioners, terauke registration collapsed, and the state rebuilt ritual order around State Shinto. The world rearranged violently — the constraint was load-bearing for the Tokugawa social order.
% FOUNDING_PROBLEM: The early imperial state (7th-8th c.) faced a religious landscape of hundreds of autonomous local kami cults and competing Buddhist schools, none legible to central administration. It needed a unified ritual framework to legitimize imperial authority, register population, and tax land.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (legible ritual administration) is attested as solved by the Kamakura period in non-state sources: temple registers (terauke-chō) show functional population coverage; land surveys (kenchi) show fused complexes as tax units. The arrangement's persistence past the Kamakura period is documented in bakufu edicts maintaining fusion for anti-Christian policing — a different problem. Corroboration from outside beneficiaries: temple diaries (e.g., Kōfuku-ji records) complain of state extraction while acknowledging administrative utility; folk narratives preserve memory of pre-fusion autonomy.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness 0.78: state harvested shrine-temple land revenues, corvée labor for combined complexes, and household registration via temple certification (terauke). Suppression 0.82: public doctrinal dissent prohibited; shrine-temple separation movements (e.g., Yoshida Shinto, Hirata Atsutane) suppressed until Meiji. Theater ratio 0.55: honji suijaku theology performed as orthodoxy while local practice remained incoherent; the performance itself became the extraction mechanism (loyalty signaling). Accessibility collapse 0.72: alternative pure-shrine or pure-temple institutional forms were legally unavailable; only the fused jingū-ji form was recognized. Resistance 0.45: episodic and localized (Yoshida, Mito, Hirata schools) but never systemically threatening until Meiji restoration created exogenous regime change.
 *
 * DIRECTIONALITY LOGIC:
 *   Imperial bureaucracy and state ritual officials are structural beneficiaries (d ~ 0.15): they collect ritual legitimacy, land revenue, and population control. Local shrine practitioners, Buddhist monastic communities, and lay devotees are structural targets (d ~ 0.85): they bear contradictory ritual demands, land expropriation, and forced affiliation. The state's enforcement apparatus (Jingikan, later Jisha-bugyō) is the agenda_setter. No agent has arbitrage exit — even powerful monasteries (Enryaku-ji, Kōfuku-ji) were co-opted into the fused hierarchy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — integrating disparate local cults into a legible state ritual order — was substantially solved by the Kamakura period. The arrangement persisted 600+ years past functional necessity because the fused system became the primary instrument of household registration (terauke) and anti-Christian policing. Mandatrophy resolved: the coordination function (cult integration) atrophied; the extraction function (registration/control) persisted. The Meiji shinbutsu bunri (separation) was not a repair but a regime change that repurposed the shrine side for State Shinto — the snare's extraction logic migrated, not ended.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_coherence_status,
    'Does the shinbutsu_ontological_substrate kernel possess any coherent doctrinal core, or is it entirely a post-hoc rationalization of state-enforced institutional fusion?',
    'Comparative textual analysis of pre-state-intervention local cult records vs. later systematized honji suijaku texts; archaeological evidence of pre-integration shrine-temple complexes.',
    'If no coherent core exists, the syncretic_fusion_reading''s foundational axiom (ontological unity) is a state-manufactured legitimating fiction; the domain_partition_reading''s functional separation becomes the only empirically grounded description of pre-state practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_coherence_status, empirical, 'Whether the kernel itself has doctrinal coherence or is purely an institutional artifact').

omega_variable(
    state_extraction_mechanism,
    'What specific material and symbolic resources did the state extract through enforced shinbutsu fusion, and how did extraction intensity vary across the Heian-Kamakura-Muromachi-Edo timeline?',
    'Fiscal records of shrine-temple landholdings, corvée labor assignments, and ritual funding flows; analysis of imperial edicts and bakufu regulations on shrine-temple administration.',
    'Quantifies the snare''s extraction rate (ε) and its trajectory; if extraction accumulated while coordination function (local cult integration) decayed, the constraint evolved from scaffold/tangled_rope toward snare/piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_extraction_mechanism, empirical, 'Material and symbolic extraction channels of state-enforced syncretism').

omega_variable(
    practitioner_identity_lock,
    'To what degree were local practitioners identity-locked into the fused system versus maintaining cryptic separation of kami/buddha practice?',
    'Ethnographic recovery of folk practice from temple/shrine records, folk narratives, and material culture (dual-altar households, hidden statues); comparison with attested resistance movements (e.g., Yoshida Shinto separatism, Meiji shinbutsu bunri enthusiasts).',
    'If practitioners maintained cryptic separation (exit_options: constrained not identity_locked), the snare''s suppression operated more on public performance than internal belief — theater_ratio rises, effective extraction on internal belief falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practitioner_identity_lock, empirical, 'Depth of practitioner internalization vs. performative compliance').

omega_variable(
    committer_structure_incoherent_bundle,
    'This reading (incoherent_bundle_reading) denies kernel coherence. How does this structural denial relate to the sibling readings'' commitments?',
    'Map the logical space: if syncretic_fusion_reading asserts ontological unity and domain_partition_reading asserts functional separation, the incoherent_bundle_reading''s denial of coherence FORECLOSES syncretic_fusion (cannot hold both ''unified ontology'' and ''no coherent kernel'') but COEXISTS_WITH domain_partition (functional separation is compatible with accumulated drift).',
    'Structural relations among readings determine whether the kernel is a genuine dispute (coexists_with) or contains a logical foreclosure (forecloses). This reading forecloses syncretic_fusion; it coexists with domain_partition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_incoherent_bundle, conceptual, 'Committer-frame structural relations among the three kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__incoherent_bundle_reading, 700, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_incoherent_tr_t700, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 700, 0.25).
narrative_ontology:measurement(shinbutsu_incoherent_tr_t900, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 900, 0.35).
narrative_ontology:measurement(shinbutsu_incoherent_tr_t1100, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1100, 0.42).
narrative_ontology:measurement(shinbutsu_incoherent_tr_t1200, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1200, 0.48).
narrative_ontology:measurement(shinbutsu_incoherent_tr_t1300, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1300, 0.52).
narrative_ontology:measurement(shinbutsu_incoherent_tr_t1400, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1400, 0.54).
narrative_ontology:measurement(shinbutsu_incoherent_tr_t1500, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1500, 0.55).
narrative_ontology:measurement(shinbutsu_incoherent_tr_t1600, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1600, 0.55).
narrative_ontology:measurement(shinbutsu_incoherent_tr_t1700, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1700, 0.55).
narrative_ontology:measurement(shinbutsu_incoherent_tr_t1868, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1868, 0.55).

% Extraction over time
narrative_ontology:measurement(shinbutsu_incoherent_be_t700, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 700, 0.35).
narrative_ontology:measurement(shinbutsu_incoherent_be_t900, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 900, 0.45).
narrative_ontology:measurement(shinbutsu_incoherent_be_t1100, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1100, 0.58).
narrative_ontology:measurement(shinbutsu_incoherent_be_t1200, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1200, 0.62).
narrative_ontology:measurement(shinbutsu_incoherent_be_t1300, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1300, 0.68).
narrative_ontology:measurement(shinbutsu_incoherent_be_t1400, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1400, 0.71).
narrative_ontology:measurement(shinbutsu_incoherent_be_t1500, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1500, 0.73).
narrative_ontology:measurement(shinbutsu_incoherent_be_t1600, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1600, 0.75).
narrative_ontology:measurement(shinbutsu_incoherent_be_t1700, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1700, 0.77).
narrative_ontology:measurement(shinbutsu_incoherent_be_t1868, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1868, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_incoherent_su_t700, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 700, 0.4).
narrative_ontology:measurement(shinbutsu_incoherent_su_t900, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 900, 0.55).
narrative_ontology:measurement(shinbutsu_incoherent_su_t1100, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1100, 0.65).
narrative_ontology:measurement(shinbutsu_incoherent_su_t1200, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1200, 0.7).
narrative_ontology:measurement(shinbutsu_incoherent_su_t1300, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1300, 0.75).
narrative_ontology:measurement(shinbutsu_incoherent_su_t1400, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1400, 0.78).
narrative_ontology:measurement(shinbutsu_incoherent_su_t1500, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1500, 0.8).
narrative_ontology:measurement(shinbutsu_incoherent_su_t1600, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1600, 0.81).
narrative_ontology:measurement(shinbutsu_incoherent_su_t1700, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1700, 0.82).
narrative_ontology:measurement(shinbutsu_incoherent_su_t1868, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1868, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__incoherent_bundle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.08).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate__domain_partition_reading).

% DUAL FORMULATION NOTE:
% This reading (incoherent_bundle) and domain_partition_reading both deny ontological unity but differ on institutional coherence: domain_partition sees stable functional separation; incoherent_bundle sees state-enforced incoherence. The syncretic_fusion_reading is the ontological unity claim that both others reject. All three form a constraint family linked by kernel_id shinbutsu_ontological_substrate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_substrate__incoherent_bundle_reading, institutional, 0.15).
constraint_indexing:directionality_override(shinbutsu_ontological_substrate__incoherent_bundle_reading, organized, 0.85).
constraint_indexing:directionality_override(shinbutsu_ontological_substrate__incoherent_bundle_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
