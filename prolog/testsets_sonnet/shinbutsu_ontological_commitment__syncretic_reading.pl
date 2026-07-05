% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__syncretic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__syncretic_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__syncretic_reading
 *   human_readable: Honji-Suijaku Syncretic Ontology (Kami-as-Buddha-Manifestation Reading)
 *   domain: religious_studies/ontology/institutional_history
 *
 * SUMMARY:
 *   This story instantiates the SYNCRETIC READING of the
 *   shinbutsu_ontological_commitment kernel: the claim that kami and buddhas
 *   are genuinely aspects of one unified cosmological order, with kami as
 *   suijaku (manifest traces) of buddhas as honji (original ground). This is
 *   a doctrinally rich, institutionally load-bearing claim — it is the
 *   reading that made combined shrine-temple administration (jingu-ji)
 *   coherent for roughly a millennium, and it is the reading that Buddhist
 *   esoteric schools (Shingon, Tendai) elaborated into a systematic
 *   metaphysics (Ryobu Shinto, Sanno Shinto). Structurally it produces high
 *   institutional integration and doctrinal coherence, with the coherence
 *   purchased at the cost of subordinating independent kami-cult authority to
 *   Buddhist interpretive control. This is NOT the same constraint as the
 *   partition_reading (which claims a stable division of religious labor with
 *   no ontological fusion) or the incoherence_reading (which denies any
 *   stable commitment existed at all) — each of those is a structurally
 *   distinct claim with its own epsilon, authored as a separate story and
 *   linked here via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - buddhist_temple_hierarchy: primary agenda-setter and beneficiary (institutional/arbitrage) — administers the doctrine and the revenue it channels
 *   - shingon_tendai_doctrinal_schools: doctrinal beneficiary (institutional/arbitrage) — supplies the metaphysical apparatus that makes the fusion systematic
 *   - independent_shrine_priesthoods: primary payer (moderate/constrained) — subordinated interpretive authority over their own kami
 *   - kami_cult_local_practitioners: diffuse payer (powerless/trapped) — loses autonomous meaning of their kami but retains ritual service
 *   - meiji_era_shinbutsu_bunri_reformers: excluded, out-of-era voice whose later success is retrospective evidence against this reading's naturalness
 *   - comparative_religion_historians: analytical observer synthesizing the corpus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, 0.61).
domain_priors:suppression_score(shinbutsu_ontological_commitment__syncretic_reading, 0.58).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__syncretic_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__syncretic_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__syncretic_reading, "Honji-Suijaku Syncretic Ontology (Kami-as-Buddha-Manifestation Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__syncretic_reading, "religious_studies/ontology/institutional_history").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__syncretic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__syncretic_reading, '2a1e0ccf-7fd8-460d-a9a5-d29553fbc247').
narrative_ontology:cs_kernel_codification('2a1e0ccf-7fd8-460d-a9a5-d29553fbc247', distributed).
narrative_ontology:cs_authority_grounding('2a1e0ccf-7fd8-460d-a9a5-d29553fbc247', lineage).
narrative_ontology:cs_interpretation_layer_present('2a1e0ccf-7fd8-460d-a9a5-d29553fbc247').
narrative_ontology:cs_reading_relation('2a1e0ccf-7fd8-460d-a9a5-d29553fbc247', shinbutsu_ontological_commitment__partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('2a1e0ccf-7fd8-460d-a9a5-d29553fbc247', shinbutsu_ontological_commitment__incoherence_reading, influences).
narrative_ontology:cs_axiom('2a1e0ccf-7fd8-460d-a9a5-d29553fbc247', foundational, kami_are_ontologically_derivative_of_buddhas).
narrative_ontology:cs_axiom_status(kami_are_ontologically_derivative_of_buddhas, holdable).
narrative_ontology:cs_axiom_grounding('2a1e0ccf-7fd8-460d-a9a5-d29553fbc247', kami_are_ontologically_derivative_of_buddhas, theological).
narrative_ontology:cs_axiom('2a1e0ccf-7fd8-460d-a9a5-d29553fbc247', secondary, single_coherent_cosmology_underlies_dual_ritual_practice).
narrative_ontology:cs_axiom_status(single_coherent_cosmology_underlies_dual_ritual_practice, overridden).
narrative_ontology:cs_axiom_grounding('2a1e0ccf-7fd8-460d-a9a5-d29553fbc247', single_coherent_cosmology_underlies_dual_ritual_practice, conventional).
narrative_ontology:cs_reference_frame('2a1e0ccf-7fd8-460d-a9a5-d29553fbc247', heian_period_jingu_ji_doctrinal_settlement).
narrative_ontology:cs_drift_state('2a1e0ccf-7fd8-460d-a9a5-d29553fbc247', meiji_shinbutsu_bunri_1868, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('2a1e0ccf-7fd8-460d-a9a5-d29553fbc247', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, buddhist_temple_hierarchy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, shingon_tendai_doctrinal_schools).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, ritsuryo_court_legitimation_apparatus).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, independent_shrine_priesthoods).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, kami_cult_local_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, kami_cult_local_practitioners).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__syncretic_reading, cosmological_unity_of_kami_and_buddhas).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__syncretic_reading, honji_suijaku_doctrinal_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the jingu-ji (shrine-temple complexes) and produces the doctrinal literature establishing which buddha is the honji (original ground) of which kami-suijaku (manifest trace). Controls ordination, ritual calendars, and land revenue flowing through combined shrine-temple institutions. Determines the metaphysical hierarchy in every case it adjudicates.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, buddhist_temple_hierarchy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, buddhist_temple_hierarchy, beneficiary).

% Supplies the esoteric metaphysical apparatus (mandala correspondence, dharma-body theory) that makes the honji-suijaku identification doctrinally systematic rather than ad hoc. Gains prestige, students, and patronage by being the interpretive authority through which every local kami is legible as a buddha's local expression.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, shingon_tendai_doctrinal_schools, beneficiary,
    institutional, civilizational, arbitrage, national).

% Uses the unified cosmology to fold regional kami cults into a single imperial-Buddhist order, converting scattered local sacred authority into a legible, tax-assessable, court-sanctioned hierarchy. Benefits from a single coherent metaphysics that makes provincial religious practice administratively tractable.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, ritsuryo_court_legitimation_apparatus, beneficiary,
    institutional, generational, arbitrage, national).

% Kami cult priest lineages whose local deity is reclassified as the suijaku (trace/manifestation) of an imported buddha, subordinating their inherited ritual authority to Buddhist doctrinal oversight. Continuing to serve their shrine means operating within the honji-suijaku framework; refusing it risks loss of court patronage and combined-institution revenue.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, independent_shrine_priesthoods, payer,
    moderate, biographical, constrained, regional).

% Villagers and lay worshippers whose kami's independent standing is reinterpreted as derivative of a buddha's cosmic reality; they still receive ritual services and festival continuity, but their kami's autonomous meaning is displaced by a foreign metaphysical frame they did not choose and cannot easily contest, since literacy and doctrinal argument sit with the temple hierarchy.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, kami_cult_local_practitioners, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, kami_cult_local_practitioners, beneficiary).

% A later historical actor (not present within the honji-suijaku era itself) who would argue the syncretic ontology was always an imposed fusion masking Shinto's separate character — their eventual forced separation (shinbutsu bunri) is the strongest evidence the syncretic reading suppressed a real alternative, but they have no voice within the era this constraint governs.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, meiji_era_shinbutsu_bunri_reformers, excluded,
    organized, generational, trapped, national).

% Study the honji-suijaku textual corpus, temple-shrine administrative records, and the later Meiji rupture to assess whether the unified cosmology reflects genuine metaphysical belief, institutional convenience, or contested accommodation. Their reconstructions are the primary evidence base for evaluating this reading against its siblings.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, comparative_religion_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__syncretic_reading, buddhist_temple_hierarchy).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__syncretic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single coherent cosmological grammar that lets Buddhist ritual specialists and kami-cult priests operate within the same combined shrine-temple institutions, sharing calendars, land, and ritual labor instead of running two incompatible sacred economies side by side.
% TRANSFER_FUNCTION: Moves interpretive authority, ritual precedence, and the associated revenue and patronage from independent kami priesthoods to Buddhist doctrinal institutions, by recasting the kami as the local trace of a buddha whose 'original ground' status is defined and adjudicated by the temple hierarchy.
% ABSENT_VOICES: Independent kami lineages that predate Buddhist arrival, and later Meiji-era shinbutsu bunri reformers, would object that the unified ontology subordinates a genuinely separate tradition; the lineages are structurally inside the arrangement without a doctrinal register to contest it, and the reformers are simply outside the time period this constraint governs.
% DISAPPEARANCE_RATIONALE: If the honji-suijaku unified-order doctrine were dropped, combined shrine-temple institutions would have no metaphysical warrant for governing kami ritual through Buddhist doctrinal categories; land, revenue, and ordination authority currently routed through jingu-ji administration would need a new justificatory basis, and independent shrine priesthoods would regain an unmediated claim to their own kami's meaning — which is approximately what did happen at the Meiji shinbutsu bunri separation.
% FOUNDING_PROBLEM: Buddhism's arrival in Japan needed to explain its relationship to already-entrenched, locally powerful kami cults without either denying kami reality (politically untenable) or admitting Buddhism was merely one deity-system among many (doctrinally corrosive to Buddhist universalist claims) — honji-suijaku solved this by making kami into buddhas' local emanations.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist temple chronicles and doctrinal treatises (Ryobu Shinto, Sanno Shinto texts) attest the founding problem and assert it remains theologically live. Independent corroboration from outside the beneficiary set is thinner: Meiji-era National Learning (kokugaku) scholars and modern comparative religion historians attest instead that the 'problem' was substantially a court-and-temple administrative convenience, and that shrine lineages retained a submerged sense of kami autonomy throughout — evidenced by how readily and completely the separation succeeded once state power backed it in 1868. No source entirely outside institutional stakeholders (Buddhist or nativist) is available; the corroboration is contested between two interested successor traditions rather than settled by a neutral third party.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__syncretic_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__syncretic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__syncretic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__syncretic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__syncretic_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) reflects a genuine but asymmetric coordination structure: the unified cosmology does solve a real problem (how do two sacred systems share ritual space and institutional resources) but the solution systematically routes interpretive authority and revenue toward the Buddhist hierarchy that authored the doctrine. Suppression (0.58) is substantial but not total — kami cults retained ritual continuity and local prestige even while ceding metaphysical primacy, which is why suppression sits below the extraction ceiling rather than at it. Theater ratio starts low (0.12) because the early doctrine performed real synthetic/administrative work, then rises across the interval (to 0.32) as the doctrinal architecture increasingly persisted through inertia and institutional habit rather than active theological innovation — later honji-suijaku elaboration reads more as maintenance of an existing settlement than fresh cosmological reasoning. Accessibility collapse (0.62) is moderate-high: once the doctrine was institutionally entrenched via jingu-ji administration, alternative framings became difficult to access for anyone operating inside the temple-shrine system, though never fully collapsed (the framework's own later reversal at Meiji is proof exit was structurally possible, just costly).
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist temple hierarchy and the esoteric doctrinal schools sit at the beneficiary end: they author the honji-suijaku correspondences, administer the combined institutions, and collect the associated patronage and land revenue — d near the beneficiary pole. The court legitimation apparatus is a secondary beneficiary: it gains an administratively tractable unified religious order across disparate regional kami cults. Independent shrine priesthoods are payers: their kami's autonomous standing is reclassified as derivative, and their own interpretive authority is subordinated to Buddhist doctrinal oversight, though their exit is only constrained (not trapped) since some priesthoods successfully resisted full absorption. Local practitioners are the most powerless payers, with trapped exit options, since they lack the doctrinal literacy to contest the reframing and depend entirely on whichever institution serves their local shrine.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling an incoming universalist tradition with entrenched local cults — was genuinely live in the early centuries of Buddhist transmission to Japan. By the medieval period the problem had substantially been solved (coexistence was achieved and stable), yet the doctrinal apparatus and its institutional privileges persisted at full strength for centuries afterward, which is exactly the founding_problem_status: contested signal this schema is built to surface — the temple hierarchy's own chronicles assert continued theological necessity while outside observers (and the eventual Meiji rupture) suggest the arrangement had become self-perpetuating administrative convenience well before it was dismantled. This does not make the reading incoherent or theatrical throughout its history — the theater_ratio trajectory shows genuine early function degrading gradually into inertial maintenance — but it does mean the syncretic reading's tangled_rope classification captures both halves honestly: real coordination function AND asymmetric extraction that required active enforcement (temple control of ordination and land) to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_belief_vs_institutional_convenience,
    'Did premodern Japanese religious actors (temple clergy, shrine priests, lay practitioners) hold the honji-suijaku unified cosmology as sincere metaphysical belief, or did the doctrine function primarily as an institutional accommodation that participants performed without deep ontological commitment?',
    'Close textual analysis of devotional and liturgical sources (as opposed to administrative/doctrinal treatises) for evidence of sincere first-person cosmological commitment versus formulaic doctrinal citation; comparison with how readily and completely the fusion dissolved at Meiji shinbutsu bunri, which is suggestive but not conclusive since forced separation does not prove the prior union was insincere.',
    'If sincere belief dominates, this reading''s claimed_type (tangled_rope) still holds but the coordination function is stronger and the extraction reading softer — closer to genuine syncretic rope with incidental institutional benefit. If institutional convenience dominates, the extraction component is closer to the whole story and the constraint edges toward snare-like capture wearing doctrinal cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_belief_vs_institutional_convenience, conceptual, 'Whether the syncretic ontology was sincerely held or institutionally performed.').

omega_variable(
    kernel_reading_selection_evidence,
    'What specific historical evidence would distinguish this syncretic reading from the partition reading and the incoherence reading, given that all three are compatible with the same surviving textual and administrative record?',
    'Systematic comparison of honji-suijaku doctrinal treatises (favoring syncretic reading), ritual calendar bifurcation evidence (favoring partition reading), and documented cases of contradictory theological statements tolerated within single institutions (favoring incoherence reading) across the same historical corpus, ideally stratified by period and region rather than treated as a single national pattern.',
    'If the record more strongly supports the partition or incoherence readings, this story''s classification as the operative historical reality would need revision — the syncretic reading would then describe an elite doctrinal minority position rather than the lived ontological commitment of the broader shinbutsu-shugo system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, empirical, 'Evidentiary basis for choosing the syncretic reading over its two siblings.').

omega_variable(
    meiji_rupture_as_counterevidence_strength,
    'Does the speed and completeness of the Meiji-era shinbutsu bunri separation constitute strong evidence that the syncretic unity was never structurally deep (favoring partition or incoherence readings), or is it better explained as state-coerced dismantling of a genuinely integrated system that could not have separated so easily without political force?',
    'Comparative study of resistance/friction at the moment of separation — did shrine and temple communities at the local level treat the separation as an artificial imposition (implying genuine prior integration) or as ratifying an already-latent distinction (implying partition or incoherence was closer to lived reality)?',
    'Strong local resistance to separation supports this syncretic reading''s naturalness claim; weak resistance or rapid local adoption undermines it and strengthens the sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_rupture_as_counterevidence_strength, empirical, 'Whether Meiji separation speed indicates shallow or coerced-deep prior integration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__syncretic_reading, 0, 900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(shin_tr_t0, observed).
narrative_ontology:measurement(shin_tr_t150, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 150, 0.16).
narrative_ontology:measurement_basis(shin_tr_t150, observed).
narrative_ontology:measurement(shin_tr_t300, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 300, 0.2).
narrative_ontology:measurement_basis(shin_tr_t300, observed).
narrative_ontology:measurement(shin_tr_t450, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 450, 0.24).
narrative_ontology:measurement_basis(shin_tr_t450, observed).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 600, 0.28).
narrative_ontology:measurement_basis(shin_tr_t600, observed).
narrative_ontology:measurement(shin_tr_t750, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 750, 0.3).
narrative_ontology:measurement_basis(shin_tr_t750, observed).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 900, 0.32).
narrative_ontology:measurement_basis(shin_tr_t900, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(shin_be_t0, observed).
narrative_ontology:measurement(shin_be_t150, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 150, 0.47).
narrative_ontology:measurement_basis(shin_be_t150, observed).
narrative_ontology:measurement(shin_be_t300, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 300, 0.53).
narrative_ontology:measurement_basis(shin_be_t300, observed).
narrative_ontology:measurement(shin_be_t450, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 450, 0.58).
narrative_ontology:measurement_basis(shin_be_t450, observed).
narrative_ontology:measurement(shin_be_t600, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 600, 0.6).
narrative_ontology:measurement_basis(shin_be_t600, observed).
narrative_ontology:measurement(shin_be_t750, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 750, 0.6).
narrative_ontology:measurement_basis(shin_be_t750, observed).
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 900, 0.61).
narrative_ontology:measurement_basis(shin_be_t900, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(shin_su_t0, observed).
narrative_ontology:measurement(shin_su_t150, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 150, 0.42).
narrative_ontology:measurement_basis(shin_su_t150, observed).
narrative_ontology:measurement(shin_su_t300, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 300, 0.48).
narrative_ontology:measurement_basis(shin_su_t300, observed).
narrative_ontology:measurement(shin_su_t450, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 450, 0.52).
narrative_ontology:measurement_basis(shin_su_t450, observed).
narrative_ontology:measurement(shin_su_t600, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 600, 0.55).
narrative_ontology:measurement_basis(shin_su_t600, observed).
narrative_ontology:measurement(shin_su_t750, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 750, 0.57).
narrative_ontology:measurement_basis(shin_su_t750, observed).
narrative_ontology:measurement(shin_su_t900, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 900, 0.58).
narrative_ontology:measurement_basis(shin_su_t900, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__syncretic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_commitment__syncretic_reading, 0.1).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment__incoherence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the colloquial label 'the honji-suijaku/shinbutsu-shugo ontological question' per the epsilon-invariance principle: the syncretic reading (this story, tangled_rope, epsilon ~0.61), the partition reading (separate domains, lower institutional integration, likely rope or mild tangled_rope), and the incoherence reading (no stable commitment, likely piton or a low-extraction rope reflecting tolerated ambiguity). Each has a distinct beneficiary/victim structure and should be evaluated independently; they are linked here for contamination-propagation and family-comparison purposes, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
