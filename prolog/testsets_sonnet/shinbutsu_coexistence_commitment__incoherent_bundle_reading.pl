% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__incoherent_bundle_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: shinbutsu_coexistence_commitment__incoherent_bundle_reading
 *   human_readable: Shinbutsu-shugo as Incoherent Bundle Sustained by Deliberate Ambiguity
 *   domain: religious_studies/philosophy_of_religion/japanese_history
 *
 * SUMMARY:
 *   This story instantiates the incoherent_bundle_reading of the
 *   shinbutsu_coexistence_commitment kernel: shinbutsu-shugo (the pre-Meiji
 *   coexistence of kami worship and Buddhist practice in Japan) is read here
 *   not as a unified syncretic theology (honji suijaku fusion) and not as a
 *   stable domain-partition between kami and Buddha jurisdictions, but as a
 *   heterogeneous accumulation of locally negotiated administrative
 *   settlements that different actors described in incompatible theological
 *   vocabularies as convenient, and that never cohered into one system
 *   because no one with authority needed it to cohere. The
 *   bettoji-administered jingu-ji complexes are the load-bearing
 *   institutional fact; the theology deployed to justify them (sometimes
 *   honji suijaku fusion language, sometimes separate-domain language, often
 *   both inconsistently at the same site) is superstructure serving
 *   administrative and fiscal ends. The Meiji shinbutsu bunri edict of 1868
 *   is read as the moment the ambiguity was forcibly resolved from outside,
 *   revealing — not creating — an incoherence that had always been present
 *   but load-bearing. This is a distinct constraint from its sibling
 *   readings: the syncretic_fusion_reading asserts genuine ontological
 *   unification (a Mountain-adjacent or Rope-adjacent claim about a real, if
 *   contested, theological achievement), and the domain_partition_reading
 *   asserts a stable, coherent division of religious labor (also a
 *   coordination-function-first claim). This reading asserts neither
 *   stability nor unification; it asserts institutionalized incoherence
 *   maintained because incoherence was administratively useful, which is why
 *   it is authored here as piton — a structure that persisted well past its
 *   founding administrative usefulness through inertia and theatrical
 *   doctrinal gesture, extracting continuously from those who needed the
 *   ambiguity resolved and never got it.
 *
 * KEY AGENTS:
 *   - bettoji_managing_priests: institutional administrators who control jingu-ji resources and benefit from unresolved ontology
 *   - shrine_temple_administrative_complexes: institutional beneficiary accumulating land and pilgrimage revenue
 *   - shogunate_religious_registration_apparatus: institutional beneficiary using the fused system for population control
 *   - lay_practitioners_seeking_doctrinal_clarity: powerless payers bearing ritual and interpretive costs
 *   - lower_ranking_kami_priests_subordinated_to_temples: moderate-power payers trapped in subordinate status
 *   - meiji_era_shinto_purists_forced_to_disentangle_the_bundle: organized actors who paid the cost of forced disentanglement
 *   - historians_of_japanese_religion: analytical observers assessing the coherence question from outside all interested parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.62).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.58).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, piton).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "Shinbutsu-shugo as Incoherent Bundle Sustained by Deliberate Ambiguity").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "religious_studies/philosophy_of_religion/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '3159b99e-1425-449e-a01c-cbc1e3dbe42e').
narrative_ontology:cs_kernel_codification('3159b99e-1425-449e-a01c-cbc1e3dbe42e', distributed).
narrative_ontology:cs_authority_grounding('3159b99e-1425-449e-a01c-cbc1e3dbe42e', practice).
narrative_ontology:cs_interpretation_layer_present('3159b99e-1425-449e-a01c-cbc1e3dbe42e').
narrative_ontology:cs_reading_relation('3159b99e-1425-449e-a01c-cbc1e3dbe42e', shinbutsu_coexistence_commitment__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('3159b99e-1425-449e-a01c-cbc1e3dbe42e', shinbutsu_coexistence_commitment__domain_partition_reading, coexists_with).
narrative_ontology:cs_axiom('3159b99e-1425-449e-a01c-cbc1e3dbe42e', foundational, ambiguity_is_load_bearing_institutional_technology).
narrative_ontology:cs_axiom_status(ambiguity_is_load_bearing_institutional_technology, holdable).
narrative_ontology:cs_axiom_grounding('3159b99e-1425-449e-a01c-cbc1e3dbe42e', ambiguity_is_load_bearing_institutional_technology, empirically_contingent).
narrative_ontology:cs_axiom('3159b99e-1425-449e-a01c-cbc1e3dbe42e', foundational, no_recoverable_unified_ontology_ever_existed).
narrative_ontology:cs_axiom_status(no_recoverable_unified_ontology_ever_existed, holdable).
narrative_ontology:cs_axiom_grounding('3159b99e-1425-449e-a01c-cbc1e3dbe42e', no_recoverable_unified_ontology_ever_existed, empirically_contingent).
narrative_ontology:cs_axiom('3159b99e-1425-449e-a01c-cbc1e3dbe42e', secondary, meiji_bunri_reveals_rather_than_creates_incoherence).
narrative_ontology:cs_axiom_status(meiji_bunri_reveals_rather_than_creates_incoherence, holdable).
narrative_ontology:cs_axiom_grounding('3159b99e-1425-449e-a01c-cbc1e3dbe42e', meiji_bunri_reveals_rather_than_creates_incoherence, empirically_contingent).
narrative_ontology:cs_reference_frame('3159b99e-1425-449e-a01c-cbc1e3dbe42e', administrative_convenience_without_settled_ontology).
narrative_ontology:cs_drift_state('3159b99e-1425-449e-a01c-cbc1e3dbe42e', meiji_shinbutsu_bunri_1868, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('3159b99e-1425-449e-a01c-cbc1e3dbe42e', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_temple_administrative_complexes).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, bettoji_managing_priests).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shogunate_religious_registration_apparatus).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, lay_practitioners_seeking_doctrinal_clarity).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, lower_ranking_kami_priests_subordinated_to_temples).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_era_shinto_purists_forced_to_disentangle_the_bundle).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__incoherent_bundle_reading, categorical_ambiguity_can_function_as_institutional_technology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buddhist clerics installed as administrators (betto) over combined shrine-temple complexes (jingu-ji), controlling ritual calendars, land revenue, and doctrinal presentation. They benefit from never resolving whether the kami is a manifestation, a guardian, or a separate deity, because each framing justifies a different claim on resources and authority, and the ambiguity lets them adjudicate case by case.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, bettoji_managing_priests, agenda_setter,
    institutional, generational, arbitrage, national).

% The combined institutions (jingu-ji, shrine-temple complexes) accumulate estates, tax exemptions, and pilgrimage income precisely because the boundary between kami worship and Buddhist practice is never fixed; a settled ontology would force a choice between competing revenue and legitimacy streams, so the institution's material interest tracks the unresolved status quo.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_temple_administrative_complexes, beneficiary,
    institutional, generational, arbitrage, national).

% The Tokugawa temple registration system (terauke) used the fused shrine-temple network to track population and suppress Christianity. It benefited from the bundle's administrative flexibility and had no interest in a clean ontology that might destabilize a working surveillance and control mechanism.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shogunate_religious_registration_apparatus, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shogunate_religious_registration_apparatus, agenda_setter).

% Villagers and pilgrims who venerate a local kami and visit the adjoining Buddhist temple without ever receiving, or being permitted to ask for, a coherent answer to what the kami actually is relative to the Buddha. They pay in confusion, in dual ritual obligations and fees, and in dependence on clerical mediation to interpret a system that resists interpretation by design.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, lay_practitioners_seeking_doctrinal_clarity, payer,
    powerless, biographical, constrained, local).

% Shinto ritualists (shanin) at combined complexes were frequently placed under Buddhist administrative authority and denied independent institutional standing. They bore the cost of an arrangement that used ambiguity to justify their subordination, unable to appeal to a clear doctrine because no stable doctrine existed to appeal to.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, lower_ranking_kami_priests_subordinated_to_temples, payer,
    moderate, generational, trapped, regional).

% Kokugaku-influenced reformers and the new Meiji state confronted centuries of unrecorded, ad hoc local settlements when they attempted the shinbutsu bunri (separation edict) of 1868. They had to litigate site by site what belonged to which category, discovering that no single coherent theology existed anywhere to separate — only accumulated administrative compromise. They paid in the enormous labor of disentanglement and in the violence (haibutsu kishaku) that followed from forcing clarity onto a system that had never possessed it.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_era_shinto_purists_forced_to_disentangle_the_bundle, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_era_shinto_purists_forced_to_disentangle_the_bundle, excluded).

% Scholars examining temple records, honji-suijaku theology, and Meiji-era separation documents to assess whether shinbutsu-shugo constituted a genuine metaphysical synthesis or a locally negotiated administrative arrangement papered over with theological vocabulary borrowed opportunistically and inconsistently across sites.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, historians_of_japanese_religion, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: At a purely local level, combining shrine and temple administration under one institution did solve a real coordination problem: it consolidated land management, ritual labor, and calendrical obligations that would otherwise have required two competing institutions to negotiate continuously. This genuine administrative economy is real but does not require, and never actually produced, a coherent shared ontology.
% TRANSFER_FUNCTION: Moves land revenue, pilgrimage income, ritual fees, and interpretive authority from lay practitioners and subordinated kami-priests to the bettoji administrators and the combined institutions, while moving the cost of eventual doctrinal reckoning onto Meiji-era reformers and communities subjected to forced separation.
% ABSENT_VOICES: Local kami cults with oral, non-textual traditions left no doctrinal record and are almost entirely absent from surviving accounts, which were written by literate Buddhist clerics; their own account of what the arrangement meant to them is largely unrecoverable. Village lay practitioners are recorded only through clerical intermediaries who had an interest in appearing coherent.
% DISAPPEARANCE_RATIONALE: The 1868 shinbutsu bunri edict is the historical natural experiment: when the ambiguity was forcibly removed, the world rearranged violently and unevenly — shrines and temples split, priesthoods were reorganized, statuary and land were seized or destroyed (haibutsu kishaku), and an entirely new institutional category (State Shinto) had to be manufactured because no prior coherent kami-only category existed to fall back on. The scale of that rearrangement is direct evidence the prior arrangement was load-bearing administrative structure, not free-floating theology.
% FOUNDING_PROBLEM: Medieval Japanese institutions needed a way to absorb an existing indigenous kami-cult landscape into an incoming, more textually and institutionally powerful Buddhist religious apparatus without provoking a legitimacy crisis for either — and needed a governance mechanism that could flexibly allocate land, labor, and ritual authority across sites with wildly different local histories.
% FOUNDING_PROBLEM_CORROBORATION: The problem of absorbing kami cults into Buddhist institutional structures without conflict was substantially resolved well before the Edo period; by the Tokugawa era the bundle persisted primarily as an administrative and fiscal convenience serving the terauke registration system and bettoji revenue interests, not as an active solution to a live theological absorption problem. This reading is corroborated by historians of religion (Kuroda Toshio's kenmitsu taisei scholarship and subsequent revisionists) working from outside both the Buddhist clerical establishment and the Meiji State Shinto apparatus, both of which had strong interests in retrospectively asserting either a coherent syncretic theology (the temples) or a coherent pure-Shinto essence (the state) — neither corroborating source benefits from the incoherent-bundle account they arrived at.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-high 0.62 (rising from 0.25 at origin to 0.62 by the eve of Meiji) because the arrangement transferred real resources (land, ritual fees, interpretive authority) from lay practitioners and subordinated priests to administering institutions over thirteen centuries, but this was gradual accumulation rather than acute predation — the bundle began as a genuinely useful administrative shortcut (low extraction) and only slowly calcified into rent-extraction dressed in inconsistent theology (rising extraction, rising theater_ratio). Theater_ratio is authored high and rising (0.2 to 0.71) because the proportion of the system's activity devoted to performing doctrinal coherence (ritual syncretism, honji-suijaku pronouncements produced on demand for particular audiences) grew relative to the shrinking proportion doing genuine coordination work, especially in the late Edo period when the terauke registration function had become the dominant institutional logic and theological consistency was needed only for external legitimacy performances. Suppression (0.58) reflects that lay practitioners and lower priests could not exit the arrangement or demand a coherent account — not through violent coercion but through the structural fact that no alternative institutional channel existed outside the fused shrine-temple system for centuries. Accessibility_collapse is authored moderate (0.4) rather than high because, unlike a genuine natural law, alternative arrangements were visibly imaginable and were in fact implemented rapidly once Meiji power made implementation possible — the collapse of alternatives was institutional and enforced, not conceptual or physical.
 *
 * DIRECTIONALITY LOGIC:
 *   Bettoji administrators and the combined institutions sit at the beneficiary end: they set the interpretive terms case by case and captured the resulting resource flows, with arbitrage-grade exit (they could reframe the theology whenever convenient). The shogunate registration apparatus is a secondary institutional beneficiary using the fused network for surveillance ends unrelated to religious coordination. Lay practitioners are full targets: powerless, locally trapped, dependent on clerical mediation, bearing the cost of unresolved ambiguity without any capacity to demand resolution. Lower-ranking kami priests are targets with moderate organizational power but trapped exit — they could not credibly threaten to leave a system that had absorbed their institutional standing. Meiji-era Shinto purists are treated as payers rather than beneficiaries of the eventual resolution, because the cost of performing the disentanglement (which the bundle's centuries of accumulated ambiguity made enormous and often violent) fell on them even though they ultimately got the doctrinal clarity they sought — the founding-problem framing they inherited was never designed for clean separation, and unwinding a piton is expensive precisely because pitons are not built with an exit in mind.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling a genuinely useful medieval administrative solution (the founding problem: absorbing kami cults into an incoming Buddhist institutional order without provoking legitimacy conflict) as either permanently coordinative (the syncretic_fusion_reading's risk) or permanently coercive from the start (a naive snare reading would miss that early shinbutsu-shugo solved a real problem cheaply). The piton classification captures the actual trajectory: real coordination function at founding, gradual atrophy of that function as the underlying absorption problem was resolved (the founding_problem_status is authored as 'dead' by the late medieval period), and persistence past that point through institutional inertia, fiscal capture by bettoji administrators, and theatrical doctrinal performance — with no single concentrated beneficiary capturing enough to make it a snare, but diffuse extraction from lay practitioners and subordinated priests sustained because no actor had sufficient standing or theological ammunition to force a resolution until an external, more powerful actor (the Meiji state) had reason to impose one for its own state-building purposes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incoherence_vs_unrecovered_coherence,
    'Is the absence of a stable, unified ontology in the surviving record evidence that no such ontology ever existed (the incoherent_bundle_reading), or merely evidence that the ontology was oral, localized, and unrecorded in ways modern textual historiography cannot recover (which would favor either sibling reading)?',
    'Comparative analysis of surviving jingu-ji administrative and ritual documents across multiple regions and centuries, checking for convergent versus divergent theological vocabulary at sites with no direct institutional contact with one another. Convergence would weaken the incoherent_bundle_reading; persistent divergence would strengthen it.',
    'If a genuinely convergent, independently-arrived-at theology is found across disconnected sites, this reading''s core claim (that incoherence was structural and maintained rather than merely undocumented) weakens substantially and the constraint should be re-evaluated toward the syncretic_fusion_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_vs_unrecovered_coherence, empirical, 'Whether documentary silence indicates genuine incoherence or merely unrecorded local coherence.').

omega_variable(
    meiji_bunri_as_revelation_vs_imposition,
    'Does the difficulty and violence of the 1868 shinbutsu bunri separation demonstrate that the prior system was genuinely incoherent (separation was hard because there was nothing coherent to separate along a clean line), or does it demonstrate that the prior system WAS coherent but the Meiji state imposed an alien, artificially clean categorical scheme onto a functioning synthesis for its own state-building purposes (which would favor the syncretic_fusion_reading being violently overridden rather than merely revealed as incoherent)?',
    'Close reading of Meiji-era local administrative correspondence and shrine-temple petitions from the separation period itself, distinguishing cases where local actors describe confusion over categorization from cases where local actors describe a clear existing distinction being forcibly ignored by state officials.',
    'If local sources predominantly describe confusion and ad hoc improvisation during separation, this reading is strongly corroborated. If local sources predominantly describe a clear prior distinction being overridden, the domain_partition_reading gains ground instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_bunri_as_revelation_vs_imposition, conceptual, 'Whether Meiji-era separation difficulty reveals prior incoherence or reflects imposed artificial categories onto a prior coherent system.').

omega_variable(
    beneficiary_status_of_deliberate_ambiguity,
    'Was the maintained ambiguity a deliberate strategic choice by bettoji administrators (an intentional extraction technology), or an emergent, unplanned byproduct of decentralized local negotiation with no single strategic author?',
    'Search for explicit administrative or clerical writings that discuss the utility of NOT resolving the kami-Buddha relationship, versus writings that simply assume the ambiguity as unremarkable background.',
    'Explicit strategic writings would support classifying the arrangement closer to a snare (a captured extraction technology with an identifiable strategic author); their absence supports the piton reading authored here (extraction without a strategic author, sustained by diffuse institutional inertia rather than deliberate design).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_status_of_deliberate_ambiguity, empirical, 'Whether the ambiguity was strategically engineered or emerged without a strategic author.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 200, 0.3).
narrative_ontology:measurement(shin_tr_t500, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 500, 0.42).
narrative_ontology:measurement(shin_tr_t800, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 800, 0.55).
narrative_ontology:measurement(shin_tr_t1050, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1050, 0.65).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1200, 0.7).
narrative_ontology:measurement(shin_tr_t1300, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1300, 0.71).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(shin_be_t200, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 200, 0.35).
narrative_ontology:measurement(shin_be_t500, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 500, 0.45).
narrative_ontology:measurement(shin_be_t800, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 800, 0.53).
narrative_ontology:measurement(shin_be_t1050, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1050, 0.6).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1200, 0.62).
narrative_ontology:measurement(shin_be_t1300, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1300, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_coexistence_commitment__incoherent_bundle_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__incoherent_bundle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.1).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__domain_partition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language label 'shinbutsu-shugo' per the epsilon-invariance principle: the syncretic_fusion_reading (genuine honji-suijaku ontological unification), the domain_partition_reading (stable non-unified division of existential domains), and this incoherent_bundle_reading (no stable ontology of either kind; ambiguity maintained as institutional technology). Each reading carries a distinct epsilon and distinct beneficiary/victim structure appropriate to its own claim about what shinbutsu-shugo actually was; they are linked via network edges rather than merged into one story with a measurement parameter, per the kernel/reading discipline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
