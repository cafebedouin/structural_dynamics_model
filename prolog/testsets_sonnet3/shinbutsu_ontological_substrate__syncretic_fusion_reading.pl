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
 *   human_readable: Honji Suijaku as Ontological Unity of Kami and Buddhas (Syncretic Fusion Reading)
 *   domain: religious/institutional/metaphysical
 *
 * SUMMARY:
 *   This story instantiates the syncretic_fusion_reading of the
 *   shinbutsu_ontological_substrate kernel: the claim that kami and buddhas
 *   share a single underlying reality, with honji suijaku (original ground /
 *   manifest trace) describing a metaphysical fact about that shared reality
 *   rather than a mere administrative or diplomatic accommodation between two
 *   separate religious institutions. On this reading, the combinatory
 *   shrine-temple complexes (jingu-ji) that dominated Japanese religious
 *   institutional life from roughly the Heian through early Meiji periods
 *   were not opportunistic bundling of convenience but the correct
 *   institutional expression of a real ontological unity. The extractiveness
 *   and suppression metrics describe how this reading's institutions actually
 *   operated — concentrating land, ritual authority, and interpretive control
 *   in combinatory clergy and courts — independent of whether the ontological
 *   claim itself is true. Two sibling readings of the same kernel are NOT
 *   represented here: domain_partition_reading holds that kami and buddhas
 *   govern separate domains and coexist functionally rather than
 *   ontologically; incoherent_bundle_reading holds that no coherent kernel
 *   exists at all, and that syncretism was accumulated institutional drift
 *   enforced by the state. Each sibling is authored as its own constraint
 *   story with its own ε and its own stakeholder structure; this file
 *   addresses only the fusion reading.
 *
 * KEY AGENTS:
 *   - shingon_tendai_combinatory_clergy: agenda_setter/beneficiary (institutional/arbitrage) — administers the fused cosmology and its ritual infrastructure
 *   - imperial_court_ritual_authority: beneficiary/agenda_setter (institutional/arbitrage) — legitimacy depends on the fusion holding
 *   - kokugaku_pure_shinto_scholars: payer (moderate/constrained) — foreclosed by the dominant doctrine
 *   - independent_shrine_priests: payer (powerless/trapped) — economically dependent on combinatory institutions
 *   - comparative_religion_historians: observer (analytical) — assesses genuineness of the ontological commitment versus retrospective systematization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.58).
domain_priors:suppression_score(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.71).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__syncretic_fusion_reading, "Honji Suijaku as Ontological Unity of Kami and Buddhas (Syncretic Fusion Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__syncretic_fusion_reading, "religious/institutional/metaphysical").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__syncretic_fusion_reading, '8f9ac767-11b7-4427-926a-9366b83967cb').
narrative_ontology:cs_kernel_codification('8f9ac767-11b7-4427-926a-9366b83967cb', formalized).
narrative_ontology:cs_authority_grounding('8f9ac767-11b7-4427-926a-9366b83967cb', lineage).
narrative_ontology:cs_interpretation_layer_present('8f9ac767-11b7-4427-926a-9366b83967cb').
narrative_ontology:cs_reading_relation('8f9ac767-11b7-4427-926a-9366b83967cb', shinbutsu_ontological_substrate__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f9ac767-11b7-4427-926a-9366b83967cb', shinbutsu_ontological_substrate__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('8f9ac767-11b7-4427-926a-9366b83967cb', foundational, kami_buddha_nondual_ground).
narrative_ontology:cs_axiom_status(kami_buddha_nondual_ground, holdable).
narrative_ontology:cs_axiom_grounding('8f9ac767-11b7-4427-926a-9366b83967cb', kami_buddha_nondual_ground, theological).
narrative_ontology:cs_axiom('8f9ac767-11b7-4427-926a-9366b83967cb', foundational, honji_suijaku_describes_discovered_fact).
narrative_ontology:cs_axiom_status(honji_suijaku_describes_discovered_fact, holdable).
narrative_ontology:cs_axiom_grounding('8f9ac767-11b7-4427-926a-9366b83967cb', honji_suijaku_describes_discovered_fact, theological).
narrative_ontology:cs_axiom('8f9ac767-11b7-4427-926a-9366b83967cb', secondary, combinatory_institutions_are_correct_expression).
narrative_ontology:cs_axiom_status(combinatory_institutions_are_correct_expression, holdable).
narrative_ontology:cs_axiom_grounding('8f9ac767-11b7-4427-926a-9366b83967cb', combinatory_institutions_are_correct_expression, conventional).
narrative_ontology:cs_reference_frame('8f9ac767-11b7-4427-926a-9366b83967cb', heian_combinatory_orthodoxy).
narrative_ontology:cs_drift_state('8f9ac767-11b7-4427-926a-9366b83967cb', meiji_shinbutsu_bunri_aftermath, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('8f9ac767-11b7-4427-926a-9366b83967cb', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, shingon_tendai_combinatory_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, shrine_temple_administrative_complexes).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, imperial_court_ritual_authority).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, kokugaku_pure_shinto_scholars).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, independent_shrine_priests).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, commoner_devotees_denied_domain_specific_practice).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, buddha_kami_nondual_essence).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__syncretic_fusion_reading, honji_suijaku_metaphysical_truth_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the combinatory shrine-temple complexes (jingu-ji), performs the ritual technologies (kanjo, goma) that stage kami as local manifestations of specific buddhas, and produces the doctrinal literature explaining why this is metaphysically necessary rather than administratively convenient. Controls both the theological framing and the physical infrastructure (land, ritual calendars, ordination) through which the fused cosmology is enacted. Extremely difficult for any rival account to displace without dismantling the institutions themselves.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, shingon_tendai_combinatory_clergy, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, shingon_tendai_combinatory_clergy, beneficiary).

% Combined jingu-ji institutions hold land grants, tax exemptions, and pilgrimage revenue that depend on the fused cosmology being treated as settled fact rather than negotiated compromise. Separating the shrine and temple functions would require re-litigating property, personnel, and ritual authority all at once, so the institutions have strong structural interest in the unity claim persisting undisturbed.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, shrine_temple_administrative_complexes, beneficiary,
    institutional, generational, constrained, national).

% The court's own legitimacy rests partly on a cosmology in which imperial kami ancestry and Buddhist state-protection doctrine reinforce rather than compete with each other. Sponsors combinatory ritual, endows temples attached to shrines, and benefits from a metaphysics that makes the emperor simultaneously kami-descended and buddha-protected without contradiction.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, imperial_court_ritual_authority, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__syncretic_fusion_reading, imperial_court_ritual_authority, agenda_setter).

% Argue for a kami tradition uncontaminated by Buddhist metaphysics as prior and self-standing. Their scholarship and institutional standing are foreclosed by the dominance of the fusion doctrine in official ritual and education; they must work against, not within, the established combinatory infrastructure, and their exit is blocked by the fact that the institutions holding land and ritual authority are combinatory ones.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, kokugaku_pure_shinto_scholars, payer,
    moderate, generational, constrained, national).

% Shrine priests who wish to conduct kami ritual without Buddhist overlay find their shrines administratively subordinated to attached temples, their ritual calendars set by combinatory doctrine, and their income tied to pilgrimage patterns organized around the fused cosmology. Leaving the arrangement means losing the shrine's institutional and economic base entirely.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, independent_shrine_priests, payer,
    powerless, biographical, trapped, regional).

% Villagers seeking a straightforward this-world kami blessing (harvest, childbirth, protection) encounter ritual practice already fused with Buddhist soteriology and mediated by combinatory clergy, whether or not they hold or care about the underlying metaphysical claim. Their devotional options are shaped by whichever fused institution serves their locality; a purely domain-separated kami cult is largely unavailable to them.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, commoner_devotees_denied_domain_specific_practice, payer,
    powerless, biographical, constrained, local).

% Later state reformers who forcibly separated kami and buddha worship are not part of this constraint's operative period but represent the counter-claim that the fusion was never metaphysically necessary, only administratively entrenched — their eventual success in dismantling combinatory institutions is evidence considered by the sibling incoherent_bundle_reading, not adjudicated within this reading.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, meiji_era_shinbutsu_bunri_reformers, excluded,
    organized, generational, mobile, national).

% Assess whether honji suijaku doctrine reflects a genuine, stable metaphysical commitment shared across combinatory institutions, or a retrospectively systematized description of what was actually a looser, more contested set of local accommodations. Their scholarship draws on temple records, ritual manuals, and doctrinal treatises without a stake in any institution's survival.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, comparative_religion_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__syncretic_fusion_reading, shingon_tendai_combinatory_clergy).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__syncretic_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single coherent cosmology under which kami worship and Buddhist practice can be conducted by the same institutions, using the same personnel and calendar, without requiring devotees or clergy to choose between traditions or treat one as subordinate to the other in a zero-sum way.
% TRANSFER_FUNCTION: Moves ritual authority, land revenue, and interpretive control from independent kami-only or buddha-only practice toward combinatory institutions (jingu-ji) and the clergy who administer the fused doctrine; moves devotional practice choice away from commoners and independent priests toward whatever synthesis the local combinatory institution has settled on.
% ABSENT_VOICES: Independent shrine priests and kokugaku scholars would object that the fusion is a theological overlay serving institutional consolidation rather than a discovered metaphysical fact; they are structurally present in the historical record but excluded from having set the terms of the doctrine when it was consolidated under aristocratic and clerical sponsorship.
% DISAPPEARANCE_RATIONALE: If the ontological-unity claim were withdrawn overnight, the combinatory shrine-temple institutions would lose their doctrinal justification for joint administration; land and ritual authority currently unified under jingu-ji would need to be re-partitioned between kami-focused and buddha-focused claimants, a process that in fact occurred (destructively) at the Meiji shinbutsu bunri separation — confirming that real arrangements, not merely descriptive language, depended on the unity claim holding.
% FOUNDING_PROBLEM: Early esoteric Buddhist and court ritualists needed a framework that let Buddhist institutions absorb, rather than compete with, the powerful and locally entrenched kami cults whose cooperation was necessary for both spiritual and political legitimacy across the archipelago.
% FOUNDING_PROBLEM_CORROBORATION: Kokugaku scholars, writing from outside the combinatory institutions that benefit from the fusion doctrine, attest that the founding problem was institutional accommodation rather than discovered metaphysical truth, and that the doctrine persisted because it served the clergy and court that administered it. Comparative religion historians examining temple records independently corroborate that doctrinal systematization of honji suijaku intensified precisely when combinatory institutions needed to defend consolidated landholdings, suggesting institutional interest shaped the metaphysical claim rather than the reverse — though this reading holds that the unity is nonetheless genuinely believed and lived by the combinatory clergy themselves, not merely instrumentally deployed.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__syncretic_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58 at interval end) reflects that the fusion doctrine channels land revenue, ritual authority, and interpretive control toward combinatory institutions and away from independent or domain-specific practitioners, and this channeling grows more pronounced as the doctrine becomes more systematized and institutionally load-bearing over the interval. Suppression (0.71) is high because sustaining the fusion claim as settled metaphysical fact — rather than one contestable reading among several — requires actively marginalizing kokugaku-style separatist scholarship and subordinating independent shrine priests administratively. Theater ratio is moderate and rising (0.12 to 0.32): the combinatory ritual technologies (kanjo, goma) are doctrinally substantive, not pure performance, but an increasing share of institutional energy over the period goes into defending the unity claim against rival readings rather than into the ritual function itself. Accessibility collapse (0.62) is high but not maximal: alternative religious practice (domain-separated kami worship, independent Buddhist practice) persisted throughout, but became structurally harder to access as combinatory institutions consolidated land and personnel.
 *
 * PERSPECTIVAL GAP:
 *   From the combinatory clergy's seat, the arrangement is the correct institutional expression of a discovered metaphysical unity — genuine coordination grounded in truth, not extraction. From the kokugaku scholar's or independent priest's seat, the same arrangement is enforced doctrinal dominance protecting institutional consolidation. The engine computes these as different effective classifications from the same structural data (power, exit options, beneficiary/victim declarations) rather than adjudicating whose metaphysics is correct — that adjudication is exactly what the omega variables below leave open.
 *
 * DIRECTIONALITY LOGIC:
 *   Combinatory clergy, shrine-temple complexes, and the imperial court sit near the beneficiary end: they collect ritual authority, land revenue, and legitimacy from the fused cosmology and could not easily separate their institutional position from it (very low d). Kokugaku scholars and independent shrine priests sit near the target end: their interpretive and institutional alternatives are foreclosed by the dominance of the fusion doctrine, and their exit options are constrained or trapped respectively because leaving means abandoning land, income, or scholarly standing built against an entrenched combinatory establishment. Commoner devotees are payers in a diffuse sense — they bear the cost of reduced access to domain-specific practice without directly funding the extraction the way institutional payers do.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling powerful, locally entrenched kami cults with an expanding Buddhist institutional and political order — is genuinely contested as to whether it remains live. The combinatory institutions themselves treat it as permanently live (the unity is metaphysically true, so there is no obsolescence to speak of). Outside observers, including later Meiji-era separatists, treat the founding problem as having been an institutional accommodation whose energy was long since spent, with the doctrine surviving mainly to protect consolidated landholdings and ritual authority. This reading takes the position that the unity is genuinely and durably held by combinatory clergy, not cynically deployed — but the metrics measure institutional operation, not sincerity of belief, so a genuinely-held metaphysical commitment can still operate with high suppression and rising extraction if its institutions have strong material interests in its continuation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_ontology_vs_institutional_convenience,
    'Is the claimed ontological unity of kami and buddhas a genuine, independently-arrived-at metaphysical commitment held by combinatory clergy, or is it a doctrinal formalization that emerged to justify and protect institutional consolidation that had already occurred for other (land, political, ritual-monopoly) reasons?',
    'Comparative analysis of doctrinal treatises'' dating against land-grant and institutional consolidation records: if systematic honji suijaku theology precedes or is independent of major consolidation events, the genuine-commitment reading gains support; if doctrinal systematization consistently follows and tracks consolidation and land-defense needs, the institutional-convenience reading (closer to incoherent_bundle_reading) gains support.',
    'If the doctrine is shown to systematically track institutional interest rather than precede it, this reading''s claim that the unity is metaphysically discovered truth becomes much harder to sustain, and the constraint''s classification would drift toward the pure extraction / tangled rope pole documented here becoming dominant rather than contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_ontology_vs_institutional_convenience, conceptual, 'Whether the fusion doctrine is prior metaphysical discovery or retrospective institutional justification.').

omega_variable(
    meiji_separation_as_falsification,
    'Does the forcible and largely successful shinbutsu bunri separation at the Meiji restoration count as evidence against the durability of the claimed ontological unity (suggesting it was administratively separable all along), or is it better read as a political rupture that destroyed a real metaphysical synthesis for state-building reasons unrelated to the synthesis''s truth or coherence?',
    'Examination of how readily and completely institutions separated when compelled: rapid, clean separation with little theological resistance would support the separability/incoherent-bundle direction; significant theological resistance, syncretic survivals, and later re-syncretization attempts would support the genuine-unity direction.',
    'Resolution in the separability direction would strengthen the domain_partition_reading and incoherent_bundle_reading siblings at this reading''s expense; resolution in the resistance/survival direction would strengthen this reading''s claim that the unity was a real, durable commitment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_separation_as_falsification, empirical, 'Whether the historical fact of successful state-enforced separation undermines the ontological-unity claim.').

omega_variable(
    framing_under_determination_kernel_or_institution,
    'Two coherent framings compete for what the ''kernel'' under contest actually is: (a) the metaphysical proposition that kami and buddhas share one ground, evaluated on its own terms as a theological claim; or (b) the institutional arrangement (jingu-ji, ritual monopoly, land tenure) that the metaphysical proposition was used to justify, evaluated as a structure of authority. Framing (a) supports treating this as a live commitment-system reading with genuine axioms; framing (b) collapses toward the incoherent_bundle_reading, where the ''kernel'' is just a label for accumulated arrangements.',
    'This story adopts framing (a) because the schema calls for readings of a contested KERNEL (a persisting commitment), and the source material explicitly frames the claim as ontological rather than institutional. Framing (b) is the domain of the incoherent_bundle_reading sibling by design.',
    'If framing (b) is adopted instead, this reading''s cs_pattern classification would likely shift: the kernel_codification would move from formalized toward distributed or implicit, and authority_grounding would move from lineage/practice toward extraction more starkly, collapsing much of the distinction this reading currently makes with incoherent_bundle_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_under_determination_kernel_or_institution, conceptual, 'Alternative framings of what counts as the kernel under contest — metaphysical proposition versus institutional arrangement — and how the choice shapes classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(shin_tr_t20, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(shin_tr_t40, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement(shin_tr_t60, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(shin_tr_t80, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 80, 0.29).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 100, 0.32).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(shin_be_t20, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(shin_be_t40, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 40, 0.49).
narrative_ontology:measurement(shin_be_t60, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 60, 0.53).
narrative_ontology:measurement(shin_be_t80, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 80, 0.56).
narrative_ontology:measurement(shin_be_t100, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(shin_su_t20, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(shin_su_t40, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(shin_su_t60, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(shin_su_t80, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 80, 0.68).
narrative_ontology:measurement(shin_su_t100, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 100, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__syncretic_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the shinbutsu_ontological_substrate kernel. domain_partition_reading holds kami and buddhas govern separate domains with functional, not ontological, coexistence — lower extraction, framed as pragmatic division of religious labor. incoherent_bundle_reading holds no coherent kernel exists at all — syncretism as accumulated institutional drift under state enforcement, likely the highest suppression and lowest doctrinal coherence of the three. This reading (syncretic_fusion) claims genuine ontological unity and sits between the two structurally: higher institutional entanglement and resistance to separation than domain_partition_reading, but a more coherent and sincerely-held doctrinal core than incoherent_bundle_reading credits. All three share the same historical institutions (jingu-ji, combinatory clergy) but differ in what those institutions are held to be doing metaphysically, which changes beneficiary/victim framing and the ε each reading authors for the standing arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_substrate__syncretic_fusion_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
