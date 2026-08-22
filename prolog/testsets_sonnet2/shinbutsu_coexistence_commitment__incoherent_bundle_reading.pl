% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: shinbutsu_coexistence_commitment__incoherent_bundle_reading
 *   human_readable: Shinbutsu-shugo as Institutionally Maintained Incoherent Bundle
 *   domain: religious/philosophical/historical
 *
 * SUMMARY:
 *   For roughly a millennium, Japanese religious institutions operated
 *   combined shrine-temple complexes (jingu-ji) in which kami veneration and
 *   Buddhist practice were administratively and physically fused without any
 *   single settled account of what the enshrined kami actually were relative
 *   to Buddhist cosmology — sometimes protector spirits, sometimes deluded
 *   beings in need of salvation, sometimes local manifestations of
 *   transcendent buddhas, sometimes none of these consistently within the
 *   same institution. This reading holds that the absence of a settled answer
 *   was not an oversight later corrected by honji-suijaku theology, but a
 *   structural feature exploited by administrators, ascetic lineages, and
 *   court ritualists who each drew authority and revenue from the flexibility
 *   to invoke whichever register suited the occasion. The 1868 Meiji state
 *   edicts forcing shinbutsu bunri (separation) did not resolve a preexisting
 *   synthesis; they demanded an answer the system had never been built to
 *   supply, and the resulting violence, iconoclasm, and administrative chaos
 *   is read here as diagnostic evidence of how much institutional weight the
 *   ambiguity had been carrying.
 *
 * KEY AGENTS:
 *   - shrine_temple_administrators: institutional beneficiary — draws dual revenue from unresolved ontological status
 *   - shugen_ascetic_lineages: organized beneficiary — professional expertise constituted by fluency in the ambiguity
 *   - lay_practitioners_seeking_doctrinal_clarity: powerless payer — bears confusion and redundant obligation with no standing to demand resolution
 *   - local_kami_cult_custodians: moderate-power payer — pre-Buddhist authority quietly subordinated without consent
 *   - meiji_state_bureaucracy: institutional agenda-setter — forces the categorical question the bundle was built to avoid
 *   - religious_studies_scholars: analytical observer — sees the bundle's structural function retrospectively
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
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, piton).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "Shinbutsu-shugo as Institutionally Maintained Incoherent Bundle").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "religious/philosophical/historical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'e30e9fbb-9286-44f4-b1e4-86211c51f7da').
narrative_ontology:cs_kernel_codification('e30e9fbb-9286-44f4-b1e4-86211c51f7da', distributed).
narrative_ontology:cs_authority_grounding('e30e9fbb-9286-44f4-b1e4-86211c51f7da', practice).
narrative_ontology:cs_interpretation_layer_present('e30e9fbb-9286-44f4-b1e4-86211c51f7da').
narrative_ontology:cs_reading_relation('e30e9fbb-9286-44f4-b1e4-86211c51f7da', shinbutsu_coexistence_commitment__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('e30e9fbb-9286-44f4-b1e4-86211c51f7da', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_axiom('e30e9fbb-9286-44f4-b1e4-86211c51f7da', foundational, no_stable_kami_buddha_ontology_was_ever_settled).
narrative_ontology:cs_axiom_status(no_stable_kami_buddha_ontology_was_ever_settled, holdable).
narrative_ontology:cs_axiom_grounding('e30e9fbb-9286-44f4-b1e4-86211c51f7da', no_stable_kami_buddha_ontology_was_ever_settled, empirically_contingent).
narrative_ontology:cs_axiom('e30e9fbb-9286-44f4-b1e4-86211c51f7da', foundational, institutional_ambiguity_avoidance_is_structural_not_incidental).
narrative_ontology:cs_axiom_status(institutional_ambiguity_avoidance_is_structural_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('e30e9fbb-9286-44f4-b1e4-86211c51f7da', institutional_ambiguity_avoidance_is_structural_not_incidental, empirically_contingent).
narrative_ontology:cs_axiom('e30e9fbb-9286-44f4-b1e4-86211c51f7da', secondary, meiji_bunri_reveals_rather_than_creates_incoherence).
narrative_ontology:cs_axiom_status(meiji_bunri_reveals_rather_than_creates_incoherence, holdable).
narrative_ontology:cs_axiom_grounding('e30e9fbb-9286-44f4-b1e4-86211c51f7da', meiji_bunri_reveals_rather_than_creates_incoherence, empirically_contingent).
narrative_ontology:cs_reference_frame('e30e9fbb-9286-44f4-b1e4-86211c51f7da', unspecified_dual_registry_practice).
narrative_ontology:cs_drift_state('e30e9fbb-9286-44f4-b1e4-86211c51f7da', meiji_bunri_edicts_1868, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('e30e9fbb-9286-44f4-b1e4-86211c51f7da', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_temple_administrators).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shugen_ascetic_lineages).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, court_ritual_specialists).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, lay_practitioners_seeking_doctrinal_clarity).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, local_kami_cult_custodians).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_doctrinal_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer combined shrine-temple complexes (jingu-ji) where kami worship and Buddhist ritual are physically and administratively co-located. Draw revenue, land grants, and political patronage from both Shinto and Buddhist channels simultaneously, and have no incentive to resolve which ontological status governs the enshrined kami — clarity would force a choice that would cost one revenue stream or the other.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_temple_administrators, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_temple_administrators, agenda_setter).

% Practice mountain asceticism (shugendo) whose ritual efficacy depends on treating kami and buddhas/bodhisattvas as interchangeable or mutually implicating without specifying the mechanism. Their professional identity and transmitted lineage authority are constituted by operating fluently within the ambiguity; a forced ontological resolution would dissolve the specific expertise they sell.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shugen_ascetic_lineages, beneficiary,
    organized, generational, constrained, regional).

% Design and perform state ritual (onmyodo-adjacent, imperial rites) that draws legitimacy from both kami ancestry claims and Buddhist cosmological authority at once. Their political usefulness to the court depends on being able to invoke either register depending on the occasion; forcing a single coherent ontology would strip them of half their justificatory toolkit.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, court_ritual_specialists, beneficiary,
    powerful, generational, constrained, national).

% Attend shrine-temple complexes for birth, marriage, death, and harvest rites without ever receiving a stable answer to what, ontologically, they are venerating or what happens to a person after death. They bear the cost of doctrinal incoherence as confusion, redundant ritual obligation (paying both shrine and temple fees for functionally overlapping rites), and no standing to demand resolution.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, lay_practitioners_seeking_doctrinal_clarity, payer,
    powerless, biographical, trapped, local).

% Maintain pre-Buddhist local kami cults that were absorbed into combinatory shrine-temple complexes under Buddhist administrative and doctrinal seniority. Their autonomous cultic authority was subordinated to temple bureaucracy under the honji-suijaku framing without their prior consent, and unwinding the arrangement (which Meiji bunri eventually forced) revealed how much of their independent status had been quietly displaced.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, local_kami_cult_custodians, payer,
    moderate, generational, constrained, regional).

% Sects and scholars pushing for doctrinal precision (on rebirth, buddha-nature, the status of local deities) find their arguments perpetually deflected by the institutional convenience of leaving kami-buddha relations unspecified. They pay in stalled reform: precision-seeking factions lose influence to administrators who profit from the fog.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_doctrinal_reformers, payer,
    moderate, generational, constrained, national).

% Enacts shinbutsu bunri (forced separation) in 1868, demanding that every shrine-temple complex declare a single ontological affiliation. Their edicts do not create the incoherence; they demand an answer the bundle was never designed to give, and the scramble, violence, and iconoclasm that follows exposes how much of the prior arrangement depended on nobody ever asking the question directly.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_state_bureaucracy, agenda_setter,
    institutional, generational, mobile, national).

% Study the pre-Meiji arrangement retrospectively and disagree about whether it constituted a genuine ontological synthesis, a domain-partitioned coexistence, or (as this reading holds) an incoherent bundle never required to resolve its own contradictions until state power forced the issue.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, religious_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_temple_administrators).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__incoherent_bundle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The bundle allowed a single ritual-administrative complex to serve overlapping but distinct needs (purity/harvest/life-passage via kami rites, death/salvation/afterlife via Buddhist rites) under one roof and one revenue structure, without requiring participants or administrators to adjudicate the metaphysical relationship between the two systems.
% TRANSFER_FUNCTION: Moves ritual fees, land-grant revenue, and political legitimacy toward shrine-temple administrators, shugen lineages, and court ritualists who can operate fluently across both registers; moves confusion, redundant obligation, and subordinated cultic autonomy onto lay practitioners and local kami custodians who cannot demand a resolved answer.
% ABSENT_VOICES: Local kami cult custodians whose pre-Buddhist ritual authority was quietly absorbed had no forum to object at the time of absorption; lay practitioners had no doctrinal standing to demand clarity; both surface only retrospectively, through Meiji-era disputes and modern scholarship, not through any contemporaneous channel.
% DISAPPEARANCE_RATIONALE: The 1868 shinbutsu bunri edicts show exactly what happens when the ambiguity is forcibly removed: physical destruction of Buddhist statuary at shrines, wholesale reclassification of clergy, violent factional disputes over which deities belonged to which registry, and the material dismantling of jingu-ji complexes. The world did rearrange itself — abruptly and destructively — which is direct evidence that the pre-Meiji arrangement was doing load-bearing institutional work, not merely describing a settled metaphysical fact.
% FOUNDING_PROBLEM: Early esoteric Buddhist missionizing needed a way to coexist with, rather than displace, entrenched local kami cults whose ritual and political authority long predated Buddhist arrival; leaving the kami-buddha relationship unspecified let both authority structures operate side by side without either side conceding subordination.
% FOUNDING_PROBLEM_CORROBORATION: Meiji-era state bureaucrats and post-Restoration Shinto nationalist scholars attest the founding problem was long dead — that the 'coexistence' had degraded into pure institutional convenience for jingu-ji administrators with no live doctrinal content, which was their stated justification for bunri. Independent religious-studies scholarship (outside both the temple-administrator beneficiary class and the Meiji state's nativist interest) corroborates that the ambiguity had become self-perpetuating administrative practice rather than an active theological synthesis by the early modern period, though it disputes the state's claim that separation restored any original 'purity' rather than simply imposing a new, equally constructed ontology.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__incoherent_bundle_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at a moderate 0.62 rather than a snare-level figure because the primary function — allowing overlapping ritual needs to be served under one roof — was genuinely useful to lay practitioners even as its ambiguity was exploited; the extraction is in the deflection of doctrinal accountability and the quiet subordination of prior kami-cult authority, not in a straightforward transfer of resources from a clearly identified victim class. Theater ratio is authored high (0.71 at interval end) because a large and increasing share of institutional activity over the centuries consisted of ritual performance whose primary function became maintaining the appearance of coherent synthesis rather than resolving or even addressing the underlying categorical tension — the honji-suijaku doctrinal apparatus itself functioned increasingly as theater papering over a bundle that was never actually reconciled. Accessibility collapse is moderate (0.4) rather than high because alternative framings (domain partition, or simply declaring one tradition subordinate) were always structurally available and periodically argued for by doctrinal reformers; the bundle persisted through institutional convenience and avoidance, not because alternatives were foreclosed. Resistance is moderate (0.45): doctrinal reformers and some kami-cult custodians periodically pushed for clarity throughout the pre-Meiji period, but lacked the institutional power to force resolution until the Meiji state's external intervention.
 *
 * PERSPECTIVAL GAP:
 *   From the administrator/beneficiary seat, the bundle looks like a successful, centuries-stable synthesis — genuine rope. From the lay-practitioner or kami-custodian seat, the same arrangement looks like an imposed, unaccountable fog that quietly redistributed authority and revenue without ever being answerable to a coherent doctrine. The Meiji state's own seat is neither: it treats the ambiguity as a foreign contamination to be purged, which is itself a retrospective, politically motivated framing (Shinto nationalist purification) rather than a neutral diagnosis — the engine should register that the state's 'clarifying' intervention was itself extractive in a new direction (nationalist reconstruction), a separate constraint from this one.
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine-temple administrators, shugen lineages, and court ritualists are declared beneficiaries because their revenue, professional identity, and political utility are constituted by the ambiguity itself — resolving it would cost each of them something specific (a revenue stream, a professional monopoly, a rhetorical register). Lay practitioners are declared victims not because they are actively targeted but because they bear the diffuse cost of never getting a straight answer about what they are venerating or what awaits them after death, while having no institutional lever to demand one — this is the classic piton signature: no concentrated capturer extracting rents through active coercion, but a diffuse cost borne by the powerless while administrators (who could resolve the ambiguity) have no incentive to. Local kami cult custodians and Buddhist doctrinal reformers are victims of a more specific kind: their institutional projects (autonomous cultic authority; doctrinal precision) were structurally deflected by an arrangement optimized for administrative convenience rather than either theological or cultic coherence.
 *
 * MANDATROPHY ANALYSIS:
 *   The claimed type is piton rather than snare or tangled_rope specifically because no single stakeholder is positioned as an active, concentrated extractor coercively suppressing exit — the founding problem (peaceful coexistence between incoming Buddhist missionizing and entrenched kami cults) was genuinely live in the early centuries, and the bundle's ambiguity was a workable, even valuable, improvisation at that stage. What makes this a piton rather than a rope that simply persisted is the founding_problem_status of 'contested': by the time of Meiji bunri, the practical coexistence problem the bundle was built to solve had substantially receded (Buddhism was thoroughly entrenched, syncretic institutions had existed for centuries), yet the ambiguous bundle persisted primarily because shrine-temple administrators profited from the accounting flexibility, not because the coexistence problem remained live. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges is exactly the piton signature this framework is built to detect: something did rearrange when it was removed (proving it was load-bearing), but what it was load-bearing FOR by 1868 was institutional convenience and revenue capture, not the original coexistence problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bundle_vs_synthesis_ambiguity,
    'Was shinbutsu-shugo ever intended or experienced as a genuine ontological synthesis by any significant class of premodern practitioners, or is ''incoherent bundle'' itself a retrospective imposition by modern scholars applying post-Meiji categorical expectations the premodern system never had to meet?',
    'Close reading of premodern doctrinal texts (e.g. Ryobu Shinto commentaries, temple administrative records) for evidence of whether practitioners themselves registered the tension as a problem requiring resolution, versus treating the coexistence as simply unremarkable.',
    'If premodern sources show no felt tension, this reading''s claim that avoidance was a deliberate structural strategy weakens toward the domain_partition_reading (the domains were simply experienced as separate, not held in an avoided tension); if sources show recurring anxious deflection of the categorical question, this reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bundle_vs_synthesis_ambiguity, empirical, 'Whether premodern practitioners experienced felt ontological tension or simple domain separation.').

omega_variable(
    meiji_bunri_revealing_vs_creating,
    'Did Meiji-era shinbutsu bunri reveal a pre-existing incoherence, or did the violent, forced-choice framing of the 1868 edicts itself manufacture an incoherence narrative to serve Meiji Shinto-nationalist state-building, retroactively projecting incoherence onto an arrangement that functioned adequately on its own terms?',
    'Comparative analysis of shrine-temple complex administrative records immediately before 1868 versus post-bunri state propaganda, checking whether internal institutional documents show signs of unresolved strain prior to state intervention or only after the state demanded resolution.',
    'If pre-1868 records show internal strain, this reading''s ''revealing not creating'' claim is corroborated. If pre-1868 records show stable, unremarked function and only post-1868 sources narrate incoherence, the reading would need revision toward treating Meiji bunri as constructing rather than exposing the bundle''s incoherence — a materially different structural claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_bunri_revealing_vs_creating, conceptual, 'Whether Meiji bunri exposed prior incoherence or narratively constructed it for state purposes.').

omega_variable(
    beneficiary_class_natural_or_constructed,
    'Are shrine-temple administrators, shugen lineages, and court ritualists genuine structural beneficiaries of the ambiguity, or would a resolved ontology (in either syncretic or partition form) have generated comparable institutional revenue through different means, making the ambiguity incidental rather than load-bearing for their benefit?',
    'Counterfactual institutional analysis: examine post-bunri Shinto shrine administration revenue and authority structures to see whether administrators who lost the dual-registry flexibility recovered comparable institutional position through the new single-registry (State Shinto) arrangement.',
    'If administrators recovered comparable position under State Shinto''s resolved ontology, the ambiguity itself was not the necessary condition for their benefit, weakening the tangled_rope-adjacent ''deliberate ambiguity as extraction mechanism'' component of this reading in favor of a more piton-like ''administrators benefit from whatever arrangement exists'' reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_class_natural_or_constructed, empirical, 'Whether administrator benefit was specifically tied to ambiguity or would transfer to any resolved arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(shin_tr_t0, projected).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 200, 0.42).
narrative_ontology:measurement_basis(shin_tr_t200, projected).
narrative_ontology:measurement(shin_tr_t500, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 500, 0.5).
narrative_ontology:measurement_basis(shin_tr_t500, projected).
narrative_ontology:measurement(shin_tr_t800, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 800, 0.58).
narrative_ontology:measurement_basis(shin_tr_t800, projected).
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1000, 0.65).
narrative_ontology:measurement_basis(shin_tr_t1000, projected).
narrative_ontology:measurement(shin_tr_t1150, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1150, 0.7).
narrative_ontology:measurement_basis(shin_tr_t1150, projected).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 1200, 0.71).
narrative_ontology:measurement_basis(shin_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(shin_be_t0, projected).
narrative_ontology:measurement(shin_be_t200, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 200, 0.36).
narrative_ontology:measurement_basis(shin_be_t200, projected).
narrative_ontology:measurement(shin_be_t500, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 500, 0.44).
narrative_ontology:measurement_basis(shin_be_t500, projected).
narrative_ontology:measurement(shin_be_t800, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 800, 0.51).
narrative_ontology:measurement_basis(shin_be_t800, projected).
narrative_ontology:measurement(shin_be_t1000, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1000, 0.57).
narrative_ontology:measurement_basis(shin_be_t1000, projected).
narrative_ontology:measurement(shin_be_t1150, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1150, 0.61).
narrative_ontology:measurement_basis(shin_be_t1150, observed).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 1200, 0.62).
narrative_ontology:measurement_basis(shin_be_t1200, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_coexistence_commitment__incoherent_bundle_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__incoherent_bundle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.06).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__domain_partition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the shinbutsu_coexistence_commitment kernel. syncretic_fusion_reading claims genuine ontological unification via honji suijaku (near-mountain/rope framing, low authored extraction, since the reading itself holds the synthesis was real and functional). domain_partition_reading claims clean separation of existential domains without need for unification (rope-adjacent, coordination without contested ontology). This incoherent_bundle_reading claims neither unification nor clean partition occurred — instead a piton-adjacent bundle sustained by institutional avoidance of the categorical question, with meaningfully higher authored extraction and theater_ratio than either sibling, because this reading's whole point is that the apparent coordination function was itself partly performative. The three readings share the same underlying historical kernel (pre-Meiji shrine-temple institutional practice) but diverge sharply on ε and claimed_type because they diverge on what, structurally, was actually happening — exactly the situation the ε-invariance principle requires decomposing into separate stories rather than averaging.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
