% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__syncretic_fusion_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__syncretic_fusion_reading
 *   human_readable: Honji Suijaku Syncretic Fusion Commitment (Syncretic Fusion Reading)
 *   domain: religious/philosophical/historical
 *
 * SUMMARY:
 *   From roughly the eleventh century to the Meiji separation, the dominant
 *   Japanese religious settlement identified kami with Buddhist originals:
 *   kami were suijaku, manifest traces, of honji — the universal Buddhas and
 *   bodhisattvas. The doctrine was authored by esoteric monastic elites
 *   (Ryobu and Sanno systems), embodied institutionally in shrine-temples
 *   where Buddhist clergy resided at kami shrines, performed ritually in kami
 *   ordinations and bodhisattva titles, and enforced increasingly by warrior
 *   governments culminating in mandatory temple registration. The arrangement
 *   carried a genuine integrative function — it legitimized kami worship
 *   inside the East Asian Buddhist cosmopolis and gave communities a single
 *   salvific frame — while extracting doctrinal authority, administrative
 *   control, and material flows from the shrine side to the temple side. It
 *   was terminated not by internal decay but by revolutionary state violence
 *   in 1868. Claim/metric independence: the claimed type is tangled_rope
 *   because both a real coordination function and asymmetric extraction are
 *   structurally present; the metrics are authored independently as
 *   descriptive truths of the arrangement's operation. Family note: the
 *   colloquial label 'shinbutsu shugo' decomposes, per epsilon-invariance,
 *   into three structurally distinct claims — this file (ontological
 *   unification), the domain-partition sibling, and the incoherent-bundle
 *   sibling — linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - esoteric_monastic_establishments: agenda-setting principal beneficiary (institutional/arbitrage) — authors the doctrine, staffs the shrine-temples, collects the flows, and can reframe rather than exit
 *   - - warrior_governments: enforcing beneficiary (institutional/constrained) — mandates registration and confirmation hierarchies, draws order and legibility from the arrangement
 *   - - hereditary_shrine_lineages: primary target (moderate/identity_locked) — bears subordination of ancestral office and loss of interpretive authority over their own kami
 *   - - kami_worshipper_communities: dual-positioned participants (organized/constrained) — receive legitimation and salvagic depth, pay fees and surrender interpretive standing
 *   - - rival_kami_doctrine_schools: excluded challengers (moderate/trapped) — Watarai, Ise, Yoshida, and nativist lines kept at the margins of official adjudication
 *   - - religious_historians: analytical observers (analytical/analytical) — reconstruct the structure from archives; no seat holds authority over them
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.62).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.65).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "Honji Suijaku Syncretic Fusion Commitment (Syncretic Fusion Reading)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "religious/philosophical/historical").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '4d4af5d3-f641-4a65-85a7-3474bac4cf9a').
narrative_ontology:cs_kernel_codification('4d4af5d3-f641-4a65-85a7-3474bac4cf9a', formalized).
narrative_ontology:cs_authority_grounding('4d4af5d3-f641-4a65-85a7-3474bac4cf9a', lineage).
narrative_ontology:cs_interpretation_layer_present('4d4af5d3-f641-4a65-85a7-3474bac4cf9a').
narrative_ontology:cs_reading_relation('4d4af5d3-f641-4a65-85a7-3474bac4cf9a', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('4d4af5d3-f641-4a65-85a7-3474bac4cf9a', shinbutsu_coexistence_commitment__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('4d4af5d3-f641-4a65-85a7-3474bac4cf9a', foundational, kami_are_manifest_traces_of_buddhas).
narrative_ontology:cs_axiom_status(kami_are_manifest_traces_of_buddhas, holdable).
narrative_ontology:cs_axiom_grounding('4d4af5d3-f641-4a65-85a7-3474bac4cf9a', kami_are_manifest_traces_of_buddhas, theological).
narrative_ontology:cs_axiom('4d4af5d3-f641-4a65-85a7-3474bac4cf9a', secondary, shrine_cult_legitimacy_requires_buddhist_ground).
narrative_ontology:cs_axiom_status(shrine_cult_legitimacy_requires_buddhist_ground, holdable).
narrative_ontology:cs_axiom_grounding('4d4af5d3-f641-4a65-85a7-3474bac4cf9a', shrine_cult_legitimacy_requires_buddhist_ground, theological).
narrative_ontology:cs_reference_frame('4d4af5d3-f641-4a65-85a7-3474bac4cf9a', honji_suijaku_unified_ontology).
narrative_ontology:cs_drift_state('4d4af5d3-f641-4a65-85a7-3474bac4cf9a', meiji_separation_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('4d4af5d3-f641-4a65-85a7-3474bac4cf9a', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, esoteric_monastic_establishments).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, warrior_governments).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, hereditary_shrine_lineages).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kami_worshipper_communities).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kami_worshipper_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Great Tendai and Shingon centers such as Enryaku-ji and To-dai-ji author the doctrinal identification of kami with Buddhist originals, install resident clergy at major shrines, conduct esoteric rites there, and collect shares of shrine lands, offerings, and ritual fees. Their academies train the interpreters who decide what any given kami is. Leaving the arrangement would mean abandoning the shrine network their expansion built; they can and do reframe doctrine when pressure demands, shifting between Ryobu, Sanno, and newer formulations without surrendering the underlying claim.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, esoteric_monastic_establishments, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__syncretic_fusion_reading, esoteric_monastic_establishments, beneficiary).

% The Kamakura, Muromachi, and Tokugawa regimes confirm temple-shrine hierarchies, require households to hold registration at a Buddhist temple, and use the fused religious order to police belief and mobilize communities. They draw social order and administrative legibility from the arrangement while supplying the coercive backing that keeps shrine and temple obligations aligned.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, warrior_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__syncretic_fusion_reading, warrior_governments, beneficiary).

% Hereditary priestly houses serve particular kami across generations. Under the fused order their offices are subordinated to resident Buddhist clergy, their rites are framed as provisional expressions of Buddhist truth, and their administrative authority over shrine lands is curtailed. Their identity is bound to their kami and to the lineage office itself; abandoning the office is unthinkable, and the authoritative account of what their kami means is authored elsewhere.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, hereditary_shrine_lineages, payer,
    moderate, generational, identity_locked, national).

% Village and town communities keep festival calendars, fund rites, and rely on shrines for this-worldly protection and communal cohesion. The fused order gives their practices salvific depth and institutional protection, while requiring them to route legitimacy through temple certification and to pay for Buddhist services attached to their festivals and funerals.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kami_worshipper_communities, beneficiary,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kami_worshipper_communities, payer).

% Watarai and Ise ritual houses, Yoshida Shinto masters, and later nativist scholars propose accounts in which kami stand on their own ground or outrank the Buddhas. They publish, litigate, and petition, but the fused order controls the institutions through which kami-meaning is officially adjudicated, so their proposals circulate at the margins or get absorbed as additional interpretive layers rather than displacing the frame.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, rival_kami_doctrine_schools, excluded,
    moderate, generational, trapped, national).

% Modern scholars reconstruct the arrangement from shrine and temple records, doctrinal treatises, and state archives. They hear from every surviving position and none holds authority over them; their reconstructions feed public understanding, heritage policy, and the historiographic dispute over whether the arrangement was ever coherent.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__syncretic_fusion_reading, esoteric_monastic_establishments).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__syncretic_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates indigenous kami cults and the pan-East-Asian Buddhist salvation economy into a single legitimate ritual order: it settles who may conduct rites at shrines, links shrines into protective temple networks, gives communities one soteriological frame covering both this-worldly benefits and the afterlife, and makes kami worship legible within the Buddhist legal and cosmological order that dominated East Asia.
% TRANSFER_FUNCTION: Moves administrative control of shrines, shares of shrine lands, offerings, and ritual fees from hereditary shrine lineages and worshipping communities to Buddhist monastic establishments; moves salvific assurance, textual prestige, and institutional protection back toward lineages and communities.
% ABSENT_VOICES: The rival kami-doctrine schools are structurally excluded: Watarai and Ise ritualists, Yoshida masters, and nativist scholars would insist kami need no Buddhist ground, but the fused order owns the adjudicating institutions. Hereditary shrine lineages are present yet subordinated, so their objections reach the record chiefly through the very frame that subordinates them. The communities funding the system appear mostly as payers in temple ledgers rather than as voices on what their kami mean.
% DISAPPEARANCE_RATIONALE: It did rearrange, historically: when the 1868 edicts severed kami from Buddhas, thousands of shrine-temples dissolved, a wave of anti-Buddhist destruction closed or defunded large numbers of temples, Hachiman's bodhisattva title was revoked, shrine lineages abruptly reacquired offices their grandfathers had lost, and the ritual calendar reorganized around a legally purified Shinto within a generation. An arrangement whose removal reorganizes a civilization's religious order that quickly is one the world was arranged around.
% FOUNDING_PROBLEM: Medieval Japan inherited indigenous kami cults alongside a comprehensive Buddhist salvation economy backed by state power and continental prestige. Someone had to answer whether kami worship was compatible with Buddhist truth, who could adjudicate that compatibility, and on what terms shrines could operate inside a Buddhist legal order. The fusion arrangement was built to settle those questions by declaring kami to be local traces of universal Buddhist originals.
% FOUNDING_PROBLEM_CORROBORATION: No beneficiary party survives to attest anything. External corroboration: the 1868 Dajokan separation edicts and the anti-Buddhist destruction records attest both the arrangement's existence and its forcible termination; Kuroda Toshio's kenmitsu-system scholarship and the religious historiography following it reconstruct the founding problem from temple and shrine archives independent of any benefiting party; surviving jinguji registers and doctrinal corpora corroborate the institutional shape. The problem died by state fiat rather than doctrinal solution — the edicts abolished the question rather than answering it.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__syncretic_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.62 because substantial asymmetric flows (shrine lands, offerings, ritual fees, administrative precedence) ride on top of services the temple side genuinely provides (rites, protection, textual legitimacy). Suppression is 0.65 and unscaled by design — it is a raw structural property: the arrangement persisted through institutional control of shrine administration and, by the Edo period, state-compelled household registration, not through participant preference alone; only extractiveness gets scaled by directionality and scope in the engine's computation. Theater ratio is 0.36: a growing share of late-period activity is ceremonial maintenance (precedence disputes, routine certifications) while core functions remain load-bearing. Accessibility collapse is 0.48 — alternative kami-theologies survived at the margins (Watarai, Yoshida inversion, nativist critique) and folk practice ran beneath the doctrine, so alternatives collapsed only partially. Resistance is 0.42 — sustained litigation, rival schools, and nativist movements met real but contained opposition. The measurement series run on one shared time grid (all three metrics authored at all eight points) so no metric inherits another's end-state values. The suppression_requirement series is authored deliberately: enforcement capacity hardened over the interval — from doctrinal persuasion, through shrine-temple institutionalization, to bakufu-mandated registration — which is precisely the enforcement-infrastructure maturation the temporal tracker exists to catch.
 *
 * PERSPECTIVAL GAP:
 *   The monastic seat experiences the arrangement as the dharma's natural completeness — the kami finally understood correctly, their cult perfected rather than displaced. The shrine-lineage seat experiences the identical structure as dispossession: their ancestral kami's meaning authored by outsiders, their offices demoted beneath resident clergy. The community seat experiences both at once — legitimation and salvific assurance arriving together with fees and compulsory Buddhist services. Same structure, three different lived arrangements; the engine computes per-seat classifications from the power, exit, and role data rather than from any authored verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Esoteric monastic establishments and warrior governments are declared beneficiaries: derivation places them near the beneficiary end of directionality, damping their effective extraction toward subsidy — the arrangement pays them. Hereditary shrine lineages are the declared victim group with identity_locked exit: they sit near the full-target end, amplifying effective extraction, because they cannot abandon the office that binds them to the frame. Worshipper communities sit near symmetric with a mild beneficiary tilt — genuine legitimation received, diffuse costs paid. Rival doctrine schools are excluded rather than coordinated: their exclusion is the enforcement object itself, and they bear the arrangement's costs without holding a seat. Scope is national-to-continental, which modestly amplifies effective extraction on the target side through verification difficulty.
 *
 * MANDATROPHY ANALYSIS:
 *   The arrangement never atrophied into performance while alive — it was killed at high functionality by external revolutionary force, which is why no mandatrophy declaration is authored despite the late theater-ratio rise: the growing ceremonialism of the Edo period sat atop registrations, rites, and doctrinal adjudications that remained load-bearing until the edicts landed. The classification discipline matters here in both directions: a pure-coordination reading would erase the shrine lineages' subordination and the resource flows to the temple side; a pure-extraction reading would erase the genuine integration that communities and lineages experienced as salvific gain. Tangled_rope keeps both faces visible. The founding problem died by state fiat rather than internal obsolescence, and the historical record of the Meiji rearrangement independently confirms the disappearance verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is the syncretic_fusion_reading of the shinbutsu_coexistence_commitment kernel; what structurally changes if the domain_partition_reading is instantiated instead?',
    'Classify the sibling story files and compare victim sets, epsilon, and per-seat classifications across the kernel family.',
    'Under the partition reading the shrine lineages lose their doctrinal-subordination grievance (no unification claim subordinate them), epsilon drops materially, and the arrangement trends toward a lighter coordination structure with different enforcement needs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: what the sibling partition reading would change structurally.').

omega_variable(
    kernel_coherence_dispute,
    'Was the fusion arrangement a single coherent ontology, as this reading holds, or an incoherent bundle sustained by ambiguity and institutional power, as the bundle reading holds?',
    'Doctrinal-consistency stress tests across the Ryobu, Sanno, and Yoshida textual corpora, weighed against Kuroda Toshio''s kenmitsu-system historiography and its critics.',
    'If the bundle reading prevails, this constraint decomposes into weakly coupled micro-constraints with divergent seats, and the tangled_rope classification fragments into per-component verdicts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_coherence_dispute, conceptual, 'Whether the kernel is one ontology or a managed bundle.').

omega_variable(
    consent_or_subordination,
    'Did hereditary shrine lineages experience the fused order as legitimate self-understanding or as imposed subordination?',
    'Shrine diaries, litigation records over shrine-temple control, and petition archives spanning the Heian through Edo periods.',
    'Predominantly consensual experience pulls effective extraction down toward coordination cost; predominantly imposed experience pushes the arrangement toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_or_subordination, empirical, 'Consent versus imposition among the subordinated seat.').

omega_variable(
    suppression_mechanism_split,
    'Was the subordination of shrine lineages maintained by structural enforcement (temple control, state registration) or by internalized identity fusion with the Buddhist-framed kami?',
    'Post-1868 trajectory: lineages that embraced separation rapidly once enforcement lifted indicate structural maintenance; lineages that resisted or mourned the fused order indicate internalized components.',
    'If substantially internalized, the measured suppression understates the arrangement''s hold — the subordination traveled with the lineages after the edicts; if structural, removal should have released them quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized maintenance of subordination.').

omega_variable(
    hierarchy_direction_ambiguity,
    'Is the arrangement''s essence the fusion frame itself (any single-ontology hierarchy) or specifically Buddhist supremacy over kami? The Yoshida inversion ran comparable machinery in reverse.',
    'Compare extraction patterns under Ryobu/Sanno supremacy versus Yoshida-era inversion: if beneficiaries simply swap, the frame is the constant; if flows track Buddhist institutions specifically, supremacy is the constant.',
    'Changes who counts as beneficiary across the interval and whether the Meiji separation targeted the frame or the hierarchy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hierarchy_direction_ambiguity, conceptual, 'Whether the extractive constant is the fusion frame or Buddhist supremacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 1090, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t1090, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1090, 0.1).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1200, 0.16).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t1350, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1350, 0.21).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t1500, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1500, 0.24).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t1600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1600, 0.29).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t1700, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1700, 0.32).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t1800, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1800, 0.35).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_tr_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1868, 0.36).

% Extraction over time
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t1090, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1090, 0.38).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1200, 0.47).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t1350, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1350, 0.56).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t1500, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1500, 0.63).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t1600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1600, 0.59).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t1700, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1700, 0.6).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t1800, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1800, 0.61).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_be_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1868, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t1090, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1090, 0.22).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1200, 0.33).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t1350, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1350, 0.42).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t1500, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1500, 0.46).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t1600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1600, 0.53).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t1700, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1700, 0.58).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t1800, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1800, 0.62).
narrative_ontology:measurement(shinbutsu_syncretic_fusion_su_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1868, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__syncretic_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'shinbutsu shugo' conflates three structurally distinct claims and is decomposed into a constraint family: (1) ontological unification via honji suijaku (this file — high doctrinal constraint, elite interpretive authority, epsilon 0.62, tangled_rope); (2) domain partition without unification (sibling — different victim set, lower epsilon, different failure modes); (3) the meta-claim that no coherent kernel existed (sibling — decomposes further into micro-constraints). This reading is the classical upstream claim; the bundle reading is downstream historiographic skepticism that cites this claim's forcible collapse as evidence. Each member carries its own epsilon, stakeholders, and classification; all are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
