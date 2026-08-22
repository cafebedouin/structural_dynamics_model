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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Honji Suijaku Syncretic Fusion Commitment (Kami as Local Manifestations of Buddhist Truth)
 *   domain: religious/philosophical/historical
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the syncretic fusion reading — of
 *   the contested shinbutsu coexistence kernel: the commitment, articulated
 *   by Tendai and Shingon exegetes from roughly the tenth century onward,
 *   that kami and Buddhas are ontologically one, the kami being local traces
 *   (suijaku) of universal Buddhist truth (honji). The reading generates a
 *   single coherent religious cosmos with high demands on doctrinal
 *   consistency, an exegetical elite as interpretive authority, and the
 *   shrine-temple complex (jinguji) as its institutional embodiment.
 *   Operationally the arrangement solved a real integration problem — two
 *   authoritative cultic systems sharing sacred sites, patrons, and calendars
 *   for nine centuries without open confessional war — while encoding a
 *   hierarchy: kami worship counted fully only under Buddhist interpretation,
 *   shrine offices fell under monk-administrators, and revenue and labor
 *   flowed through joint complexes the monastic centers controlled. The claim
 *   and the metrics are authored independently: the reading presents itself
 *   as metaphysical truth rather than policy, while the authored metrics
 *   describe a moderately-to-substantially extractive, actively enforced
 *   arrangement whose maintenance grew increasingly ceremonial in its final
 *   centuries. The sibling readings are separate constraint files, not part
 *   of this one. KEY AGENTS (by structural relationship): -
 *   buddhist_monastic_establishment: agenda-setter and principal collector
 *   (institutional/arbitrage) — formulates the ontology, staffs and
 *   administers the shrine-temples, adjudicates what the kami are -
 *   hereditary_shrine_priest_lineages: primary bearer of costs
 *   (organized/constrained) — hereditary custodians whose rites require
 *   Buddhist framing and whose shrines sit under temple administration -
 *   court_aristocracy: secondary beneficiary (powerful/mobile) — receives a
 *   unified ritual order legitimating court rule -
 *   affiliated_shrine_communities: dual-positioned local seat
 *   (powerless/trapped) — receives combined rites, pays dues and labor to the
 *   joint complex - independent_kami_cult_practitioners: secondary bearer of
 *   costs (powerless/trapped) — ritualists outside the affiliated complexes
 *   whose practices are legible only as provisional -
 *   ise_grand_shrine_priesthood: excluded dissenting seat
 *   (powerful/identity_locked) — refuses Buddhist presence outright and sits
 *   outside the consensus machinery - medieval_religious_historians:
 *   analytical observer — reconstructs the arrangement from documents without
 *   participating
 *
 * KEY AGENTS:
 *   - buddhist_monastic_establishment: agenda-setter and principal collector (institutional power, arbitrage-grade position)
 *   - hereditary_shrine_priest_lineages: primary bearer of costs (organized power, constrained exit)
 *   - court_aristocracy: secondary beneficiary (powerful, mobile)
 *   - affiliated_shrine_communities: dual-positioned local seat (powerless, trapped)
 *   - independent_kami_cult_practitioners: secondary bearer of costs (powerless, trapped)
 *   - ise_grand_shrine_priesthood: excluded dissenting seat (powerful, identity-locked)
 *   - medieval_religious_historians: analytical observer (analytical power, analytical exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.68).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.6).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "Honji Suijaku Syncretic Fusion Commitment (Kami as Local Manifestations of Buddhist Truth)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "religious/philosophical/historical").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '317b2615-6f5f-436a-8710-339f84718aa0').
narrative_ontology:cs_kernel_codification('317b2615-6f5f-436a-8710-339f84718aa0', formalized).
narrative_ontology:cs_authority_grounding('317b2615-6f5f-436a-8710-339f84718aa0', lineage).
narrative_ontology:cs_interpretation_layer_present('317b2615-6f5f-436a-8710-339f84718aa0').
narrative_ontology:cs_reading_relation('317b2615-6f5f-436a-8710-339f84718aa0', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('317b2615-6f5f-436a-8710-339f84718aa0', shinbutsu_coexistence_commitment__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('317b2615-6f5f-436a-8710-339f84718aa0', foundational, kami_are_buddha_manifestations).
narrative_ontology:cs_axiom_status(kami_are_buddha_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('317b2615-6f5f-436a-8710-339f84718aa0', kami_are_buddha_manifestations, theological).
narrative_ontology:cs_axiom('317b2615-6f5f-436a-8710-339f84718aa0', foundational, kami_salvation_requires_buddhist_framing).
narrative_ontology:cs_axiom_status(kami_salvation_requires_buddhist_framing, holdable).
narrative_ontology:cs_axiom_grounding('317b2615-6f5f-436a-8710-339f84718aa0', kami_salvation_requires_buddhist_framing, instrumental).
narrative_ontology:cs_axiom('317b2615-6f5f-436a-8710-339f84718aa0', secondary, buddhist_rite_precedence_at_shared_sites).
narrative_ontology:cs_axiom_status(buddhist_rite_precedence_at_shared_sites, holdable).
narrative_ontology:cs_axiom_grounding('317b2615-6f5f-436a-8710-339f84718aa0', buddhist_rite_precedence_at_shared_sites, conventional).
narrative_ontology:cs_reference_frame('317b2615-6f5f-436a-8710-339f84718aa0', honji_suijaku_unified_cosmos).
narrative_ontology:cs_drift_state('317b2615-6f5f-436a-8710-339f84718aa0', contemporary_post_bunri, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('317b2615-6f5f-436a-8710-339f84718aa0', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_monastic_establishment).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, court_aristocracy).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, affiliated_shrine_communities).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, hereditary_shrine_priest_lineages).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, independent_kami_cult_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, affiliated_shrine_communities).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, honji_suijaku_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, dharmakaya_manifestation_theory).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, non_dual_esoteric_metaphysics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Tendai and Shingon centers formulate the unification doctrine, train the exegetes who certify what a given kami is, appoint the monk-administrators who run the shrine-temples, and receive the revenues, labor obligations, and land stewardship that flow through the joint complexes. They bear the cost of maintaining doctrinal consistency across hundreds of sites. Their position spans the whole system: they can reinterpret challenges, absorb rival lineages, and relocate personnel, so no single site's loss threatens them.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_monastic_establishment, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_monastic_establishment, beneficiary).

% Hereditary custodial families keep the shrines, perform the kami rites, and hold local standing tied to ancestral office. Under the arrangement their rites count fully only when framed in Buddhist terms, their shrines commonly fall under monk-administrators, and offerings and land income pass through joint complexes they serve but do not control. Leaving would mean abandoning the ancestral office, the community role, and the shrine itself; staying means serving under a scheme that ranks their deities below the Buddhas.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, hereditary_shrine_priest_lineages, payer,
    organized, generational, constrained, regional).

% The court and the great houses patronize both sides and receive a single ritual order: one calendar, one set of intercessory institutions, and a cosmology in which imperial descent and universal truth do not collide. They fund complexes, arbitrate disputes, and can shift favor between temples and shrines, which keeps their position comfortable whichever way doctrinal fashion moves.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, court_aristocracy, beneficiary,
    powerful, generational, mobile, national).

% Villages and towns attached to the shrine-temples receive the combined package — harvest and purification rites from the shrine side, funerals and memorial services from the temple side — and pay dues, corvee labor, and festival costs to the joint complex. There is no alternative religious infrastructure within reach; participation is not optional in practice.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, affiliated_shrine_communities, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__syncretic_fusion_reading, affiliated_shrine_communities, payer).

% Oracle-masters, mountain ascetics outside the major lineages, and local ritualists operating apart from the temple-affiliated complexes. Their practices are legible within the dominant scheme only as provisional or vulgar unless they affiliate; they lack the corporate standing to negotiate terms, and the affiliation on offer absorbs their independence.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, independent_kami_cult_practitioners, payer,
    powerless, biographical, trapped, local).

% The priestly houses of the Grand Shrine of Ise refuse the arrangement outright: no temple within the precincts, no Buddhist funeral rites for shrine officials, purification-only ritual. Their objections are on the historical record, but they hold no seat in the exegetical machinery that certifies doctrine. The meaning of their office — service to a deity whose purity excludes Buddhist presence — makes accommodation unthinkable from where they stand.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, ise_grand_shrine_priesthood, excluded,
    powerful, civilizational, identity_locked, national).

% Modern scholars reconstruct the arrangement from doctrinal treatises, engi narratives, land and tax records, and litigation archives. They take no part in its operation and bear neither its costs nor its receipts; their stake is analytic.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, medieval_religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_monastic_establishment).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__syncretic_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrated an imported salvation religion and an indigenous cultic system into one ritual-cosmological order: shared sacred sites, a common festival calendar, pooled patronage, and a single account of what the enshrined powers are — problems otherwise worked out (or not) through open rivalry between two authorities each claiming final say over ritual meaning.
% TRANSFER_FUNCTION: Moved interpretive authority over the kami — and with it ritual precedence, administrative control of the shrine-temples, and much of the associated land revenue and labor obligations — from hereditary shrine lineages to the Buddhist clerical hierarchy, while moving legitimation toward the court's unified ritual regime.
% ABSENT_VOICES: The Ise priesthood and the purist shrine lineages held a partition view and objected, but sat outside the doctrinal consensus machinery that produced and certified the fusion — their objections survive as protest records, not as votes. Non-elite worshippers were described by the doctrine far more than consulted by it. The kami themselves, the arrangement's ostensible subjects, speak only through the exegetes who interpreted them.
% DISAPPEARANCE_RATIONALE: Overnight removal would unwind the shrine-temple complexes, sever Buddhist rites from hundreds of shrines, fragment the court's unified ritual calendar, strip kami worship of its salvific framing, and return the two systems to open competition for sites, patrons, and doctrinal final say — approximately the reorganization the Meiji separation edicts actually produced, amid considerable violence, beginning in 1868.
% FOUNDING_PROBLEM: Two authoritative systems — an imported religion claiming universal truth and an indigenous cult carrying imperial political legitimacy — occupied the same sacred geography, each with final say over ritual meaning; some working settlement was needed that avoided zero-sum conflict between them.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Meiji government's separation edicts (1868-1871) closed the problem by state fiat rather than leaving it to the arrangement's beneficiaries; kokugaku scholars (Motoori Norinaga, Hirata Atsutane) had already argued, from outside the monastic establishment, that the integration problem was in fact a contamination problem; modern historiography (notably Kuroda Toshio's reassessment of medieval religiosity) confirms the settlement tracked institutional power as much as doctrine. No party outside the arrangement's beneficiaries attests that the founding problem remains live in its original form.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__syncretic_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   The metric profile describes an arrangement with a real coordination core and a real extraction overlay. Extractiveness (0.68 at interval end, rising from 0.45) tracks the consolidation of temple administration over shrines: the betto appointment system, jinguji land stewardship, and the doctrinal rule that kami worship is complete only under Buddhist interpretation moved interpretive authority and material flows toward the monastic centers. Suppression (0.60) is authored as a raw structural property and is deliberately not scaled by anything in this file — it reflects the enforcement work that doctrinal consistency required (policing kami-superiority claims, absorbing or marginalizing purist lineages), not any contextual amplifier. Theater ratio rises from 0.15 to 0.50: early formulation did heavy integrative work, while by the late Tokugawa period much maintenance consisted of rote citation of a doctrine whose intellectual vitality had migrated to kokugaku and Confucian critique — classic proxy-goal drift. Accessibility collapse (0.62) is high but incomplete: inside the framework an independent kami theology is hard to state coherently, yet Ise and later the nativist schools demonstrated that exit and counter-frameworks remained constructible. Resistance (0.55) is correspondingly substantial and persistent. The three temporal series share one grid (900, 1100, 1300, 1500, 1700, 1868) so no metric is ever sampled against another's end-state; the suppression series is non-monotonic by design (Sengoku disruption briefly strained enforcement capacity before Tokugawa-era administrative settling restored it). Coalition note: the payer lineages periodically coordinated — petitions against unwanted monk-administrators, joint protests in the Kasuga-Ise disputes — but fragmentation by local interest kept coalition power episodic rather than systemic.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes divergent per-seat classifications from the structural data, and the seats genuinely diverge. From the monastic seat the arrangement is revealed truth plus legitimate administration; from the shrine-lineage seat the same structure is subordination dressed as reverence — their deities ranked as traces, their offices made derivative, their incomes routed through complexes they serve but do not command; from the court seat it is administrative convenience; from the Ise seat it is contamination to be refused at any price. No authored claim adjudicates between these; the divergence is the datum the corpus exists to record.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the monastic establishment (formulates, administers, collects — nearest the beneficiary pole despite bearing real coordination costs), the court (receives legitimation, pays little, can shift patronage), and incidentally the affiliated communities (receive the combined rite package, pay dues — near symmetric). Victim declarations drive high directionality for the hereditary shrine lineages (lose interpretive authority and administrative control, constrained exit) and the independent practitioners (lose standing entirely, no exit). No directionality overrides are authored: the derivation chain reads the declared structure correctly, and an override would be unsafe here because the court and the Ise priesthood share the same power atom while standing at opposite poles — an atom-keyed override would corrupt one seat or the other.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than snare preserves the genuine coordination achievement: two rival authoritative systems shared sites, patrons, and calendars for nine centuries without the confessional violence visible elsewhere in the period — that is not cover-story coordination. Classifying it as tangled_rope rather than rope preserves the encoded hierarchy: the ontology itself assigned the kami derivative status, and the institutional embodiment moved resources and precedence upward. The late-interval theater rise suggests drift toward inertial maintenance, but the arrangement was terminated by external state coercion (the Meiji separation edicts), not by internal atrophy resolving its own mandate — so mandatrophy is left unresolved here, and the R5 record (a dead founding problem attached to a world-rearranging disappearance) carries the obsolescence signal for the consumer instead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the honji suijaku fusion the operative structure of the shinbutsu_coexistence_commitment kernel, or do the sibling readings describe the commitment better — domain_partition_reading (kami and Buddhas govern separate existential domains without ontological unification) or incoherent_bundle_reading (the commitment was never a coherent kernel but an incoherent bundle held together by deliberate ambiguity and institutional power)?',
    'Comparative philology: test whether doctrinal treatises (Ryobu and Sanno ichijitsu texts) predict administrative and liturgical practice better than a partition model or an ambiguity model, using shrine-temple litigation records, appointment rolls, and rite schedules as the practice-side evidence.',
    'Under domain_partition_reading the victim set shifts (shrine lineages lose less autonomy, the monastic elite extracts less through the ontology itself), epsilon falls and the classification trends toward rope. Under incoherent_bundle_reading the constraint loses its doctrinal-consistency character entirely and trends toward piton — theatrical maintenance around an empty center.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer-frame omega: this story is one reading of the shinbutsu coexistence kernel; sibling readings would change the victim set, epsilon, and classification.').

omega_variable(
    ontological_claim_status,
    'Is the unity assertion a metaphysical truth-claim about the structure of reality (as the reading''s holders insisted) or a constructed institutional arrangement that benefits identifiable parties?',
    'Philosophical assessment of the doctrine''s arguments independently of institutional history, plus comparative study of whether equivalent unification doctrines reliably arise wherever an imported salvation religion meets an established cultic substrate.',
    'If treated as constructed, the arrangement forfeits any natural-law immunity and faces a full extraction accounting with named beneficiaries; if a genuine truth-claim, part of its binding force is epistemic rather than institutional, and the extraction ledger must net out what the truth (if true) is worth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_claim_status, conceptual, 'Natural-law versus constructed-arrangement ambiguity in the fusion ontology.').

omega_variable(
    extraction_baseline_ambiguity,
    'How much of the measured flow toward the monastic centers reflects this ontological commitment specifically, versus the general medieval concentration of land, labor, and revenue in aristocratic and religious corporations of every kind?',
    'Compare revenue and land flows of temple-affiliated versus unaffiliated shrines of similar size and region, controlling for general estate (shoen) economics across the same period.',
    'If most of the flow is baseline medieval economics, the epsilon attributable to the fusion commitment falls materially and the classification softens toward rope; if the doctrine-specific differential is large, the extraction is properly charged to this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_baseline_ambiguity, empirical, 'Doctrine-specific extraction versus ambient medieval institutional economics.').

omega_variable(
    kami_agency_in_subordination,
    'Were the shrine lineages passive bearers of the arrangement''s costs, or strategic participants who traded interpretive subordination for resources, protection, and prestige?',
    'Close reading of shrine-side documents — engi compositions, petition letters, appointment negotiations — for evidence of strategic adoption versus coerced compliance.',
    'If participation was substantially strategic, the victim declaration weakens for that seat, effective extraction falls, and the arrangement looks more like a mutually negotiated bargain; if coerced, the current victim structure stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kami_agency_in_subordination, empirical, 'Victim agency versus imposed subordination in the shrine lineages.').

omega_variable(
    subordination_internalization,
    'Was shrine-lineage acquiescence structural (administrative and economic dependence on the temple complexes) or internalized (genuine acceptance of the kami-as-trace identity in the lineages'' own self-understanding)?',
    'Post-separation trajectory: after 1868 removed the enforcement machinery, did former shrine lineages revert rapidly to independent kami-centered identities, or did Buddhist-framed self-understanding persist across generations?',
    'If internalized, the arrangement''s hold exceeded its enforcement capacity and the structural suppression measure understates its real grip; if structural, removal should have sufficed — which, broadly, it did.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subordination_internalization, empirical, 'Structural versus internalized mechanism of shrine-lineage compliance.').

omega_variable(
    latent_vitality_at_termination,
    'Would the arrangement have persisted without the Meiji state''s coercion, or had its function already atrophied to the point of impending self-collapse?',
    'Analysis of late-Tokugawa institutional indicators: new jinguji foundations, doctrinal publication volume, elite recruitment into exegetical lines versus conversion of educated clergy to kokugaku and Confucian studies.',
    'If already moribund, the rising theater trajectory pointed at an inertial endpoint and the Meiji destruction merely accelerated an internal death; if vital, the arrangement was destroyed alive, which strengthens the tangled_rope reading and locates the failure wholly in political coercion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(latent_vitality_at_termination, empirical, 'Whether the 1868 termination cut a living arrangement or finished a dying one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 900, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_fusion_tr_t900, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 900, 0.15).
narrative_ontology:measurement_basis(shinbutsu_fusion_tr_t900, observed).
narrative_ontology:measurement(shinbutsu_fusion_tr_t1100, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1100, 0.2).
narrative_ontology:measurement_basis(shinbutsu_fusion_tr_t1100, observed).
narrative_ontology:measurement(shinbutsu_fusion_tr_t1300, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1300, 0.28).
narrative_ontology:measurement_basis(shinbutsu_fusion_tr_t1300, observed).
narrative_ontology:measurement(shinbutsu_fusion_tr_t1500, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1500, 0.35).
narrative_ontology:measurement_basis(shinbutsu_fusion_tr_t1500, observed).
narrative_ontology:measurement(shinbutsu_fusion_tr_t1700, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1700, 0.43).
narrative_ontology:measurement_basis(shinbutsu_fusion_tr_t1700, observed).
narrative_ontology:measurement(shinbutsu_fusion_tr_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1868, 0.5).
narrative_ontology:measurement_basis(shinbutsu_fusion_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(shinbutsu_fusion_be_t900, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 900, 0.45).
narrative_ontology:measurement_basis(shinbutsu_fusion_be_t900, observed).
narrative_ontology:measurement(shinbutsu_fusion_be_t1100, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1100, 0.55).
narrative_ontology:measurement_basis(shinbutsu_fusion_be_t1100, observed).
narrative_ontology:measurement(shinbutsu_fusion_be_t1300, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1300, 0.63).
narrative_ontology:measurement_basis(shinbutsu_fusion_be_t1300, observed).
narrative_ontology:measurement(shinbutsu_fusion_be_t1500, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1500, 0.66).
narrative_ontology:measurement_basis(shinbutsu_fusion_be_t1500, observed).
narrative_ontology:measurement(shinbutsu_fusion_be_t1700, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1700, 0.67).
narrative_ontology:measurement_basis(shinbutsu_fusion_be_t1700, observed).
narrative_ontology:measurement(shinbutsu_fusion_be_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1868, 0.68).
narrative_ontology:measurement_basis(shinbutsu_fusion_be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_fusion_su_t900, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 900, 0.3).
narrative_ontology:measurement_basis(shinbutsu_fusion_su_t900, observed).
narrative_ontology:measurement(shinbutsu_fusion_su_t1100, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1100, 0.42).
narrative_ontology:measurement_basis(shinbutsu_fusion_su_t1100, observed).
narrative_ontology:measurement(shinbutsu_fusion_su_t1300, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1300, 0.52).
narrative_ontology:measurement_basis(shinbutsu_fusion_su_t1300, observed).
narrative_ontology:measurement(shinbutsu_fusion_su_t1500, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1500, 0.5).
narrative_ontology:measurement_basis(shinbutsu_fusion_su_t1500, observed).
narrative_ontology:measurement(shinbutsu_fusion_su_t1700, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1700, 0.56).
narrative_ontology:measurement_basis(shinbutsu_fusion_su_t1700, observed).
narrative_ontology:measurement(shinbutsu_fusion_su_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1868, 0.6).
narrative_ontology:measurement_basis(shinbutsu_fusion_su_t1868, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__syncretic_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'shinbutsu shugo' conflates three structurally distinct claims, decomposed per the epsilon-invariance principle into a three-story family: this file (syncretic fusion — ontological unity asserted, epsilon 0.68, tangled_rope), the domain partition reading (functional separation without unification — different victim structure, lower epsilon), and the incoherent bundle reading (no coherent kernel — near-zero doctrinal epsilon, theatrical maintenance). The fusion reading is the upstream elite articulation: its treatises were cited as warrant for the institutional arrangements the other readings describe or deny, so its edges point at both siblings. Each file authors its own epsilon, beneficiaries, and victims; nothing in this file averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
