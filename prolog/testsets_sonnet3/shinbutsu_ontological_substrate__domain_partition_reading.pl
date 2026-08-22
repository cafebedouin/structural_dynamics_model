% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__domain_partition_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__domain_partition_reading
 *   human_readable: Kami-Buddha Domain Partition (Functional Coexistence Reading)
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the domain_partition_reading of the
 *   shinbutsu_ontological_substrate kernel: kami and buddhas are held to
 *   govern genuinely separate domains — kami the affairs of this world
 *   (harvest, protection, community welfare), buddhas the affairs of the
 *   afterlife (salvation, funerary rites) — and their long coexistence in
 *   Japanese religious practice (jingu-ji shrine-temple complexes, joint
 *   festivals, shared sacred sites) is read as a pragmatic division of
 *   institutional labor rather than evidence of any deeper metaphysical
 *   unity. Under this reading, extraction and suppression are both low: no
 *   institution needs to dominate or absorb the other, and communities draw
 *   freely on both without doctrinal conflict. This is one of three linked
 *   constraints reading the same underlying phenomenon; the
 *   syncretic_fusion_reading treats the same coexistence as evidence of
 *   genuine ontological unification (honji suijaku as metaphysical truth),
 *   and the incoherent_bundle_reading treats it as unstable institutional
 *   drift later corrected by Meiji-era state force. The three share a
 *   referent (the pre-Meiji shrine-temple coexistence) but author different
 *   epsilon, different beneficiary structures, and different classifications,
 *   per the epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - shrine_priesthoods: beneficiary/agenda_setter (organized/constrained) — retain this-world ritual jurisdiction
 *   - temple_clergy: beneficiary/agenda_setter (organized/constrained) — retain afterlife ritual jurisdiction
 *   - local_agricultural_communities: beneficiary (powerless/constrained) — draw on both domains as needed
 *   - itinerant_ritual_specialists: beneficiary (moderate/mobile) — livelihood depends on domain separability
 *   - honji_suijaku_theorists: excluded — hold the sibling fusion reading, not represented here
 *   - meiji_era_separatists: excluded — hold the sibling incoherent-bundle reading, not represented here
 *   - historians_of_japanese_religion: observer (analytical) — study the underlying kernel contest itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__domain_partition_reading, 0.28).
domain_priors:suppression_score(shinbutsu_ontological_substrate__domain_partition_reading, 0.22).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__domain_partition_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__domain_partition_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__domain_partition_reading, "Kami-Buddha Domain Partition (Functional Coexistence Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__domain_partition_reading, "religious_studies/japanese_history/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__domain_partition_reading, 'a4071d51-93ad-4031-95c6-87816004c380').
narrative_ontology:cs_kernel_codification('a4071d51-93ad-4031-95c6-87816004c380', distributed).
narrative_ontology:cs_authority_grounding('a4071d51-93ad-4031-95c6-87816004c380', practice).
narrative_ontology:cs_interpretation_layer_present('a4071d51-93ad-4031-95c6-87816004c380').
narrative_ontology:cs_reading_relation('a4071d51-93ad-4031-95c6-87816004c380', shinbutsu_ontological_substrate__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('a4071d51-93ad-4031-95c6-87816004c380', shinbutsu_ontological_substrate__incoherent_bundle_reading, influences).
narrative_ontology:cs_axiom('a4071d51-93ad-4031-95c6-87816004c380', foundational, domains_are_functionally_not_ontologically_distinct).
narrative_ontology:cs_axiom_status(domains_are_functionally_not_ontologically_distinct, holdable).
narrative_ontology:cs_axiom_grounding('a4071d51-93ad-4031-95c6-87816004c380', domains_are_functionally_not_ontologically_distinct, conventional).
narrative_ontology:cs_axiom('a4071d51-93ad-4031-95c6-87816004c380', secondary, coexistence_requires_no_metaphysical_reconciliation).
narrative_ontology:cs_axiom_status(coexistence_requires_no_metaphysical_reconciliation, holdable).
narrative_ontology:cs_axiom_grounding('a4071d51-93ad-4031-95c6-87816004c380', coexistence_requires_no_metaphysical_reconciliation, instrumental).
narrative_ontology:cs_reference_frame('a4071d51-93ad-4031-95c6-87816004c380', dual_institution_functional_specialization).
narrative_ontology:cs_drift_state('a4071d51-93ad-4031-95c6-87816004c380', late_edo_period, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('a4071d51-93ad-4031-95c6-87816004c380', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__domain_partition_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, shrine_priesthoods).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, temple_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, local_agricultural_communities).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__domain_partition_reading, itinerant_ritual_specialists).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, domain_specialization_doctrine).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__domain_partition_reading, functional_pluralism_coherence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer kami rites tied to harvest, community welfare, and this-worldly protection. Under the domain-partition reading, their jurisdiction (this-world, present flourishing) is structurally distinct from and non-competing with Buddhist soteriological claims, so they retain undiminished ritual authority over their sphere without needing to argue kami and buddhas are the same being.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, shrine_priesthoods, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__domain_partition_reading, shrine_priesthoods, agenda_setter).

% Administer funerary rites, afterlife liturgy, and soteriological doctrine. Their domain (the afterlife, salvation) is functionally separate from kami jurisdiction, so they can practice alongside shrines at the same sites without doctrinal collision — coexistence is a working division of labor, not a claim that their deity and the kami are one substance.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, temple_clergy, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__domain_partition_reading, temple_clergy, agenda_setter).

% Engage kami ritual for harvest and this-worldly protection and Buddhist ritual for death and afterlife concerns, drawing on both without needing either institution to explain how the two relate metaphysically. The domain-partition reading matches their lived practice: different problems, different specialists, no felt contradiction.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, local_agricultural_communities, beneficiary,
    powerless, biographical, constrained, local).

% Move between shrine and temple contexts performing whichever rite a situation calls for. Their livelihood depends on the two domains staying practically separable — if either institution insisted the domains be unified or hierarchically subordinated, cross-domain practice would narrow their available work.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, itinerant_ritual_specialists, beneficiary,
    moderate, biographical, mobile, regional).

% Doctrinal specialists who argue kami are local manifestations (suijaku) of buddhas (honji) in a single metaphysical hierarchy. Their ontological-unification project is not what this reading is about — this reading treats their claims as belonging to a different constraint (the syncretic_fusion_reading), and their voice is absent from the functional-partition account by construction, not by suppression.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, honji_suijaku_theorists, excluded,
    moderate, biographical, constrained, regional).

% Later state actors who forcibly separated kami and buddha institutions (shinbutsu bunri) on the premise that centuries of coexistence had been institutional confusion requiring correction. Their retrospective judgment that the arrangement was never coherent belongs to the incoherent_bundle_reading, not this one; they are excluded here because this reading treats the pre-Meiji arrangement as functionally stable on its own terms.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, meiji_era_separatists, excluded,
    institutional, generational, trapped, national).

% Study the shrine-temple complexes (jingu-ji) and honji suijaku doctrine as evidence for competing accounts of what shinbutsu shugo actually was — a working division of labor, a genuine metaphysical fusion, or an unstable accretion later dissolved by state fiat. Their disagreement is precisely the kernel contest this story is one reading of.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__domain_partition_reading, historians_of_japanese_religion, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows two distinct ritual institutions — kami cults oriented to this-worldly flourishing and Buddhist institutions oriented to the afterlife and salvation — to operate at the same sites and serve the same communities without requiring either to subordinate or dissolve into the other. Each institution solves a problem the other does not address.
% TRANSFER_FUNCTION: Little net transfer between the domains under this reading: shrine priesthoods retain harvest/protection ritual income and jurisdiction, temple clergy retain funerary/salvation ritual income and jurisdiction, and communities pay each institution for its respective service. Resources move from communities to specialists along domain-appropriate lines, not from one religious institution to the other.
% ABSENT_VOICES: Honji suijaku theorists, who hold that kami are local traces of buddhas in one metaphysical order, are not represented in this account — their claim describes a different constraint (ontological fusion). Meiji-era state separatists, who later declared the whole arrangement incoherent and requiring dissolution, are also absent; their retrospective verdict describes yet another constraint (the incoherent-bundle reading).
% DISAPPEARANCE_RATIONALE: If the functional-partition understanding vanished, shrine and temple institutions could in principle continue operating side by side purely from habit and local practice (low institutional entanglement is this reading's own claim) — communities would likely go on invoking whichever specialist fits the occasion. But whether that continuation would represent 'the world unchanged' or a quiet reversion toward one of the other readings (fusion or incoherence) is exactly what the sibling constraints dispute; this reading asserts easy separability, but that assertion is itself contested territory.
% FOUNDING_PROBLEM: Provide ritual coverage for the full span of human concern — this-worldly welfare (harvest, health, protection from calamity) and post-mortem fate (salvation, ancestral rites) — using two institutional traditions that arrived in Japan with different specializations and neither of which alone covered the whole span.
% FOUNDING_PROBLEM_CORROBORATION: Shrine and temple institutions themselves attest the domains remain functionally distinct and complementary. Independent evidence is mixed: honji suijaku doctrinal texts (produced by clergy with a stake in a unified account) argue against functional separation, while Meiji-era state ethnographers and modern historians of religion (outside both institutions' benefiting interest) have argued the pre-Meiji arrangement was neither cleanly partitioned nor genuinely fused but an unstable accretion — corroboration for the domain-partition claim specifically, from outside the beneficiary set, is thin.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__domain_partition_reading, contested).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__domain_partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__domain_partition_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).
:- end_tests(shinbutsu_ontological_substrate__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28 at interval end) because under the domain-partition reading neither institution captures rents from the other's domain — each collects fees/support for services within its own jurisdiction. The modest upward drift over the interval (0.15 to 0.28) reflects gradual institutional entrenchment at jingu-ji complexes, where shared physical sites and overlapping patronage networks created some structural interdependence even as functional separation was maintained. Theater ratio rises more sharply (0.10 to 0.32) reflecting increasing performative elaboration of joint ritual calendars and shared festival administration over centuries — some of this activity became more about maintaining the appearance of harmonious coexistence for patrons than about the underlying functional division itself. Suppression is low (0.22): this reading claims coexistence was voluntary and low-conflict, not maintained by coercion — institutions that found the division unworkable could and did renegotiate boundaries locally. Accessibility collapse and resistance are both moderate-low (0.35, 0.30): the domain-partition arrangement was one live option among others (fusion, rejection) rather than a foreclosed inevitability, which is exactly what makes this a contested kernel rather than settled fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine priesthoods and temple clergy are declared beneficiaries because the domain-partition reading gives each institution secure, non-competing jurisdiction — a structural subsidy relative to a world where they had to argue over primacy. Local communities and itinerant specialists are also beneficiaries: the partition lets them consume both ritual traditions without needing to resolve doctrinal tension, which is a genuine convenience under this reading. There are no declared victims because this reading's central claim is precisely that low institutional entanglement means no party is structurally extracted from by the coexistence itself. This is exactly why the reading remains contestable rather than obviously correct — a reading with no victims looking at the same historical material a sibling reading finds actively coercive (state-enforced amalgamation, Meiji-era forced separation of previously fused institutions) is a strong candidate for further scrutiny, which the omega variables below register.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (covering both this-worldly and afterlife concern with distinct specialized institutions) is authored as contested rather than resolved-dead, because the domain-partition reading holds the original coordination logic to remain intact and functioning throughout the interval — there is no claim here that the arrangement outlived its function, only a claim about what kind of arrangement it always was. The mismatch signal to watch is between this reading's founding_problem_status=contested and disappearance_verdict=contested: if a future analysis found strong evidence the domains had merged or one had captured the other by the interval's end, that would push this reading toward the fusion or incoherent-bundle siblings rather than resolve within this reading's own terms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_boundary_stability,
    'Did the this-world/afterlife domain boundary actually hold stable in practice across the Heian through Edo periods, or did jurisdictional creep occur in either direction (e.g., Buddhist institutions acquiring this-worldly protective functions, kami cults acquiring funerary functions)?',
    'Systematic study of jingu-ji temple-shrine complex records for jurisdictional disputes, fee-sharing arrangements, and ritual calendar overlap across regions and centuries; comparison with periods/regions where the boundary was explicitly contested.',
    'If the boundary held reliably, this reading''s low-entanglement claim is well-supported. If jurisdictional creep was common and required active negotiation or subordination, that evidence would favor the incoherent_bundle_reading''s claim of drift-under-tolerance rather than clean functional partition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_boundary_stability, empirical, 'Whether the claimed this-world/afterlife domain boundary was empirically stable or subject to jurisdictional creep.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the domain-partition framing (separate jurisdictions, functional coexistence) actually distinguishable in the historical record from the fusion framing (honji suijaku as literal metaphysical claim), or do practitioners and institutions at the time hold both simultaneously without treating them as competing accounts?',
    'Close reading of period doctrinal texts and ritual manuals to determine whether contemporary actors experienced the domain-partition and fusion accounts as alternatives requiring choice, or as compatible descriptions operating at different levels (practical administration vs. theological explanation).',
    'If contemporary actors held both simultaneously without contradiction, the domain-partition and syncretic-fusion readings may not be genuine rivals at all but different registers of the same commitment — which would argue for merging or tightly coupling these two constraint stories rather than treating them as independent competing readings. If they were experienced as genuine alternatives requiring institutional choice, the three-way kernel split is well-founded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether domain-partition and ontological-fusion are genuinely rival readings or compatible registers of one commitment, bearing on whether the kernel decomposition into three sibling stories is itself correctly drawn.').

omega_variable(
    state_tolerance_vs_organic_coexistence,
    'Was the low institutional entanglement this reading claims a product of organic, bottom-up religious practice, or was it actively maintained by state policy that found dual-institution tolerance administratively convenient — meaning the ''functional partition'' was itself a governed arrangement rather than a natural equilibrium?',
    'Examination of pre-Meiji state records (shogunate temple/shrine registration systems, danka seido) for evidence of active state management of the shrine-temple boundary versus laissez-faire non-interference.',
    'If state policy actively maintained the partition, this reading''s claim of ''low institutional entanglement'' and near-zero enforcement requirement would need revision toward including a governance/enforcement dimension — potentially shifting metrics toward the tangled_rope range and undermining the rope classification''s clean coordination story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_tolerance_vs_organic_coexistence, empirical, 'Whether the domain partition was an organic equilibrium or an actively state-managed arrangement, bearing on the requires_active_enforcement declaration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__domain_partition_reading, 0, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(shin_tr_t300, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 300, 0.14).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 600, 0.18).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 900, 0.22).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1200, 0.26).
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1500, 0.29).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_substrate__domain_partition_reading, theater_ratio, 1868, 0.32).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(shin_be_t300, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 300, 0.18).
narrative_ontology:measurement(shin_be_t600, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 600, 0.22).
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 900, 0.24).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1200, 0.26).
narrative_ontology:measurement(shin_be_t1500, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1500, 0.27).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_substrate__domain_partition_reading, base_extractiveness, 1868, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_ontological_substrate__domain_partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_substrate__domain_partition_reading, 0.1).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__domain_partition_reading, incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the shinbutsu_ontological_substrate kernel, all describing the same historical phenomenon (kami-buddha institutional coexistence in pre-Meiji Japan) with different structural claims and different epsilon values. domain_partition_reading (this story) authors low extraction (0.28) and a rope classification on the premise of genuine functional separability. syncretic_fusion_reading would author different metrics on the premise of literal ontological unity via honji suijaku. incoherent_bundle_reading would author metrics reflecting unstable, state-tolerated drift rather than either clean partition or genuine fusion, likely with higher suppression once Meiji-era forced separation is factored into that reading's own interval. Per the epsilon-invariance principle, these are three separate constraint files linked by network edges, not one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
