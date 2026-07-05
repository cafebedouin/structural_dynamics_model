% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__honji_suijaku_monism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__honji_suijaku_monism, []).

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
 *   constraint_id: kami_buddha_ontology__honji_suijaku_monism
 *   human_readable: Honji Suijaku Monism — Kami as Traces of Buddhist Original Ground
 *   domain: religious/philosophical/institutional
 *
 * SUMMARY:
 *   This constraint models the honji-suijaku (original ground / trace
 *   manifestation) theory as it functioned within medieval Japanese
 *   shinbutsu-shugo (kami-buddha combinatory) institutions: the doctrinal
 *   claim that kami are phenomenal traces of an underlying, ontologically
 *   prior Buddhist reality — specific buddhas and bodhisattvas manifesting
 *   locally as kami to save beings not yet ready for direct Buddhist
 *   teaching. This is a single reading among three competing accounts of the
 *   same combinatory kernel; the domain-partition reading (kami and buddhas
 *   as ontologically distinct, governing separate functional domains) and the
 *   incoherent-bundle reading (shinbutsu-shugo as an unsystematized bundle of
 *   contradictory commitments held together institutionally rather than
 *   logically) are separate constraints with their own ε values, not
 *   alternative measurements of this one. This story evaluates only the
 *   monist, hierarchical, Buddhist-prior reading and its structural
 *   consequences.
 *
 * KEY AGENTS:
 *   - buddhist_temple_institutions: agenda_setter/beneficiary — produce and administer the honji-suijaku theoretical apparatus
 *   - shingon_tendai_scholastic_lineages: beneficiary — supply the metaphysical machinery that grounds Buddhist ontological priority
 *   - court_sanctioned_syncretic_priesthoods: beneficiary — draw dual revenue and status from joint administration
 *   - independent_kami_shrine_priesthoods: payer — lose interpretive authority over their own kami's nature
 *   - local_kami_cult_practitioners: payer — inherit a cosmology that subordinates their ancestral worship
 *   - kokugaku_revivalist_scholars: excluded — the dissenting reading with no institutional seat during the dominant period
 *   - comparative_religion_scholars: observer — reconstruct the doctrine's development and institutional function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, 0.58).
domain_priors:suppression_score(kami_buddha_ontology__honji_suijaku_monism, 0.52).
domain_priors:theater_ratio(kami_buddha_ontology__honji_suijaku_monism, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, extractiveness, 0.58).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__honji_suijaku_monism, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__honji_suijaku_monism, "Honji Suijaku Monism — Kami as Traces of Buddhist Original Ground").
narrative_ontology:topic_domain(kami_buddha_ontology__honji_suijaku_monism, "religious/philosophical/institutional").

domain_priors:requires_active_enforcement(kami_buddha_ontology__honji_suijaku_monism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__honji_suijaku_monism, 'b131817b-d324-4c8b-bffa-efeb5424f899').
narrative_ontology:cs_kernel_codification('b131817b-d324-4c8b-bffa-efeb5424f899', formalized).
narrative_ontology:cs_authority_grounding('b131817b-d324-4c8b-bffa-efeb5424f899', lineage).
narrative_ontology:cs_interpretation_layer_present('b131817b-d324-4c8b-bffa-efeb5424f899').
narrative_ontology:cs_reading_relation('b131817b-d324-4c8b-bffa-efeb5424f899', kami_buddha_ontology__domain_partition, forecloses).
narrative_ontology:cs_reading_relation('b131817b-d324-4c8b-bffa-efeb5424f899', kami_buddha_ontology__incoherent_bundle, influences).
narrative_ontology:cs_axiom('b131817b-d324-4c8b-bffa-efeb5424f899', foundational, buddhist_entities_ontologically_prior).
narrative_ontology:cs_axiom_status(buddhist_entities_ontologically_prior, holdable).
narrative_ontology:cs_axiom_grounding('b131817b-d324-4c8b-bffa-efeb5424f899', buddhist_entities_ontologically_prior, theological).
narrative_ontology:cs_axiom('b131817b-d324-4c8b-bffa-efeb5424f899', foundational, kami_lack_independent_ontological_status).
narrative_ontology:cs_axiom_status(kami_lack_independent_ontological_status, holdable).
narrative_ontology:cs_axiom_grounding('b131817b-d324-4c8b-bffa-efeb5424f899', kami_lack_independent_ontological_status, theological).
narrative_ontology:cs_axiom('b131817b-d324-4c8b-bffa-efeb5424f899', secondary, single_ultimate_reality_requires_systematic_hierarchy).
narrative_ontology:cs_axiom_status(single_ultimate_reality_requires_systematic_hierarchy, overridden).
narrative_ontology:cs_axiom_grounding('b131817b-d324-4c8b-bffa-efeb5424f899', single_ultimate_reality_requires_systematic_hierarchy, conventional).
narrative_ontology:cs_reference_frame('b131817b-d324-4c8b-bffa-efeb5424f899', esoteric_buddhist_original_enlightenment_cosmology).
narrative_ontology:cs_drift_state('b131817b-d324-4c8b-bffa-efeb5424f899', meiji_shinbutsu_bunri_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('b131817b-d324-4c8b-bffa-efeb5424f899', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, buddhist_temple_institutions).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, shingon_tendai_scholastic_lineages).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, court_sanctioned_syncretic_priesthoods).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, independent_kami_shrine_priesthoods).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, local_kami_cult_practitioners).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, buddhist_ontological_priority_doctrine).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, single_ultimate_reality_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the jingu-ji (shrine-temple) complexes and issue the theoretical apparatus explaining kami as manifestations of specific buddhas and bodhisattvas. They control the honji-suijaku assignments (which buddha stands behind which kami), collect offerings and land grants flowing to the temple side of combined shrine-temple institutions, and train the scholar-monks who systematize the doctrine. Their exit from this arrangement is costless — the theory is their institutional product.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, buddhist_temple_institutions, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__honji_suijaku_monism, buddhist_temple_institutions, beneficiary).

% Esoteric Buddhist schools that supply the metaphysical machinery (dharma-body, expedient means, original enlightenment thought) used to explain why kami are traces of buddhas rather than the reverse. They gain prestige, patronage, and doctrinal authority whenever the honji-suijaku framework is invoked to subordinate local kami cults to Buddhist cosmology.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, shingon_tendai_scholastic_lineages, beneficiary,
    institutional, civilizational, arbitrage, national).

% Priests who hold joint appointments across combined shrine-temple sites benefit from the theoretical fusion: it lets them draw revenue and status from both ritual traditions simultaneously, using Buddhist ontological priority to justify their supervisory role over shrine affairs.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, court_sanctioned_syncretic_priesthoods, beneficiary,
    powerful, generational, mobile, regional).

% Local shrine priests whose kami traditions predate the Buddhist theoretical overlay find their deity reclassified as a lesser, dependent manifestation of a buddha whose worship, temple, and priesthood must now be housed alongside their own. They lose interpretive authority over their own kami's nature and often lose a share of ritual and material control to the co-located temple; abandoning the shrine tradition entirely is not a real option given local social embeddedness.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, independent_kami_shrine_priesthoods, payer,
    moderate, generational, constrained, regional).

% Villagers and lay worshippers whose ancestral kami worship is reframed by the dominant theory as a provisional, lower-order access point to Buddhist salvation. Their inherited cosmology is not abolished but is subordinated — they continue practicing but within a hierarchy that tells them their kami is derivative, not primary. They have no institutional voice in the systematization process.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, local_kami_cult_practitioners, payer,
    powerless, generational, trapped, local).

% Later nativist scholars who would argue kami are ontologically prior and independent of Buddhist metaphysics are structurally absent from the honji-suijaku theoretical apparatus during its dominant period; their reading only gains institutional traction centuries later, after political conditions shift. Within the honji-suijaku era, they have no seat at the table where the ontology is fixed.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, kokugaku_revivalist_scholars, excluded,
    moderate, generational, constrained, national).

% Historians of Japanese religion who reconstruct the honji-suijaku theory's textual development, map which temple lineages produced which trace-assignments, and assess whether the monist ontology was a genuine metaphysical conviction or a doctrinal instrument for institutional consolidation.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__honji_suijaku_monism, buddhist_temple_institutions).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__honji_suijaku_monism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single coherent cosmology that lets shrine and temple ritual, personnel, and property coexist at the same physical sites without requiring worshippers to choose between traditions or resolve two competing accounts of the sacred independently at every locality.
% TRANSFER_FUNCTION: Moves interpretive authority, ritual precedence, land revenue, and doctrinal prestige from independent kami priesthoods and lay kami cults toward Buddhist temple institutions and the scholastic lineages that produce and certify the honji-suijaku trace-assignments.
% ABSENT_VOICES: Independent kami priesthoods whose traditions predate Buddhist arrival, and lay practitioners whose cosmology is reframed as derivative, have no role in constructing the theoretical apparatus that reclassifies their own deities; their objection — that kami are self-standing and prior, not traces — surfaces institutionally only centuries later through Kokugaku revivalism.
% DISAPPEARANCE_RATIONALE: If the honji-suijaku ontological framework vanished, jointly administered shrine-temple complexes would lose their shared theoretical justification, temple institutions would lose their doctrinal claim to precedence over co-located kami worship, and independent kami priesthoods would regain unmediated interpretive control over their own deities' nature — much as happened, materially, at the Meiji shinbutsu bunri separation.
% FOUNDING_PROBLEM: Buddhism arrived in Japan alongside entrenched, geographically dispersed kami worship; without a unifying theory, competing claims to sacred authority at the same sites and over the same communities threatened both traditions' stability and made joint ritual administration incoherent.
% FOUNDING_PROBLEM_CORROBORATION: Temple institutions and scholastic lineages attest the problem remained live throughout the medieval period, citing continued need for cosmological coherence at combined sites. Independent kami priesthoods and later Kokugaku scholars — outside the beneficiary set — attest the 'problem' was substantially a doctrinal solution to a power-sharing arrangement that could have been organized on partition or reciprocal terms instead, and that the monist framing was never neutral with respect to who ended up subordinate.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__honji_suijaku_monism, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__honji_suijaku_monism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__honji_suijaku_monism, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kami_buddha_ontology__honji_suijaku_monism, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__honji_suijaku_monism, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects a real but partial transfer: the theory does perform genuine coordination work (letting shrine and temple coexist administratively at combined sites) while also systematically reassigning interpretive and material precedence toward the Buddhist institutional side. Suppression (0.52) is moderate — dissenting local traditions were not eradicated, they were subordinated and absorbed into a hierarchy that persisted through court and monastic backing over centuries, requiring continuous doctrinal maintenance (hence requires_active_enforcement) rather than pure consent. Theater ratio (0.42) is substantial by the end of the interval: as the theory calcified into elaborate trace-assignment taxonomies (specific buddha-to-kami correspondences multiplying scholastically), an increasing share of the intellectual activity serviced doctrinal completeness and institutional legitimation rather than any live soteriological or administrative need. Accessibility collapse (0.6) is moderate-high: once absorbed into a jointly administered site, an independent kami priesthood's alternative framing became difficult to articulate or act on locally, though it never fully vanished (later Kokugaku revival demonstrates the alternative reading survived somewhere). Resistance (0.55) reflects real, if largely submerged, friction — local kami cults persisted in practice even while institutionally subordinated, and the reading's eventual overturning at Meiji shinbutsu bunri shows the subordination was contested rather than settled.
 *
 * PERSPECTIVAL GAP:
 *   From the temple-institution seat, the honji-suijaku framework is settled metaphysical truth undergirding legitimate joint administration. From the independent kami priesthood seat, the same framework is a hierarchy imposed on a tradition that considers itself self-standing. The engine's per-seat computation should reflect this: the agenda-setter/beneficiary seats compute as participating in stable coordination, while the payer seats compute nearer extraction, from the identical structural facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist temple institutions and scholastic lineages sit at the beneficiary end: they produce the theory, control its application, and collect the institutional benefits of ontological priority. Independent kami priesthoods and lay practitioners sit toward the target end: the theory is applied TO their tradition, reclassifying it, without their participation in its construction. Court-sanctioned joint priesthoods are intermediate — they benefit from the arrangement's practical convenience even though they must operate within a Buddhist-prior framework not of their own devising.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating two traditions occupying the same ritual and geographic space) was genuinely live at the doctrine's origin. Its status becomes contested precisely because the coordination could have been achieved on partition terms (domain_partition reading) or left explicitly unsystematized (incoherent_bundle reading) without the specific hierarchical claim that kami are ontologically dependent traces. The persistence of the monist reading past the point where joint administration required it — visible in the rising theater_ratio as trace-assignment taxonomies proliferated scholastically — is the signature this story is built to register: a coordination function real at t0 increasingly overlaid with doctrinal elaboration serving institutional precedence rather than administrative necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_metaphysical_conviction_vs_institutional_instrument,
    'Was honji-suijaku monism a sincerely held metaphysical conviction among its scholastic architects, or primarily a doctrinal instrument constructed to secure Buddhist institutional precedence at combined shrine-temple sites?',
    'Close textual analysis of early honji-suijaku treatises for internal argumentative structure versus post-hoc rationalization patterns; comparison with contexts where the theory was invoked opportunistically to resolve specific land or revenue disputes.',
    'If sincere conviction, the extraction reading is weaker and the tangled_rope classification should lean more toward genuine coordination with incidental asymmetry; if primarily instrumental, the extraction component is closer to deliberate institutional capture and the tangled_rope classification is more heavily weighted toward the extraction pole.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_metaphysical_conviction_vs_institutional_instrument, conceptual, 'Whether the ontology was sincere metaphysics or institutional instrument.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that the underlying shinbutsu-shugo kernel supports at least three structurally distinct readings (monist hierarchy, domain partition, incoherent bundle), what determined which reading dominated institutional practice at a given site and period, and could the choice itself have been contested locally rather than uniform?',
    'Site-by-site historical survey of which theoretical framing (monist, partition, or unsystematized coexistence) was invoked in temple records, land grants, and ritual manuals across different regions and periods, to test whether honji-suijaku monism was ever truly dominant versus one framing among several in simultaneous local use.',
    'If regional variation was substantial, this constraint''s claim to model ''the'' dominant reading during the interval overstates uniformity, and the true historical picture may be closer to the incoherent_bundle reading operating underneath a monist doctrinal veneer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, empirical, 'Uncertainty over how uniformly the monist reading actually dominated versus competing local framings.').

omega_variable(
    foreclosure_reversibility,
    'Was the Buddhist ontological priority claimed by honji-suijaku monism ever fully foreclosing of the domain-partition reading in practice, given that partition-like functional divisions (purity/impurity, life/death domains) persisted informally even under monist doctrine?',
    'Examine ritual practice records for functional domain separation (e.g., kami handling birth/purity rites, Buddhist institutions handling death rites) occurring concurrently with monist doctrinal statements, to assess whether the two readings coexisted in practice despite doctrinal incompatibility in theory.',
    'If functional partition persisted in practice regardless of doctrinal monism, the reading_relations edge to domain_partition should be reconsidered from a stronger claim toward coexists_with rather than any stronger displacement, since the readings may have operated at different levels (doctrine vs. practice) simultaneously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_reversibility, conceptual, 'Whether doctrinal monism actually foreclosed functional partition in lived practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__honji_suijaku_monism, 0, 900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0, 0.2).
narrative_ontology:measurement(kami_tr_t150, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 150, 0.26).
narrative_ontology:measurement(kami_tr_t300, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 300, 0.33).
narrative_ontology:measurement(kami_tr_t450, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 450, 0.37).
narrative_ontology:measurement(kami_tr_t600, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 600, 0.39).
narrative_ontology:measurement(kami_tr_t750, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 750, 0.41).
narrative_ontology:measurement(kami_tr_t900, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 900, 0.42).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(kami_be_t150, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 150, 0.38).
narrative_ontology:measurement(kami_be_t300, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 300, 0.46).
narrative_ontology:measurement(kami_be_t450, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 450, 0.52).
narrative_ontology:measurement(kami_be_t600, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 600, 0.55).
narrative_ontology:measurement(kami_be_t750, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 750, 0.57).
narrative_ontology:measurement(kami_be_t900, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 900, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(kami_su_t150, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 150, 0.4).
narrative_ontology:measurement(kami_su_t300, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 300, 0.44).
narrative_ontology:measurement(kami_su_t450, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 450, 0.47).
narrative_ontology:measurement(kami_su_t600, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 600, 0.49).
narrative_ontology:measurement(kami_su_t750, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 750, 0.51).
narrative_ontology:measurement(kami_su_t900, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 900, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__honji_suijaku_monism, identity_coordination).
narrative_ontology:boltzmann_floor_override(kami_buddha_ontology__honji_suijaku_monism, 0.1).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__incoherent_bundle).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked stories decomposing the colloquial label 'shinbutsu-shugo' (kami-buddha combinatory religion) per the ε-invariance principle. honji_suijaku_monism (this story) claims a single ultimate reality with Buddhist ontological priority and kami as dependent traces — a tangled_rope with real coordination function overlaid with asymmetric extraction favoring Buddhist institutions. domain_partition claims ontological parity with functional separation (kami for life/purity, Buddhist entities for death/impurity) — a structurally different, likely lower-extraction rope-leaning constraint since neither tradition is subordinated. incoherent_bundle claims the entire kernel is not a coherent single commitment at all but an institutionally sustained bundle of contradictory practices — likely a piton or tangled_rope candidate depending on whether any party benefits from the incoherence persisting. Each story carries its own ε and stakeholder structure; they should not be merged or averaged, only linked structurally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
