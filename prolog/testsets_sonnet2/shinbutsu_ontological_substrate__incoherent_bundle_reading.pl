% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: shinbutsu_ontological_substrate__incoherent_bundle_reading
 *   human_readable: Shinbutsu-shūgō as Incoherent Institutional Bundle Under State Enforcement
 *   domain: religious/political/historical
 *
 * SUMMARY:
 *   This story instantiates the incoherent_bundle_reading of the
 *   shinbutsu_ontological_substrate kernel: the claim that no unified
 *   metaphysical or functional-partition account of kami-buddha coexistence
 *   ever existed, and that what historians call 'shinbutsu-shūgō' is instead
 *   a name retroactively applied to centuries of locally variable,
 *   administratively convenient institutional settlements. On this reading,
 *   the honji suijaku attribution schemes (which kami is whose local trace)
 *   differ by site and period in ways inconsistent with either a single
 *   fusion doctrine or a stable domain-partition principle; they track land
 *   grants, political alliances, and registry convenience. The Tokugawa
 *   terauke system then weaponizes this pre-existing institutional
 *   entanglement for population control and religious suppression, hardening
 *   what began as ad hoc convenience into an enforced arrangement lay
 *   practitioners cannot exit. The Meiji shinbutsu bunri edicts of 1868 treat
 *   the arrangement as reversible administrative fact, not sacred truth —
 *   corroborating, from the state's own later action, that no coherent kernel
 *   was ever being protected.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.71).
domain_priors:suppression_score(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.78).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__incoherent_bundle_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__incoherent_bundle_reading, snare).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__incoherent_bundle_reading, "Shinbutsu-shūgō as Incoherent Institutional Bundle Under State Enforcement").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__incoherent_bundle_reading, "religious/political/historical").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__incoherent_bundle_reading, '744166ea-fbc5-4b58-a6a5-f5f9db256531').
narrative_ontology:cs_kernel_codification('744166ea-fbc5-4b58-a6a5-f5f9db256531', distributed).
narrative_ontology:cs_authority_grounding('744166ea-fbc5-4b58-a6a5-f5f9db256531', extraction).
narrative_ontology:cs_interpretation_layer_present('744166ea-fbc5-4b58-a6a5-f5f9db256531').
narrative_ontology:cs_reading_relation('744166ea-fbc5-4b58-a6a5-f5f9db256531', shinbutsu_ontological_substrate__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('744166ea-fbc5-4b58-a6a5-f5f9db256531', shinbutsu_ontological_substrate__domain_partition_reading, forecloses).
narrative_ontology:cs_axiom('744166ea-fbc5-4b58-a6a5-f5f9db256531', foundational, no_stable_kernel_ever_existed).
narrative_ontology:cs_axiom_status(no_stable_kernel_ever_existed, holdable).
narrative_ontology:cs_axiom_grounding('744166ea-fbc5-4b58-a6a5-f5f9db256531', no_stable_kernel_ever_existed, empirically_contingent).
narrative_ontology:cs_axiom('744166ea-fbc5-4b58-a6a5-f5f9db256531', foundational, honji_suijaku_is_administrative_convenience_not_doctrine).
narrative_ontology:cs_axiom_status(honji_suijaku_is_administrative_convenience_not_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('744166ea-fbc5-4b58-a6a5-f5f9db256531', honji_suijaku_is_administrative_convenience_not_doctrine, empirically_contingent).
narrative_ontology:cs_reference_frame('744166ea-fbc5-4b58-a6a5-f5f9db256531', no_unified_kernel_ever_present).
narrative_ontology:cs_drift_state('744166ea-fbc5-4b58-a6a5-f5f9db256531', meiji_separation_edicts_1868, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('744166ea-fbc5-4b58-a6a5-f5f9db256531', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__incoherent_bundle_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, shrine_temple_administrative_complexes).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, tokugawa_bakufu_religious_registry).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__incoherent_bundle_reading, meiji_state_prior_to_separation_edicts).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, lay_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, shrine_priests_subordinated_to_temples).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__incoherent_bundle_reading, sectarian_reformers_seeking_doctrinal_clarity).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_substrate__incoherent_bundle_reading, honji_suijaku_as_administrative_convenience_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jingūji (shrine-temples) and combinatory institutions administer land, ritual calendars, and parishioner registries under a fused kami-buddha framework. They set which local kami is which buddha's local trace, adjudicate ritual disputes, and collect tithes, land income, and pilgrimage revenue under the fused arrangement. They can shift the fusion's specific content case-by-case without needing it to be doctrinally coherent, because the arrangement's function for them is administrative and fiscal, not theological.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, shrine_temple_administrative_complexes, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, shrine_temple_administrative_complexes, beneficiary).

% Uses the terauke (temple registration) system, itself built on the fused shrine-temple institutional base, to track population, suppress prohibited sects (notably Christianity), and enforce social order. The bundle's incoherence is irrelevant to this seat's purpose; what matters is that every household is legible to a temple, and the temple is entangled with the local shrine. The registry could not function if shrines and temples were cleanly separated into competing institutions.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, tokugawa_bakufu_religious_registry, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_substrate__incoherent_bundle_reading, tokugawa_bakufu_religious_registry, beneficiary).

% Are required to register with a temple for census and anti-Christian certification purposes, participate in shrine festivals tied to agricultural and community cycles, and hold beliefs about kami and buddhas that the institutional apparatus never requires them to reconcile. They carry the resulting doctrinal ambiguity as lived contradiction — praying to a kami for a harvest and to a buddha for the dead without any institution explaining how, or whether, these are the same act. Exit from the registration requirement is not available; exit from the ambiguity is not offered because no one administering the system needs it resolved.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, lay_practitioners, payer,
    powerless, biographical, trapped, local).

% Operate under jingūji arrangements where Buddhist temple administration often holds superior institutional and often literal physical position over shrine ritual specialists. Their kami-focused practice is folded into a Buddhist administrative frame that subordinates their function and revenue share without offering a coherent account of why kami worship is subsumed rather than partnered. Leaving the arrangement means losing institutional recognition and often physical access to the shrine precinct itself, which remains legally tied to the temple.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, shrine_priests_subordinated_to_temples, payer,
    moderate, biographical, constrained, regional).

% Figures across the medieval and early modern period who pushed for either a purified Shinto (removing Buddhist accretion) or a purified Buddhism (removing kami accommodation) find their positions treated as sectarian deviation rather than live theological options within the mainstream administrative apparatus. Their objection — that the bundle asserts unity or partition only when convenient and offers no stable account otherwise — has no institutional forum within the fused system; it surfaces mainly in restricted intellectual and later nativist (kokugaku) circles, outside the state-recognized religious administration.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, sectarian_reformers_seeking_doctrinal_clarity, excluded,
    moderate, generational, constrained, national).

% Examine primary institutional records — land grants, registry documents, ritual calendars, honji suijaku attribution lists — and observe that the specific pairing of kami to buddha varies by site, era, and administrative convenience rather than following a stable doctrinal schema. They document the bundle's function as accumulated administrative settlement rather than either metaphysical unity or clean domain division.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__incoherent_bundle_reading, religious_studies_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__incoherent_bundle_reading, diffuse).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__incoherent_bundle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a working administrative and ritual vocabulary that lets shrine sites, temple institutions, and state registries operate under a single legible framework without requiring anyone to resolve whether kami and buddhas are the same beings, different beings in different domains, or unrelated categories loosely associated by local history.
% TRANSFER_FUNCTION: Moves land revenue, tithes, and administrative authority from lay communities and subordinated shrine priests to jingūji administrative complexes and, through the terauke registry, channels political control and surveillance capacity to the bakufu — all riding on an institutional bundle that is never required to be doctrinally coherent.
% ABSENT_VOICES: Sectarian reformers pressing for either doctrinal purification (Shinto-only or Buddhism-only positions) are structurally excluded from the mainstream administrative-religious apparatus; their critique that the bundle is incoherent rather than unified or partitioned surfaces in restricted scholarly and nativist circles, not in the institutions that administer shrine-temple life.
% DISAPPEARANCE_RATIONALE: If the fused administrative bundle vanished overnight — as it in fact did, forcibly, under the Meiji shinbutsu bunri edicts of 1868 — shrine and temple institutions would have to be physically and legally separated, registries rebuilt, priesthoods reassigned, and centuries of accumulated joint property and ritual practice unwound. This is not hypothetical: the actual separation triggered widespread destruction of Buddhist statuary and institutions (haibutsu kishaku), demonstrating the bundle's disappearance forces massive real-world rearrangement rather than leaving practice unchanged.
% FOUNDING_PROBLEM: Early Japanese Buddhist institutions needed local legitimacy and needed to explain their coexistence with existing kami worship without either displacing it outright or subordinating themselves to it; over centuries this produced ad hoc local settlements (honji suijaku attributions, jingūji institutions) that were never unified into a single doctrine but were serially useful to whichever authority needed shrine-temple cooperation — Buddhist institutions seeking local roots, and later the bakufu seeking a population registry mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Meiji-era state officials themselves attested the bundle had become administratively contingent rather than doctrinally necessary — this is precisely the premise the 1868 separation edicts acted on, treating shinbutsu-shūgō as reversible institutional accretion, not settled truth. Modern religious historians (outside both the shrine-temple administrative lineage and the state) corroborate this from documentary analysis of variable and inconsistent honji suijaku attributions across sites and periods, which would not be possible if a single coherent kernel had ever governed the arrangement. No source internal to the beneficiary institutions themselves ever attested to founding coherence that later drifted; the incoherence appears to trace to origin, not decay.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness starts moderate (0.35) reflecting early ad hoc local settlements with real coordination value (explaining coexistence, enabling shared ritual calendars) and rises steadily to 0.71 by 1868 as the bundle is folded into bakufu registry enforcement, with land revenue and administrative control increasingly flowing to shrine-temple complexes and the state rather than reflecting any doctrinal settlement. Theater ratio tracks a parallel rise (0.30 to 0.62): the honji suijaku vocabulary is increasingly deployed to paper over administrative inconsistency (why this kami maps to that buddha here but not elsewhere) rather than to state a coherent position, functioning more as institutional cover than living theology by the Tokugawa period. Suppression rises sharply once the terauke system attaches (jump from 0.42 at 700 to 0.75 at 1603) reflecting the criminalization of exit (mandatory temple registration, anti-Christian certification) — this is enforcement hardening layered onto a pre-existing loose institutional bundle, not enforcement of a theological claim.
 *
 * PERSPECTIVAL GAP:
 *   From the shrine-temple administrative seat, shinbutsu-shūgō looks like flexible, functional accommodation — precisely because its incoherence lets them apply local settlements pragmatically. From the lay-practitioner seat, the same absence of coherent doctrine looks like unexplained contradiction they must simply live with, backed by state enforcement they cannot contest. The engine should compute these seats as structurally divergent even though both parties nominally participate in 'the same' religious system.
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine-temple administrative complexes and the bakufu registry sit at the beneficiary end: they administer the bundle's specific content case by case, extract land revenue and political control through it, and require no doctrinal coherence for it to serve their purposes — indeed coherence would constrain their administrative flexibility. Lay practitioners sit at the target end: trapped by registration requirements, they inherit the bundle's contradictions as lived experience with no institutional venue for resolution. Shrine priests subordinated to temple administration occupy an intermediate position — moderate power, constrained exit — bearing institutional subordination dressed in fusion vocabulary that was never obligated to justify itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview surfaces exactly the mandatrophy signature: the original problem (local legitimacy negotiation between incoming Buddhist institutions and existing kami cults) was substantially resolved by the classical period, yet the resulting ad hoc bundle was retained and intensified specifically because it served a NEW function — bakufu population registry and suppression — that had nothing to do with its origin. Classifying this as snare rather than tangled_rope reflects the incoherent_bundle_reading's specific claim: there is no genuine coordination function surviving to be traded off against the extraction, because coordination requires a stable kernel to coordinate around, and this reading denies one exists. What coordination-like function remains (shared festival calendars, land administration continuity) is a byproduct of institutional path-dependency, not evidence of an underlying coherent commitment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_existence_ambiguity,
    'Did shinbutsu-shūgō ever possess ANY stable underlying commitment (fused or partitioned), or is the appearance of a ''kernel'' entirely a retrospective scholarly and institutional construction imposed on variable local practice?',
    'Systematic comparative analysis of honji suijaku attribution records across multiple sites and centuries: if attributions cluster around a small number of stable, theologically motivated patterns, some coherent kernel likely existed (favoring syncretic_fusion_reading or domain_partition_reading); if attributions vary essentially at random with local administrative and political circumstance and show no theological patterning, the incoherent_bundle_reading is supported.',
    'If a genuine kernel is found, this reading should be abandoned in favor of one of its siblings, and the classification would likely shift toward tangled_rope (real coordination function plus extraction) rather than snare (no coordination function, extraction only).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_existence_ambiguity, empirical, 'Whether any coherent theological or functional kernel underlies shinbutsu-shūgō, or whether the entire apparent ''system'' is retrospective naming of institutional drift.').

omega_variable(
    sibling_reading_foreclosure_boundary,
    'Can the domain_partition_reading and syncretic_fusion_reading both be simultaneously false in a way that leaves this reading as the residual truth, or is the incoherent_bundle_reading itself a modern historiographical projection (skeptical, secular) onto premodern actors who may have held genuine, if locally variable, theological commitments that just look incoherent to outside observers?',
    'Textual analysis of premodern doctrinal writings (as opposed to purely administrative records) from shrine-temple complexes: do these writings show internal awareness of and attempts to resolve apparent contradictions (evidence against pure incoherence), or do they show no concern for consistency across sites (evidence for incoherence)?',
    'If premodern actors demonstrably worked to reconcile apparent contradictions, the incoherent_bundle_reading overstates its case and should be weighted toward acknowledging a contested-but-real kernel, shifting the classification toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_boundary, conceptual, 'Whether the incoherence this reading identifies is a feature of premodern religious life or an artifact of applying modern coherence standards to it.').

omega_variable(
    enforcement_versus_bundle_separability,
    'Is the extraction and suppression measured here attributable to the shinbutsu-shūgō institutional bundle itself, or entirely to the later, separable terauke registry system that merely rode on top of a pre-existing (and possibly benign) local accommodation?',
    'Comparative study of extraction and suppression levels in the pre-Tokugawa period (before terauke) versus after: if extraction/suppression were low before terauke and jumped sharply after, the bundle''s core coordination function may have been largely benign and the extraction should be attributed mainly to the state registry apparatus rather than to shinbutsu-shūgō as such.',
    'If separable, this constraint''s ε should be decomposed into two distinct constraints per the ε-invariance principle: an earlier, lower-extraction local-accommodation constraint and a later, higher-extraction state-registry constraint, rather than treating the whole 1868-year interval as one constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_versus_bundle_separability, empirical, 'Whether measured extraction traces to the shrine-temple institutional bundle itself or to the later state registry system that exploited it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__incoherent_bundle_reading, 0, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(shin_tr_t0, observed).
narrative_ontology:measurement(shin_tr_t300, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 300, 0.38).
narrative_ontology:measurement_basis(shin_tr_t300, observed).
narrative_ontology:measurement(shin_tr_t700, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 700, 0.45).
narrative_ontology:measurement_basis(shin_tr_t700, observed).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1200, 0.52).
narrative_ontology:measurement_basis(shin_tr_t1200, observed).
narrative_ontology:measurement(shin_tr_t1603, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1603, 0.58).
narrative_ontology:measurement_basis(shin_tr_t1603, observed).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_ontological_substrate__incoherent_bundle_reading, theater_ratio, 1868, 0.62).
narrative_ontology:measurement_basis(shin_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(shin_be_t0, observed).
narrative_ontology:measurement(shin_be_t300, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 300, 0.45).
narrative_ontology:measurement_basis(shin_be_t300, observed).
narrative_ontology:measurement(shin_be_t700, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 700, 0.55).
narrative_ontology:measurement_basis(shin_be_t700, observed).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1200, 0.62).
narrative_ontology:measurement_basis(shin_be_t1200, observed).
narrative_ontology:measurement(shin_be_t1603, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1603, 0.68).
narrative_ontology:measurement_basis(shin_be_t1603, observed).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_ontological_substrate__incoherent_bundle_reading, base_extractiveness, 1868, 0.71).
narrative_ontology:measurement_basis(shin_be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(shin_su_t0, observed).
narrative_ontology:measurement(shin_su_t300, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 300, 0.3).
narrative_ontology:measurement_basis(shin_su_t300, observed).
narrative_ontology:measurement(shin_su_t700, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 700, 0.42).
narrative_ontology:measurement_basis(shin_su_t700, observed).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1200, 0.55).
narrative_ontology:measurement_basis(shin_su_t1200, observed).
narrative_ontology:measurement(shin_su_t1603, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1603, 0.75).
narrative_ontology:measurement_basis(shin_su_t1603, observed).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_ontological_substrate__incoherent_bundle_reading, suppression_requirement, 1868, 0.78).
narrative_ontology:measurement_basis(shin_su_t1868, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__incoherent_bundle_reading, domain_partition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the shinbutsu_ontological_substrate kernel. syncretic_fusion_reading claims ontological unity (honji suijaku as metaphysical truth) and would classify closer to rope or tangled_rope (a genuine, if contestable, coordination function). domain_partition_reading claims functional separation of this-world and afterlife domains and would similarly support a coordination-plus-modest-extraction reading. This reading (incoherent_bundle_reading) denies either sibling's positive kernel claim and reads the entire arrangement as accumulated institutional drift weaponized by state enforcement — hence snare, not tangled_rope, because no genuine coordination function is asserted to exist. All three stories share the same underlying historical record (shrine-temple institutional practice, honji suijaku attribution records, terauke registry mechanics) but assign structurally different ε, victim/beneficiary sets, and types because they differ on whether a coherent kernel exists at all.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
