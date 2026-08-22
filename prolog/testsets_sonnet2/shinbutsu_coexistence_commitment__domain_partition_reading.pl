% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__domain_partition_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: shinbutsu_coexistence_commitment__domain_partition_reading
 *   human_readable: Domain-Partitioned Kami/Buddha Coexistence (Life-Domain / Death-Domain Separation)
 *   domain: religious/philosophical/institutional
 *
 * SUMMARY:
 *   Across the medieval and early modern periods, Japanese religious life
 *   operated through parallel institutional systems — shrine-based kami
 *   worship handling life-affirming, purity, and agricultural concerns, and
 *   temple-based Buddhism handling death, funerary rites, and soteriology —
 *   without either system requiring a unified theological account of how kami
 *   and Buddhas relate to one another. This reading treats that
 *   non-unification as the structurally stable and functionally load-bearing
 *   feature, not as a gap awaiting resolution or a cover story for
 *   incoherence.
 *
 * KEY AGENTS:
 *   - shrine_priesthoods: beneficiary/agenda_setter (organized/constrained) — administer life-domain rites
 *   - temple_institutions: beneficiary/agenda_setter (organized/constrained) — administer death-domain rites
 *   - village_communities: beneficiary (moderate/constrained) — use both systems by life stage
 *   - systematic_theologians: excluded (powerless/analytical) — press for ontological resolution the arrangement does not provide
 *   - meiji_state_shinto_reformers: excluded (institutional/analytical) — later demand administrative unification the domain-partition frame was never built to satisfy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__domain_partition_reading, 0.28).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__domain_partition_reading, 0.22).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__domain_partition_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__domain_partition_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__domain_partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__domain_partition_reading, "Domain-Partitioned Kami/Buddha Coexistence (Life-Domain / Death-Domain Separation)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__domain_partition_reading, "religious/philosophical/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__domain_partition_reading, 'ee5c2d72-8f5b-43d6-afc2-5173f806599f').
narrative_ontology:cs_kernel_codification('ee5c2d72-8f5b-43d6-afc2-5173f806599f', distributed).
narrative_ontology:cs_authority_grounding('ee5c2d72-8f5b-43d6-afc2-5173f806599f', practice).
narrative_ontology:cs_interpretation_layer_present('ee5c2d72-8f5b-43d6-afc2-5173f806599f').
narrative_ontology:cs_reading_relation('ee5c2d72-8f5b-43d6-afc2-5173f806599f', shinbutsu_coexistence_commitment__syncretic_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('ee5c2d72-8f5b-43d6-afc2-5173f806599f', shinbutsu_coexistence_commitment__incoherent_bundle_reading, influences).
narrative_ontology:cs_axiom('ee5c2d72-8f5b-43d6-afc2-5173f806599f', foundational, domains_are_ontologically_independent).
narrative_ontology:cs_axiom_status(domains_are_ontologically_independent, holdable).
narrative_ontology:cs_axiom_grounding('ee5c2d72-8f5b-43d6-afc2-5173f806599f', domains_are_ontologically_independent, conventional).
narrative_ontology:cs_axiom('ee5c2d72-8f5b-43d6-afc2-5173f806599f', foundational, practice_coherence_does_not_require_doctrinal_unification).
narrative_ontology:cs_axiom_status(practice_coherence_does_not_require_doctrinal_unification, holdable).
narrative_ontology:cs_axiom_grounding('ee5c2d72-8f5b-43d6-afc2-5173f806599f', practice_coherence_does_not_require_doctrinal_unification, instrumental).
narrative_ontology:cs_reference_frame('ee5c2d72-8f5b-43d6-afc2-5173f806599f', medieval_dual_institutional_equilibrium).
narrative_ontology:cs_drift_state('ee5c2d72-8f5b-43d6-afc2-5173f806599f', meiji_shinbutsu_bunri_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('ee5c2d72-8f5b-43d6-afc2-5173f806599f', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, shrine_priesthoods).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, temple_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, village_communities).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__domain_partition_reading, ritual_specialists).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__domain_partition_reading, domain_specific_sacred_authority).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__domain_partition_reading, functional_pluralism_without_ontological_unification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer kami rites tied to birth, purity, harvest, and community continuity. Their authority over the life-domain is secure precisely because Buddhist institutions do not contest jurisdiction over birth and harvest ritual; the partition protects their functional monopoly without requiring them to explain how kami relate ontologically to Buddhas.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, shrine_priesthoods, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, shrine_priesthoods, agenda_setter).

% Administer funerary rites, ancestor memorialization, and salvation doctrine. Their authority over the death-domain is likewise uncontested by shrine priesthoods. Both institutions collect fees and social standing from their respective domains without needing a shared cosmology to justify the division of labor.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, temple_institutions, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__domain_partition_reading, temple_institutions, agenda_setter).

% Use shrine rites for birth, marriage, and harvest and temple rites for funerals and memorial services, moving between the two systems as life-stage requires. They do not experience this as contradiction because each system answers a different practical question; asking them to reconcile the systems theologically would be asking a question their practice never poses.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, village_communities, beneficiary,
    moderate, biographical, constrained, local).

% Itinerant or dual-trained practitioners (yamabushi, some Buddhist-Shinto hybrid clergy) who perform rites across both domains, translating community need into the appropriate ritual register without adjudicating which cosmology is 'true.'
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, ritual_specialists, beneficiary,
    moderate, biographical, mobile, local).

% Would press for a unified account of how kami and Buddhas relate ontologically — are they the same beings under different names, different orders of being, or unrelated? The domain-partition reading has no place for this question; systematizers who raise it are answered with 'that is not what this practice is for' rather than with an argument.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, systematic_theologians, excluded,
    powerless, civilizational, analytical, national).

% Later state actors who insisted the domains be forcibly separated (shinbutsu bunri) precisely because functional coexistence without ontological resolution was administratively intolerable to a modernizing state that needed a single, legible religious category. From this reading's own frame they are outside the system it describes, imposing a demand for unification the domain-partition arrangement was never built to satisfy.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__domain_partition_reading, meiji_state_shinto_reformers, excluded,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides existential labor: kami rites handle this-worldly concerns (birth, purity, agricultural fertility, community continuity) and Buddhist rites handle other-worldly concerns (death, salvation, karmic destiny). Communities get complete life-cycle ritual coverage without either institution needing to resolve what the other institution's deities actually are.
% TRANSFER_FUNCTION: Moves ritual fees, land grants, and social deference to shrine priesthoods for life-domain services and to temple institutions for death-domain services, keeping the flows separate along the same boundary that separates the domains themselves.
% ABSENT_VOICES: Systematic theologians pressing for ontological coherence, and later Meiji state reformers demanding administrative unification, are both outside the frame this reading describes — the arrangement persisted for centuries by never inviting the question they wanted answered.
% DISAPPEARANCE_RATIONALE: If the domain partition dissolved, either shrines or temples would need to claim jurisdiction over the other's domain (birth ritual and funerary ritual would compete for the same institutional authority), or communities would need an entirely new arrangement for life-cycle ritual coverage — the division of ritual labor between the two institutional systems would have to be renegotiated from scratch.
% FOUNDING_PROBLEM: Communities needed complete ritual coverage across the life cycle — birth, growth, marriage, harvest, death, afterlife — but no single imported or indigenous system covered all of it. Buddhism arrived with sophisticated soteriology but no indigenous fertility/purity ritual apparatus; kami worship had deep local roots in agriculture and purity but no developed afterlife doctrine.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological and historical fieldwork on contemporary Japanese religious practice (external to both shrine and temple institutions) documents the same domain-split pattern persisting in household ritual behavior today — most Japanese households use shrines for life events and temples for funerals without treating this as contradictory, corroborating that the founding problem (complete life-cycle ritual coverage) remains functionally addressed by the same division, independent of either institution's self-interested account of its own necessity.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__domain_partition_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).
:- end_tests(shinbutsu_coexistence_commitment__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.28 at interval end) because the arrangement's persistence does not depend on suppressing an alternative that participants want and cannot get — households genuinely use both systems for different purposes and pay for services genuinely rendered in each domain. Suppression is low (0.22): there is no active coercive apparatus forcing kami and Buddhist institutions to stay separate; the boundary holds through practical convenience and institutional non-interference rather than enforcement. Theater ratio is low and rises only slightly over the very long interval (0.10 to 0.15) as institutional self-justification narratives thickened over centuries without displacing the underlying functional division. Accessibility collapse is moderate (0.35): communities could in principle have sought a unified account, and some intellectual currents (culminating in honji suijaku theorizing and later in Meiji-era forced separation) did press toward resolution, so alternatives to the partition were never fully foreclosed the way a genuine mountain forecloses alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From inside the domain-partition frame — shrine priesthoods, temple institutions, ordinary households — the arrangement looks like straightforward functional coordination: two systems, two jobs, no conflict. From outside the frame — a systematizing theologian, or a modernizing state bureaucrat who needs single, legible religious categories for legal and administrative purposes — the same arrangement looks like an unresolved contradiction demanding either unification (the honji suijaku move) or forcible separation (the Meiji shinbutsu bunri move). The engine should compute the excluded seats' experience differently from the beneficiary seats' experience precisely because their exit options and time horizons differ: the excluded seats are analytical/civilizational (concerned with coherence across centuries) while the beneficiary seats are constrained/generational (concerned with functioning ritual coverage within their own institutional lifespan).
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine priesthoods and temple institutions are declared beneficiaries because the domain partition secures each institution's uncontested jurisdiction over its half of the ritual life-cycle without requiring either to defend a systematic cosmology — this yields low directionality (near the beneficiary end) for both. Village communities are also beneficiaries: they receive complete ritual coverage they could not get from either system alone, at a cost (dual ritual fees/participation) they willingly bear because each half addresses a genuinely different practical need — this reads as closer to symmetric than extractive. No victim group is declared because this reading does not locate an extraction target: the partition's cost is paid in kind (participation in two systems) for value received in kind (coverage of two different existential domains), not siphoned to an uninvolved party. Systematic theologians and Meiji reformers are excluded rather than victimized — they are structurally outside the arrangement's frame, wanting a different kind of system (a unified one) rather than being extracted from within this one.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (complete life-cycle ritual coverage where no single system covered the whole cycle) remains live according to corroborating evidence from outside both institutions (contemporary anthropological fieldwork on Japanese household practice), which prevents this reading from being mislabeled as a hollowed-out arrangement kept alive only by institutional inertia. Because founding_problem_status is 'live' and disappearance_verdict is 'world_rearranges' with no status/verdict mismatch, no zombie-arrangement flag should fire — this is closer to a genuine, still-functioning rope than to a piton performing a function it no longer serves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_partition_vs_syncretic_fusion_locus,
    'Is the historically dominant lived reading of shinbutsu-shugo actually the domain-partition reading (functional coexistence without ontological claims), or was honji suijaku theorizing (syncretic fusion) doctrinally dominant among literate elites even while ordinary practice looked partition-like?',
    'Comparative analysis of elite doctrinal writing (which skews toward honji suijaku formulations) versus popular ritual practice records and pilgrimage/festival participation patterns (which skew toward domain-partition behavior) across regions and periods.',
    'If elite doctrine was genuinely dominant and popular practice was merely downstream compliance with an underlying unified cosmology, this reading overstates the independence of the two domains and understates the syncretic_fusion_reading''s structural priority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_partition_vs_syncretic_fusion_locus, conceptual, 'Whether domain-partition or syncretic-fusion better captures the historically operative kernel reading, or whether they coexisted at different social strata.').

omega_variable(
    partition_stability_vs_managed_ambiguity,
    'Was the domain partition a genuinely stable functional division, or was it deliberately maintained ambiguity that only looked stable because no external party had reason to force clarification until the Meiji state did?',
    'Examine pre-Meiji instances where jurisdictional disputes DID arise between shrine and temple institutions (land, mixed funerary/purity rites, contested sacred sites) and whether resolution mechanisms existed short of forced separation.',
    'If disputes were routinely resolved through improvised local accommodation rather than any stable principle, this reading''s claim of structural stability collapses toward the incoherent_bundle_reading''s account of managed ambiguity rather than genuine functional partition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_stability_vs_managed_ambiguity, empirical, 'Whether the partition reflects real structural stability or retrospectively-imposed coherence on what was managed ambiguity.').

omega_variable(
    beneficiary_status_vs_natural_division_of_labor,
    'Are shrine priesthoods and temple institutions genuinely ''beneficiaries'' in an extractive sense, or does the domain-partition reading''s own logic suggest this is simply a natural, low-friction division of religious labor with no meaningful surplus being captured by either institution at the other''s expense?',
    'Compare ritual fee structures and land-grant patterns for shrines versus temples over time to determine whether either extracted disproportionate resources relative to the ritual complexity/labor they provided.',
    'If fee/resource capture was roughly proportional to service across both institutions, the ''beneficiary'' framing should be read as mild (near-rope) rather than as concealing meaningful extraction; if asymmetric, some domain-partition-era arrangements may deserve reclassification toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_status_vs_natural_division_of_labor, empirical, 'Whether institutional beneficiary status reflects genuine surplus capture or simply describes participants in a low-extraction coordination arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__domain_partition_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(shin_tr_t0, projected).
narrative_ontology:measurement(shin_tr_t200, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 200, 0.1).
narrative_ontology:measurement_basis(shin_tr_t200, projected).
narrative_ontology:measurement(shin_tr_t400, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 400, 0.11).
narrative_ontology:measurement_basis(shin_tr_t400, projected).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 600, 0.12).
narrative_ontology:measurement_basis(shin_tr_t600, projected).
narrative_ontology:measurement(shin_tr_t800, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 800, 0.13).
narrative_ontology:measurement_basis(shin_tr_t800, projected).
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1000, 0.14).
narrative_ontology:measurement_basis(shin_tr_t1000, observed).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, theater_ratio, 1200, 0.15).
narrative_ontology:measurement_basis(shin_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(shin_be_t0, projected).
narrative_ontology:measurement(shin_be_t200, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 200, 0.24).
narrative_ontology:measurement_basis(shin_be_t200, projected).
narrative_ontology:measurement(shin_be_t400, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 400, 0.25).
narrative_ontology:measurement_basis(shin_be_t400, projected).
narrative_ontology:measurement(shin_be_t600, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 600, 0.26).
narrative_ontology:measurement_basis(shin_be_t600, projected).
narrative_ontology:measurement(shin_be_t800, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 800, 0.27).
narrative_ontology:measurement_basis(shin_be_t800, projected).
narrative_ontology:measurement(shin_be_t1000, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1000, 0.28).
narrative_ontology:measurement_basis(shin_be_t1000, observed).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_coexistence_commitment__domain_partition_reading, base_extractiveness, 1200, 0.28).
narrative_ontology:measurement_basis(shin_be_t1200, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_coexistence_commitment__domain_partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__domain_partition_reading, 0.1).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__domain_partition_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the shinbutsu_coexistence_commitment kernel. domain_partition_reading (this file) treats kami and Buddhist deities as governing genuinely separate existential domains with no ontological unification claimed or needed — low extraction, low suppression, rope-leaning. syncretic_fusion_reading claims explicit ontological unification via honji suijaku (kami as local manifestations of universal Buddhist truth) — a different, stronger metaphysical commitment with its own beneficiary structure (likely favoring Buddhist institutional authority over kami cults, since honji suijaku typically subordinates kami as manifestations rather than co-equals). incoherent_bundle_reading denies the arrangement was ever a coherent system at all, treating apparent coexistence as deliberately maintained ambiguity that collapsed under Meiji administrative pressure — this reading would carry higher theater_ratio and likely a tangled_rope or snare classification reflecting the ambiguity's protective function for institutional actors. Each reading has its own epsilon; they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
