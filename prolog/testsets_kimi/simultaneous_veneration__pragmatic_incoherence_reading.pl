% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__pragmatic_incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__pragmatic_incoherence_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: simultaneous_veneration__pragmatic_incoherence_reading
 *   human_readable: Simultaneous Veneration under Pragmatic Incoherence
 *   domain: religious studies/comparative religion/japanese history
 *
 * SUMMARY:
 *   This constraint story models the syncretic religious regime of pre-Meiji
 *   Japan (shinbutsu-shÅ«gÅ) under the pragmatic incoherence reading: the
 *   simultaneous veneration of kami and buddhas was not a stable theological
 *   coordination but an unstable arrangement in which practitioners held
 *   contradictory beliefs without resolution. The regime extracted economic
 *   rents from parishioners and cognitive labor from clergy, sustained by
 *   institutional inertia and identity lock rather than by active
 *   enforcement. The Meiji shinbutsu-bunri (separation of kami and buddhas)
 *   is read not as an imposed rupture but as a revelation of latent
 *   incoherence that became visible once enforcement pressure was finally
 *   applied.
 *
 * KEY AGENTS:
 *   - shrine_temple_complexes: Agenda-setter (institutional/constrained) â administered the syncretic rites and captured ritual fees
 *   - syncretic_elite_clergy: Beneficiary (powerful/constrained) â elaborated theological cover for the arrangement
 *   - common_worshippers: Primary target (powerless/identity_locked) â paid fees and participated in contradictory rites as condition of community membership
 *   - parish_clergy: Secondary target (moderate/identity_locked) â performed incoherent rituals daily without authority to resolve contradictions
 *   - kokugaku_scholars: Excluded voice (moderate/constrained) â advocated purity but were kept outside institutional authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, 0.72).
domain_priors:suppression_score(simultaneous_veneration__pragmatic_incoherence_reading, 0.48).
domain_priors:theater_ratio(simultaneous_veneration__pragmatic_incoherence_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__pragmatic_incoherence_reading, snare).
narrative_ontology:human_readable(simultaneous_veneration__pragmatic_incoherence_reading, "Simultaneous Veneration under Pragmatic Incoherence").
narrative_ontology:topic_domain(simultaneous_veneration__pragmatic_incoherence_reading, "religious studies/comparative religion/japanese history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__pragmatic_incoherence_reading, 'eb5ea520-64a4-44f3-ab1c-7d9889d75793').
narrative_ontology:cs_kernel_codification('eb5ea520-64a4-44f3-ab1c-7d9889d75793', implicit).
narrative_ontology:cs_authority_grounding('eb5ea520-64a4-44f3-ab1c-7d9889d75793', extraction).
narrative_ontology:cs_interpretation_layer_present('eb5ea520-64a4-44f3-ab1c-7d9889d75793').
narrative_ontology:cs_reading_relation('eb5ea520-64a4-44f3-ab1c-7d9889d75793', simultaneous_veneration__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb5ea520-64a4-44f3-ab1c-7d9889d75793', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_axiom('eb5ea520-64a4-44f3-ab1c-7d9889d75793', foundational, practical_incoherence_as_structural_condition).
narrative_ontology:cs_axiom_status(practical_incoherence_as_structural_condition, holdable).
narrative_ontology:cs_axiom_grounding('eb5ea520-64a4-44f3-ab1c-7d9889d75793', practical_incoherence_as_structural_condition, empirically_contingent).
narrative_ontology:cs_axiom('eb5ea520-64a4-44f3-ab1c-7d9889d75793', foundational, enforcement_absence_sustains_syncretism).
narrative_ontology:cs_axiom_status(enforcement_absence_sustains_syncretism, holdable).
narrative_ontology:cs_axiom_grounding('eb5ea520-64a4-44f3-ab1c-7d9889d75793', enforcement_absence_sustains_syncretism, empirically_contingent).
narrative_ontology:cs_reference_frame('eb5ea520-64a4-44f3-ab1c-7d9889d75793', latent_incoherence_unenforced).
narrative_ontology:cs_drift_state('eb5ea520-64a4-44f3-ab1c-7d9889d75793', meiji_shinbutsu_bunri, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('eb5ea520-64a4-44f3-ab1c-7d9889d75793', '2026-06-20T12:00:00Z').
narrative_ontology:cs_kernel_id(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, shrine_temple_complexes).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, syncretic_elite_clergy).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, common_worshippers).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, parish_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered combined Buddhist and Shinto rites for parishioners, collecting donations and fees from both ritual economies. Their institutional identity and revenue streams were built on the syncretic arrangement; exit would require dismantling centuries of entangled property and parish relationships.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, shrine_temple_complexes, agenda_setter,
    institutional, generational, constrained, national).

% High-ranking abbots and shrine priests who elaborated honji-suijaku theory and other syncretic theologies. They benefited from institutional prestige and scholarly authority derived from maintaining the doctrinal ambiguity that justified simultaneous rites.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, syncretic_elite_clergy, beneficiary,
    powerful, generational, constrained, national).

% Required to participate in both Buddhist memorial services and Shinto festivals through their parish affiliations, paying fees to both shrine and temple. Their religious identity was fused with local community membership, making exit from the syncretic system equivalent to social exile.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, common_worshippers, payer,
    powerless, biographical, identity_locked, local).

% Lower-ranking priests and shrine attendants who performed daily rituals requiring them to articulate Buddhist and Shinto meanings simultaneously. They bore the cognitive cost of maintaining contradictory theologies in practice without the authority or resources to resolve them.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, parish_clergy, payer,
    moderate, biographical, identity_locked, regional).

% Advocates of Shinto purity and nativist learning who argued for separating kami worship from Buddhist contamination. They were structurally excluded from institutional authority before the Meiji period; their voices were marginalized by the syncretic elite who controlled religious education and certification.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, kokugaku_scholars, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__pragmatic_incoherence_reading, shrine_temple_complexes).
narrative_ontology:fixing_cost_class(simultaneous_veneration__pragmatic_incoherence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Presented itself as coordinating access to both this-worldly and other-worldly benefits through a unified ritual economy, but under this reading no genuine coordination existed â only the appearance of coordination masking unresolved theological contradiction.
% TRANSFER_FUNCTION: Moves ritual fees, parishioner loyalty, and institutional legitimacy from common worshippers to shrine-temple complexes, while extracting cognitive labor from clergy and worshippers to maintain contradictory belief structures without resolution.
% ABSENT_VOICES: Kokugaku scholars advocating Shinto purity; Buddhist reformers seeking sectarian clarification; common worshippers who might have preferred coherent affiliation but were embedded in parish systems.
% DISAPPEARANCE_RATIONALE: The separation of kami and buddhas forced institutional reorganization, disentangling parish networks, dissolving combined shrine-temple complexes, and requiring worshippers to choose distinct affiliations rather than participate in an ambiguous whole.
% FOUNDING_PROBLEM: The historical entanglement of Buddhist institutions with local kami cults from the Heian period onward, which created interlocking ritual economies that became politically and economically self-sustaining.
% FOUNDING_PROBLEM_CORROBORATION: Modern historians of Japanese religion operating from analytical seats outside the benefiting institutions, as well as Meiji-era bureaucrats who documented the syncretic system as a historical accretion without theological necessity, attest that the original conditions producing the entanglement were obsolete long before the arrangement was dissolved.
narrative_ontology:disappearance_verdict(simultaneous_veneration__pragmatic_incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__pragmatic_incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__pragmatic_incoherence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(simultaneous_veneration__pragmatic_incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the arrangement extracted both economic surplus (dual fees to shrine-temple complexes) and cognitive labor (suppressing contradiction) from its targets. Suppression is moderate (0.48) because the constraint was not sustained by active state violence but by institutional interlock and identity fusion â exit was blocked socially rather than legally. Theater_ratio is very high (0.78) because the elaborate honji-suijaku doctrinal apparatus functioned primarily as performative cover for an underlying incoherence; the theological work increased over time while the practical arrangement remained contradictory. Accessibility_collapse (0.65) reflects that alternatives (pure Shinto or pure Buddhist affiliation) were theologically conceivable but institutionally unreachable for most worshippers. Resistance (0.35) captures the marginal but persistent critique from kokugaku and reformist circles that was suppressed until the Meiji rupture.
 *
 * PERSPECTIVAL GAP:
 *   From the shrine-temple complex seat, the arrangement appeared as a functional tradition with legitimate theological grounding; from the parish clergy seat, it appeared as daily cognitive dissonance without resolution; from the worshipper seat, it appeared as an unquestioned custom whose costs were invisible because no alternative was socially available. The engine computes these divergences from the structural data rather than from any authored classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine-temple complexes and syncretic elite clergy are declared beneficiaries: they collected rents and prestige from the arrangement, giving them low directionality (structurally subsidized by the constraint). Common worshippers and parish clergy are declared victims: they bore the economic and cognitive costs, with identity_locked exit options placing them near the full-target end. The identity_lock mechanism is crucial â for worshippers, religious and communal identity were fused, so the effective extraction was amplified beyond what a purely economic transfer would suggest.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the syncretic regime as a Rope (domain-partition coordination) or Mountain (ontological fusion as natural law). By claiming snare and independently authoring high theater and extraction metrics, the story captures the divergence between the coordination narratives offered by sibling readings and the structural reality of extraction through suppressed contradiction. The founding problem (Heian-period entanglement) is dead, yet the arrangement persisted â a classic mandatrophy signal that the constraint had outlived its original function and was maintained by institutional inertia and performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_of_persistence,
    'Was the syncretic regime sustained by positive institutional incentives (rent capture by temples and shrines) or primarily by the absence of negative enforcement pressure (no authority forced theological clarification)?',
    'Comparative analysis of religious economies in other East Asian societies where state enforcement did or did not mandate theological clarification.',
    'If sustained primarily by absence of pressure, the constraint is more accurately classified as inertial performance than as actively enforced extraction; this would lower suppression and raise theater_ratio, shifting the computed type toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_of_persistence, empirical, 'Whether persistence was active rent-seeking or passive absence of enforcement').

omega_variable(
    meiji_rupture_nature,
    'Was the Meiji shinbutsu-bunri an external imposition that created incoherence where a stable syncretism existed, or a revelation of incoherence that was always latent?',
    'Micro-historical study of pre-Meiji parish records and clerical writings to measure the prevalence of acknowledged contradiction versus seamless syncretic practice.',
    'If incoherence was latent, the pragmatic reading is structurally vindicated and the sibling readings are falsified as descriptive claims; if the separation created incoherence, the domain partition or ontological fusion readings gain empirical support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_rupture_nature, empirical, 'Whether Meiji separation revealed or created incoherence').

omega_variable(
    kernel_reading_contest,
    'Which reading of the simultaneous_veneration kernel best describes the structural reality, and can empirical historiography adjudicate among them?',
    'Corpus-wide comparison of all three sibling constraints against independent historical datasets (economic, doctrinal, social-network).',
    'Adjudication would collapse the kernel from three competing constraints to one stable classification, or confirm that the same historical label covers multiple structurally distinct constraints (Îµ-invariance violation in the historiography itself).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Empirical adjudication among three competing readings of the same kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__pragmatic_incoherence_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(simu_tr_t6, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement(simu_tr_t12, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 12, 0.48).
narrative_ontology:measurement(simu_tr_t18, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 18, 0.58).
narrative_ontology:measurement(simu_tr_t24, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 24, 0.67).
narrative_ontology:measurement(simu_tr_t30, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 30, 0.74).
narrative_ontology:measurement(simu_tr_t35, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 35, 0.78).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(simu_be_t6, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(simu_be_t12, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(simu_be_t18, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 18, 0.61).
narrative_ontology:measurement(simu_be_t24, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(simu_be_t30, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(simu_be_t35, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 35, 0.72).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(simultaneous_veneration__pragmatic_incoherence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, ontological_fusion_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'simultaneous veneration' covers three structurally distinct constraints: domain-partition coordination, ontological-fusion identity coordination, and pragmatic-incoherence extraction. Each has a different epsilon, beneficiary structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
