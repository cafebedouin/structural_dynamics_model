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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: shinbutsu_coexistence_commitment__incoherent_bundle_reading
 *   human_readable: Shinbutsu-shugo as Incoherent Bundle Maintained Through Ambiguity
 *   domain: religious/philosophical/historical
 *
 * SUMMARY:
 *   Shinbutsu-shugo (the coexistence of Shinto and Buddhism) was presented
 *   during the Edo and early Meiji periods as a coherent system in which kami
 *   and Buddhist deities inhabited the same spiritual universe without
 *   contradiction. This reading rejects the coherence narratives (honji
 *   suijaku, domain partition, syncretic fusion) as cover stories for what
 *   was structurally an incoherent bundle sustained only by institutional
 *   power and deliberate avoidance of categorical questions. The bundle
 *   permitted two competing institutional orders (Buddhist temples and Shinto
 *   priesthoods) to share authority, income, and ritual responsibility over
 *   the same communities without resolving their theological differences.
 *   When Meiji state pressure forced the bundle to choose (bunri: separating
 *   kami worship from Buddhist institutional authority), the entire
 *   arrangement collapsed — not because bunri solved a latent problem, but
 *   because it revealed that the system's coherence had always been theater
 *   and the underlying structure was incoherence maintained by power. This
 *   reading is one of three competing readings of the
 *   shinbutsu_coexistence_commitment kernel; the other readings
 *   (syncretic_fusion and domain_partition) claim coherence narratives
 *   actually unified the system.
 *
 * KEY AGENTS:
 *   - institutional_buddhism: Buddhist temples and priestly networks that extracted authority and income by administering Shinto ritual without doctrinal claim
 *   - shinto_priesthoods: Shinto priests that maintained autonomy and authority under the shelter of the bundle's ambiguity, neither independent nor absorbed
 *   - rural_communities: Communities that paid for dual-practice maintenance and bore the costs of the bundle without understanding its lack of coherence
 *   - meiji_centralists: State builders and Shinto purists excluded from the bundle's governance, seeking to impose coherence through bunri
 *   - scholarly_interpreters: Intellectuals offering coherence narratives (honji suijaku, etc.) that enabled the bundle's continuation by providing interpretive cover
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.62).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.71).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, piton).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "Shinbutsu-shugo as Incoherent Bundle Maintained Through Ambiguity").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "religious/philosophical/historical").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '37626e34-9089-4105-a69d-734f99f4473b').
narrative_ontology:cs_kernel_codification('37626e34-9089-4105-a69d-734f99f4473b', implicit).
narrative_ontology:cs_authority_grounding('37626e34-9089-4105-a69d-734f99f4473b', extraction).
narrative_ontology:cs_interpretation_layer_present('37626e34-9089-4105-a69d-734f99f4473b').
narrative_ontology:cs_reading_relation('37626e34-9089-4105-a69d-734f99f4473b', shinbutsu_coexistence_commitment__shinbutsu_syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('37626e34-9089-4105-a69d-734f99f4473b', shinbutsu_coexistence_commitment__shinbutsu_domain_partition_reading, forecloses).
narrative_ontology:cs_axiom('37626e34-9089-4105-a69d-734f99f4473b', foundational, coherence_is_performative_not_substantive).
narrative_ontology:cs_axiom_status(coherence_is_performative_not_substantive, holdable).
narrative_ontology:cs_axiom_grounding('37626e34-9089-4105-a69d-734f99f4473b', coherence_is_performative_not_substantive, empirically_contingent).
narrative_ontology:cs_axiom('37626e34-9089-4105-a69d-734f99f4473b', foundational, institutional_power_sustains_ambiguity_deliberately).
narrative_ontology:cs_axiom_status(institutional_power_sustains_ambiguity_deliberately, holdable).
narrative_ontology:cs_axiom_grounding('37626e34-9089-4105-a69d-734f99f4473b', institutional_power_sustains_ambiguity_deliberately, empirically_contingent).
narrative_ontology:cs_reference_frame('37626e34-9089-4105-a69d-734f99f4473b', incoherent_coexistence_maintained_by_power).
narrative_ontology:cs_drift_state('37626e34-9089-4105-a69d-734f99f4473b', meiji_bunri_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('37626e34-9089-4105-a69d-734f99f4473b', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, institutional_buddhism).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinto_priesthoods).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, rural_communities).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, rural_communities).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__incoherent_bundle_reading, religious_coexistence_possible_under_ambiguity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buddhist temples and priestly networks administered Shinto shrines and kami-worship rituals, extracting income and authority without requiring doctrinal justification for the coexistence. The arrangement permitted Buddhist expansion into formerly Shinto domains while avoiding direct institutional competition. Priests maintained coherence narratives (honji suijaku) to justify the arrangement, but these narratives served as interpretive theater, not actual reconciliation. Exit from this arrangement would mean surrendering ritual authority over kami-worship and the income it provided.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, institutional_buddhism, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, institutional_buddhism, beneficiary).

% Shinto priests maintained institutional identity and community authority by performing kami-worship under the bundle's umbrella of ambiguity. They neither asserted independence from Buddhism nor fully integrated into it; this middle position was enabled by the bundle's deliberate avoidance of categorical claims. They benefited from not competing directly with the more powerful Buddhist institutions while maintaining autonomous ritual roles. Professional identity fused with the bundle's incoherent structure: exit would require choosing between Buddhist subordination or institutional fragility.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinto_priesthoods, agenda_setter,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinto_priesthoods, beneficiary).

% Communities participated in both kami-worship and Buddhist practices without any requirement that they understand how the two cohere. Families addressed agricultural, fertility, life-cycle, and death-related concerns through whichever tradition was locally effective or customary. They bore the cost of maintaining dual-practice ritual sites (paying both Buddhist temples and Shinto shrines) and contributed labor and goods to both. They had no exit: regional social structure embedded both traditions, and attempting to practice only one would incur community sanctions and loss of ritual coverage.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, rural_communities, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, rural_communities, beneficiary).

% State builders and Shinto purists sought a coherent state mythology grounded in Shinto purism and imperial descent-from-Amaterasu (Kokugaku revival, state Shinto ideology). The bundle's institutional entrenchment and Buddhist power directly obstructed this project. They were structurally excluded from the constraint's governance and administration, forced to work around existing arrangements. Their solution was bunri (separation of kami-worship from Buddhist institutional control), which would require dismantling the bundle entirely.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_centralists, excluded,
    institutional, generational, trapped, national).

% Religious scholars, court intellectuals, Buddhist exegetes, and philosophical schools documented and theorized shinbutsu-shugo. Honji suijaku scholars produced elaborate philosophical frameworks suggesting that kami were local manifestations of universal Buddhist truths, providing interpretive coherence narratives. These narratives enabled the continuation of incoherence by creating the appearance of coherence. Scholars occupied an analytical seat, neither collecting from nor paying into the arrangement, but their work sustained the constraint by producing theater.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, scholarly_interpreters, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permitted two institutionally distinct and theologically incompatible religious orders (Buddhism and Shinto) to coexist and share ritual authority over the same communities, phenomena, and lifecycle events without resolving their fundamental ontological differences. This avoided institutional warfare that would have produced winner-take-all dynamics and permitted both orders to entrench themselves.
% TRANSFER_FUNCTION: Transferred ritual authority, social status, and economic resources from rural communities to institutional centers of both Buddhism and Shinto. Communities paid dual-practice maintenance costs (supporting both temples and shrines) and labor contributions; institutional orders extracted authority over kami-worship, ancestral rites, agricultural blessing, and death rituals without coherent doctrinal justification for why both had legitimate claims.
% ABSENT_VOICES: Meiji state builders seeking coherent state ideology, Shinto purists insisting on institutional independence, Buddhist universalists seeking clear doctrinal positions, rational philosophers demanding categorical clarity, and communities that might have demanded transparency about the bundle's incoherence. These voices were structurally excluded by the bundle's mechanism: asking categorical questions would have collapsed the arrangement.
% DISAPPEARANCE_RATIONALE: The constraint's sudden collapse during Meiji bunri immediately reorganized Japan's entire religious landscape. Buddhist institutional power shattered (temples lost shrine administration, lost ritual authority over kami-worship, lost substantial income); Shinto priesthoods underwent radical state consolidation and ideological reorientation; rural communities lost flexibility to maintain dual practice and were forced to navigate state-imposed separation; the entire social structure grounded in the bundle's incoherent coexistence reorganized around the new coherence narrative (state Shinto + domesticated Buddhism). The arrangement's disappearance reveals it was not natural law but institutional structure.
% FOUNDING_PROBLEM: Japan's indigenous religious system (Shinto kami-worship) encountered an institutionally powerful imported system (Buddhism); both had competed for authority and resources; neither wished to face direct institutional elimination; continued coexistence without unity became operational necessity.
% FOUNDING_PROBLEM_CORROBORATION: Meiji government documents and Kokugaku scholars explicitly attest that the founding problem (competitive coexistence) had been solved by the 18th century through routine institutional accommodation — not by the bundle's coherence but by institutional entrenchment and power equilibrium. Modern religious historians outside the benefiting institutions (Grapard, Teeuws, Kuroda, others) attest the bundle never actually solved the founding problem through coherence; it simply displaced and suppressed the question of coherence. The problem disappeared not because the bundle sustained coexistence peacefully, but because the state imposed bunri (a forced choice), revealing that coexistence had become a power arrangement, not a problem-solving mechanism.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'none', 1).

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
 *   Extractiveness (0.62 at interval end) is moderate: the bundle extracts authority and income from communities, but neither institutional order maximizes extraction alone — the system's survival depends on maintaining both. Suppression (0.71) is higher because the bundle's persistence depends on actively suppressing categorical questions and alternatives: Shinto independence, Buddhist-exclusive authority, and rational coherence demands are all suppressed. Theater ratio (0.68, highest of the three metrics) is the diagnostic signature of a piton: the bundle's primary function (coexistence without institutional warfare) becomes increasingly performative over time as the institutions entrench. The measurement series shows theater rising while extractiveness plateaus — a classic piton pattern: the maintenance effort increases while the coordination benefit declines. Accessibility collapse (0.45) is low because alternatives existed (Meiji bunri revealed alternatives were available, just suppressed); resistance (0.58) is moderate because institutions benefited from the arrangement even if it was incoherent. The single shared time grid captures the bundle's slow entropy growth: suppression and theater must increase to maintain a system that has no intrinsic coherence.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (institutional Buddhism and Shinto priesthoods) should compute differently from the payer seats (rural communities). From the institutional perspective, the bundle was a working arrangement permitting coexistence and dual extraction without direct competition — a tangled_rope or rope depending on how much one emphasizes coordination vs. extraction. From the community perspective, the bundle's incoherence was confusing, its cost was unavoidable, and its persistence depended on power (institutional gatekeeping of ritual authority). The engine computes this divergence from the structural data — the institutional actors could claim coherence through interpretive frameworks, while the payer seats experienced only extraction and ambiguity.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional Buddhism and Shinto priesthoods are beneficiaries (extract from communities, maintain ritual authority, avoid direct competition) but also agenda-setters (they maintain the bundle's ambiguity through institutional power and interpretive frameworks). Rural communities are payers and beneficiaries: they pay for dual-practice maintenance and bear suppression of alternatives, but also benefit from flexibility in addressing life-cycle and agricultural concerns through whichever tradition served locally. Meiji centralists are excluded: they would dismantle the bundle through bunri. The directionality from the payer seats tilts toward target (high d): communities have trapped exit, must maintain both traditions, and lack exit alternatives. From the beneficiary seats, d is lower: institutions could switch strategy if suppression weakened, making their exit constrained rather than trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading diagnoses the bundle as a piton: an arrangement maintained primarily through theater and institutional inertia, no longer serving its founding problem (coexistence without institutional warfare) but persisting because fixing it costs more than any single party pays. The founding problem (institutional coexistence) was dead by the late Edo period — both orders had entrenched and coexistence was routine, not contested. Yet the bundle persisted, now sustained by interpretive theater (honji suijaku and coherence narratives making the incoherent appear unified) and by institutional extraction (both orders profiting from ritual authority). The Meiji bunri is not solving a coordinated problem; it is revealing that the problem had been solved decades earlier and the bundle had become pure performance. The constraint's classification as piton is strengthened by theater rising (from 0.48 to 0.68) while extractiveness plateaus (0.48 to 0.62), a signature of maintenance burden exceeding functional value.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incoherence_vs_coherence_narratives,
    'Was the bundle maintained as genuinely incoherent, or did honji suijaku and other interpretive frameworks constitute a real coherence that modern scholarship dismisses too readily?',
    'Close historical reading: do honji suijaku texts claim to RESOLVE the kami-Buddha ontological relationship, or do they merely ASSERT compatibility without providing explanation? What happens to coherence claims when queried explicitly — when sources are asked whether kami ARE Buddhist deities or only sometimes function as such? Do Edo-period intellectuals debate coherence, or is debate only over interpretation?',
    'If honji suijaku was genuine coherence, the syncretic_fusion_reading becomes more plausible and this reading collapses. If honji suijaku was interpretive theater (asserting coherence without delivering it), this reading is strengthened: the system''s survival depended on performative coherence, not actual unity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_vs_coherence_narratives, empirical, 'Whether the bundle had genuine coherence or only coherence narratives masking incoherence.').

omega_variable(
    institutional_extraction_vs_religious_coexistence,
    'Did the bundle''s institutional structure primarily serve peaceful coexistence (two orders sharing space without competition) or primarily serve dual extraction (two orders each monetizing the same rituals and communities)?',
    'Revenue audit: do Buddhist temples and Shinto shrines at the same sites extract differentiated revenue streams (each charging separately for different services), or integrated revenue (genuinely shared collection)? If revenue is fully integrated, the bundle coordinated services. If revenue is separate, each order was independently extracting, using the bundle as cover.',
    'If dual extraction is primary, this reading (piton with high theater and moderate extraction) holds. If coexistence is primary (genuinely coordinating shared ritual provision), the bundle was a working tangled_rope or rope, not an incoherent assembly maintained through power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_extraction_vs_religious_coexistence, empirical, 'Whether the bundle served peaceful coexistence or dual-order extraction.').

omega_variable(
    ambiguity_deliberately_maintained,
    'Did institutional actors deliberately maintain ambiguity because it served their interests, or did ambiguity persist because no actor had power to impose coherence?',
    'Historical analysis of coherence projects: did Buddhist schools, Shinto reformers, court intellectuals, or state actors repeatedly attempt to impose unity and face institutional resistance? If yes, ambiguity was deliberately sustained (each side blocked coherence that would subordinate it). If such projects never arose, ambiguity may have been default rather than sustained.',
    'If deliberately maintained, this reading''s core claim holds: ambiguity was a feature both institutional orders preserved because coherence would subordinate one to the other. If default, the bundle was not actively maintaining incoherence — it was simply never developing coherence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ambiguity_deliberately_maintained, conceptual, 'Whether ambiguity was deliberately preserved by institutional power or naturally persistent.').

omega_variable(
    bunri_revealing_vs_creating_incoherence,
    'Did Meiji bunri (separation) REVEAL incoherence that was always structural to the bundle, or did bunri CREATE incoherence by forcing an artificial separation of what had been genuinely unified?',
    'Counterfactual analysis: if Meiji pressure had not occurred, would the bundle have continued indefinitely on the same trajectory, or do pre-Meiji sources show increasing internal strain, calls for resolution, or mounting contradictions? If strain is evident, bunri revealed preexisting incoherence. If no strain appears, bunri may have created the incoherence.',
    'If bunri revealed existing incoherence, this reading holds: the system was always incoherent and its persistence depended on power to suppress categorical questions. If bunri created the incoherence, the syncretic_fusion_reading better describes pre-Meiji reality as genuinely coherent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bunri_revealing_vs_creating_incoherence, conceptual, 'Whether Meiji bunri revealed structural incoherence or created it through forced separation.').

omega_variable(
    suppression_structural_or_internalized,
    'The measured suppression (0.71) reflects enforcement of the bundle''s ambiguity — but was this suppression primarily structural (institutional gatekeeping of alternatives) or internalized (communities and priests accepting ambiguity as legitimate)?',
    'Post-bunri trajectory: after the state imposed separation, did communities and priests mount resistance to coherence demands, or did they rapidly reorganize? Sustained resistance indicates suppression was internalized (acceptance legitimated the constraint). Rapid compliance indicates suppression was structural (enforcement was what held it in place).',
    'If suppression was primarily internalized, the constraint''s extractiveness was masked because ambiguity felt natural or correct to participants. If structural, the bundle was vulnerable precisely because it lacked legitimacy beyond institutional enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_or_internalized, empirical, 'Whether suppression was structural enforcement or internalized legitimation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement_basis(shin_tr_t0, observed).
narrative_ontology:measurement(shin_tr_t5, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement_basis(shin_tr_t5, observed).
narrative_ontology:measurement(shin_tr_t10, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 10, 0.56).
narrative_ontology:measurement_basis(shin_tr_t10, observed).
narrative_ontology:measurement(shin_tr_t15, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 15, 0.6).
narrative_ontology:measurement_basis(shin_tr_t15, observed).
narrative_ontology:measurement(shin_tr_t20, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 20, 0.64).
narrative_ontology:measurement_basis(shin_tr_t20, observed).
narrative_ontology:measurement(shin_tr_t25, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 25, 0.66).
narrative_ontology:measurement_basis(shin_tr_t25, observed).
narrative_ontology:measurement(shin_tr_t30, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 30, 0.67).
narrative_ontology:measurement_basis(shin_tr_t30, observed).
narrative_ontology:measurement(shin_tr_t35, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 35, 0.68).
narrative_ontology:measurement_basis(shin_tr_t35, observed).
narrative_ontology:measurement(shin_tr_t40, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 40, 0.68).
narrative_ontology:measurement_basis(shin_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(shin_be_t0, observed).
narrative_ontology:measurement(shin_be_t5, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(shin_be_t5, observed).
narrative_ontology:measurement(shin_be_t10, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(shin_be_t10, observed).
narrative_ontology:measurement(shin_be_t15, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(shin_be_t15, observed).
narrative_ontology:measurement(shin_be_t20, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(shin_be_t20, observed).
narrative_ontology:measurement(shin_be_t25, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(shin_be_t25, observed).
narrative_ontology:measurement(shin_be_t30, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(shin_be_t30, observed).
narrative_ontology:measurement(shin_be_t35, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 35, 0.62).
narrative_ontology:measurement_basis(shin_be_t35, observed).
narrative_ontology:measurement(shin_be_t40, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(shin_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(shin_su_t0, observed).
narrative_ontology:measurement(shin_su_t5, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(shin_su_t5, observed).
narrative_ontology:measurement(shin_su_t10, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(shin_su_t10, observed).
narrative_ontology:measurement(shin_su_t15, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(shin_su_t15, observed).
narrative_ontology:measurement(shin_su_t20, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(shin_su_t20, observed).
narrative_ontology:measurement(shin_su_t25, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(shin_su_t25, observed).
narrative_ontology:measurement(shin_su_t30, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(shin_su_t30, observed).
narrative_ontology:measurement(shin_su_t35, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(shin_su_t35, observed).
narrative_ontology:measurement(shin_su_t40, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(shin_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__incoherent_bundle_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_bunri_state_ideology).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the shinbutsu_coexistence_commitment kernel. The syncretic_fusion_reading claims honji suijaku philosophy constituted genuine coherence; the domain_partition_reading claims separate ontological domains needed no unification. This reading claims no coherence existed and was only theater. All three readings link bidirectionally through network.affects_constraints. The upstreammost claim (coherence narratives enabled the arrangement) is shared by all readings; they diverge on whether coherence was real, functional, or performative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__incoherent_bundle_reading, powerless, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
