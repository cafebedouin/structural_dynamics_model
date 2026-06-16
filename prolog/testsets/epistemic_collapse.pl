% ============================================================================
% CONSTRAINT STORY: epistemic_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_collapse, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: epistemic_collapse
 *   human_readable: Irreversible Visual Evidentiary Collapse
 *   domain: technology_governance/information_epistemology
 *
 * SUMMARY:
 *   This constraint models the epistemic_collapse reading of visual
 *   evidentiary authority: the position that generative AI has irreversibly
 *   destroyed the ability to verify visual claims, rendering the visual
 *   system 'utterly useless' as forensic researcher Hany Farid predicts. This
 *   is ONE reading of a contested kernel—sibling readings (indexical_realism,
 *   distributed_verification, post_evidentiary) model alternative structural
 *   relationships to the same technological shift. The claim/metric
 *   independence is preserved: claimed_type is 'snare' (the structural
 *   diagnosis this reading makes), while metrics describe the actual
 *   extractiveness, suppression, and theatrical maintenance the constraint
 *   exhibits.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_collapse, 0.82).
domain_priors:suppression_score(epistemic_collapse, 0.91).
domain_priors:theater_ratio(epistemic_collapse, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_collapse, extractiveness, 0.82).
narrative_ontology:constraint_metric(epistemic_collapse, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(epistemic_collapse, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(epistemic_collapse, accessibility_collapse, 0.94).
narrative_ontology:constraint_metric(epistemic_collapse, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_collapse, snare).
narrative_ontology:human_readable(epistemic_collapse, "Irreversible Visual Evidentiary Collapse").
narrative_ontology:topic_domain(epistemic_collapse, "technology_governance/information_epistemology").

domain_priors:requires_active_enforcement(epistemic_collapse).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(epistemic_collapse, '961160c1-a6a6-4e00-82a0-29f74912e343').
narrative_ontology:cs_kernel_codification('961160c1-a6a6-4e00-82a0-29f74912e343', distributed).
narrative_ontology:cs_authority_grounding('961160c1-a6a6-4e00-82a0-29f74912e343', distributed).
narrative_ontology:cs_reading_relation('961160c1-a6a6-4e00-82a0-29f74912e343', visual_evidentiary_authority__indexical_realism, coexists_with).
narrative_ontology:cs_reading_relation('961160c1-a6a6-4e00-82a0-29f74912e343', visual_evidentiary_authority__distributed_verification, coexists_with).
narrative_ontology:cs_reading_relation('961160c1-a6a6-4e00-82a0-29f74912e343', visual_evidentiary_authority__post_evidentiary, influences).
narrative_ontology:cs_axiom('961160c1-a6a6-4e00-82a0-29f74912e343', foundational, verification_impossibility_at_scale).
narrative_ontology:cs_axiom_status(verification_impossibility_at_scale, holdable).
narrative_ontology:cs_axiom_grounding('961160c1-a6a6-4e00-82a0-29f74912e343', verification_impossibility_at_scale, empirically_contingent).
narrative_ontology:cs_axiom('961160c1-a6a6-4e00-82a0-29f74912e343', foundational, generation_detection_gap_irreversible).
narrative_ontology:cs_axiom_status(generation_detection_gap_irreversible, holdable).
narrative_ontology:cs_axiom_grounding('961160c1-a6a6-4e00-82a0-29f74912e343', generation_detection_gap_irreversible, empirically_contingent).
narrative_ontology:cs_reference_frame('961160c1-a6a6-4e00-82a0-29f74912e343', pre_generative_visual_authority).
narrative_ontology:cs_drift_state('961160c1-a6a6-4e00-82a0-29f74912e343', post_diffusion_model_era, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('961160c1-a6a6-4e00-82a0-29f74912e343', '').
narrative_ontology:cs_kernel_id(epistemic_collapse, visual_evidentiary_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_collapse, generative_ai_platforms).
narrative_ontology:constraint_beneficiary(epistemic_collapse, disinformation_actors).
narrative_ontology:constraint_beneficiary(epistemic_collapse, authoritarian_regimes).
narrative_ontology:constraint_victim(epistemic_collapse, journalism_institutions).
narrative_ontology:constraint_victim(epistemic_collapse, judicial_systems).
narrative_ontology:constraint_victim(epistemic_collapse, democratic_publics).
narrative_ontology:constraint_victim(epistemic_collapse, forensic_investigators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(epistemic_collapse, verification_technology_vendors).
narrative_ontology:constraint_victim(epistemic_collapse, verification_technology_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy increasingly sophisticated image and video generation models at scale, with minimal verification infrastructure. Set the pace of capability advancement that outstrips detection. Profit from engagement and compute sales regardless of epistemic consequences. Could implement watermarking or provenance standards but face competitive pressure not to.
narrative_ontology:constraint_stakeholder(epistemic_collapse, generative_ai_platforms, agenda_setter,
    institutional, biographical, arbitrage, global).

% Gain unprecedented ability to fabricate convincing visual evidence at negligible cost. Operate in the gap between generation capability and detection infrastructure. Benefit from the collapse of visual trust without bearing costs of maintaining it.
narrative_ontology:constraint_stakeholder(epistemic_collapse, disinformation_actors, beneficiary,
    organized, immediate, mobile, global).

% Exploit visual uncertainty to discredit authentic documentation of abuses while manufacturing exculpatory evidence. The collapse of visual authority eliminates a constraint on state violence that depended on documentary proof.
narrative_ontology:constraint_stakeholder(epistemic_collapse, authoritarian_regimes, beneficiary,
    institutional, generational, constrained, national).

% Must verify every visual claim in an environment where verification is structurally impossible within news cycles. Cannot abandon visual evidence without abandoning core function, but publishing unverified material destroys credibility. Bear the full cost of epistemic collapse while lacking tools to navigate it.
narrative_ontology:constraint_stakeholder(epistemic_collapse, journalism_institutions, payer,
    organized, biographical, identity_locked, global).

% Depend on visual evidence for criminal prosecution, civil litigation, and rights documentation. Face systematic contamination of evidence chains as fabrication becomes indistinguishable from authentic documentation. Must either exclude all visual evidence or accept systematic unreliability.
narrative_ontology:constraint_stakeholder(epistemic_collapse, judicial_systems, payer,
    institutional, generational, identity_locked, national).

% Lose the ability to distinguish authentic documentation from fabrication in politically consequential contexts. Cannot exit the visual information environment but can no longer trust it. Democratic accountability mechanisms that depend on documentary evidence collapse.
narrative_ontology:constraint_stakeholder(epistemic_collapse, democratic_publics, payer,
    organized, biographical, trapped, global).

% Develop detection methods that are systematically outpaced by generation capabilities. Professional identity depends on providing verification services that are becoming structurally impossible. Each detection advance is defeated within months by the next generation model.
narrative_ontology:constraint_stakeholder(epistemic_collapse, forensic_investigators, payer,
    moderate, biographical, identity_locked, global).

% Sell detection and watermarking solutions to institutions desperate for verification tools. Profit from the crisis while unable to deliver solutions that keep pace with generation. Face reputational collapse as each solution is defeated but continue extracting revenue from institutional demand.
narrative_ontology:constraint_stakeholder(epistemic_collapse, verification_technology_vendors, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(epistemic_collapse, verification_technology_vendors, payer).

% Document the systematic failure of detection methods and the structural impossibility of verification at scale. Provide the empirical basis for the collapse diagnosis but lack institutional power to implement systemic remedies.
narrative_ontology:constraint_stakeholder(epistemic_collapse, content_authentication_researchers, observer,
    moderate, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This reading asserts there is no coordination function remaining—visual evidentiary systems existed to coordinate truth claims across institutions, but that function has been destroyed by the generation-detection capability gap.
% TRANSFER_FUNCTION: Transfers epistemic authority from truth-dependent institutions (journalism, courts, democratic accountability) to actors who benefit from uncertainty (disinformation networks, authoritarian regimes, platforms profiting from engagement regardless of truth value).
% ABSENT_VOICES: Future generations who will inherit information ecosystems with no reliable visual record. Populations in conflict zones whose documentation of atrocities becomes systematically contestable. Marginalized communities whose visual evidence of discrimination can be dismissed as fabrication.
% DISAPPEARANCE_RATIONALE: If generative AI capabilities vanished overnight, visual evidence would regain evidentiary authority within months. Journalism could resume using photographic documentation without systematic verification overhead. Courts could admit visual evidence without presuming contamination. The institutions currently bearing collapse costs would recover their epistemic foundations.
% FOUNDING_PROBLEM: Early digital manipulation was detectable through forensic analysis; institutions needed tools to distinguish authentic from altered images in adversarial contexts.
% FOUNDING_PROBLEM_CORROBORATION: Hany Farid (forensic researcher, outside beneficiary set) attests the founding problem is dead: detection cannot keep pace with generation, making the visual system 'utterly useless.' DARPA's Semantic Forensics program documentation shows systematic detection failure. Academic literature from content authentication conferences documents the widening capability gap. No party outside the beneficiary set claims the founding verification problem remains solvable.
narrative_ontology:disappearance_verdict(epistemic_collapse, world_rearranges).
narrative_ontology:founding_problem_status(epistemic_collapse, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(epistemic_collapse, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-16',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(epistemic_collapse, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_collapse_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because truth-dependent institutions bear systematic costs (verification overhead, credibility loss, evidence exclusion) while beneficiaries (platforms, disinformation actors, authoritarian regimes) capture value from uncertainty without bearing verification costs. Suppression is very high (0.91) because institutions cannot exit: journalism cannot abandon visual evidence without ceasing to function; courts cannot exclude all visual evidence without collapsing entire categories of prosecution. Theater ratio is substantial (0.68) because verification services continue to be sold and deployed despite systematic failure—the performance of verification persists after the function has collapsed. Accessibility_collapse is very high (0.94) because once the generation-detection gap is understood, no alternative verification method remains viable at relevant timeframes. Resistance is high (0.73) because truth-dependent institutions actively resist the collapse diagnosis and continue investing in detection, but resistance does not restore function.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (platforms) should compute as rope or scaffold from their position: they built coordination infrastructure (content generation) that serves genuine demand. The payer seats (journalism, courts, publics) should compute as snare: they experience systematic extraction (epistemic authority loss, verification cost imposition) with identity-locked or trapped exit. The engine measures this divergence from structural data—the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Generative AI platforms are agenda-setters (control capability advancement pace, could implement provenance standards but face competitive pressure not to) with arbitrage exit—they benefit from the collapse without being trapped by it. Disinformation actors and authoritarian regimes are pure beneficiaries with mobile/constrained exit respectively. Journalism, courts, and forensic investigators are identity-locked payers: their professional identities depend on functions the constraint has made impossible, so they cannot exit without ceasing to exist. Democratic publics are trapped payers with no exit from the contaminated information environment. Verification vendors are dual-positioned: benefit from crisis demand while facing reputational collapse as solutions fail.
 *
 * MANDATROPHY ANALYSIS:
 *   This is not mandatrophy—the constraint's mandate (enable visual communication and creativity) has not outlived its function. Rather, the constraint has INVERTED its function: what was coordination infrastructure (visual documentation enabling truth claims) has become extraction infrastructure (visual generation destroying truth claims). The founding problem (distinguish authentic from altered images) is dead per corroborated testimony, but the arrangement persists and intensifies because beneficiaries profit from epistemic uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    irreversibility_claim,
    'Is the generation-detection capability gap structurally irreversible, or could cryptographic provenance or hardware attestation restore visual authority?',
    'Empirical test: widespread deployment of content authentication standards (C2PA, hardware-attested capture) with measurement of adoption rates and circumvention methods. If authenticated content becomes the norm and unauthenticated content is systematically discounted, the gap is bridgeable. If authentication is systematically circumvented or adoption fails, irreversibility is confirmed.',
    'If reversible via technical standards, this reading collapses into the indexical_realism reading and the constraint reclassifies from snare to scaffold (temporary crisis during transition to authenticated media). If irreversible, the snare classification holds and victims must adapt to permanent epistemic uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irreversibility_claim, empirical, 'Whether technical countermeasures can restore visual evidentiary authority or the collapse is permanent.').

omega_variable(
    adaptation_vs_collapse,
    'Does loss of visual authority constitute epistemic collapse, or do institutions adapt by migrating to alternative verification methods?',
    'Observation of institutional behavior over 5-10 years: if journalism, courts, and democratic accountability mechanisms develop functional alternatives to visual evidence (network verification, cryptographic trails, sensor fusion), adaptation is occurring. If these institutions experience systematic credibility loss and functional degradation, collapse is confirmed.',
    'If adaptation occurs, this reading is too pessimistic and the constraint reclassifies toward the distributed_verification or post_evidentiary readings. If collapse occurs, the snare classification and victim identification are confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_vs_collapse, empirical, 'Whether institutions collapse under epistemic uncertainty or successfully adapt to post-visual verification.').

omega_variable(
    beneficiary_intentionality,
    'Do generative AI platforms intentionally extract from epistemic collapse, or is the extraction an unintended externality of capability advancement?',
    'Discovery of internal platform communications regarding content authentication standards, watermarking deployment decisions, and competitive strategy around verification. If platforms actively resist authentication to preserve engagement regardless of epistemic consequences, intentional extraction is confirmed. If platforms attempt authentication but face insurmountable coordination problems, extraction is structural rather than intentional.',
    'Intentional extraction supports snare classification and regulatory intervention targeting platforms. Unintended extraction suggests the constraint is a coordination failure (tangled_rope) where platforms are also victims of competitive dynamics, warranting different remedies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_intentionality, empirical, 'Whether platforms intentionally profit from epistemic collapse or are trapped in competitive dynamics that produce collapse as externality.').

omega_variable(
    reading_frame_selection,
    'What structural signals distinguish the epistemic_collapse reading from the indexical_realism reading when both are consistent with current observations?',
    'This is the kernel-level omega: the readings differ on whether cryptographic provenance can restore authority at scale. Signals that would favor epistemic_collapse: authentication adoption remains below 20% after 5 years, circumvention methods proliferate faster than authentication standards, institutions continue experiencing credibility loss despite authentication availability. Signals that would favor indexical_realism: authentication adoption exceeds 60%, unauthenticated content is systematically discounted, institutional credibility stabilizes.',
    'This omega documents the reading selection itself. The choice between readings determines whether the constraint is a permanent snare (epistemic_collapse) or a temporary scaffold during transition to authenticated media (indexical_realism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_frame_selection, conceptual, 'What observable patterns distinguish this reading from its siblings when both remain structurally coherent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_collapse, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t0, epistemic_collapse, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(epis_tr_t0, observed).
narrative_ontology:measurement(epis_tr_t6, epistemic_collapse, theater_ratio, 6, 0.32).
narrative_ontology:measurement_basis(epis_tr_t6, observed).
narrative_ontology:measurement(epis_tr_t12, epistemic_collapse, theater_ratio, 12, 0.41).
narrative_ontology:measurement_basis(epis_tr_t12, observed).
narrative_ontology:measurement(epis_tr_t18, epistemic_collapse, theater_ratio, 18, 0.49).
narrative_ontology:measurement_basis(epis_tr_t18, observed).
narrative_ontology:measurement(epis_tr_t24, epistemic_collapse, theater_ratio, 24, 0.58).
narrative_ontology:measurement_basis(epis_tr_t24, observed).
narrative_ontology:measurement(epis_tr_t30, epistemic_collapse, theater_ratio, 30, 0.64).
narrative_ontology:measurement_basis(epis_tr_t30, observed).
narrative_ontology:measurement(epis_tr_t36, epistemic_collapse, theater_ratio, 36, 0.68).
narrative_ontology:measurement_basis(epis_tr_t36, projected).

% Extraction over time
narrative_ontology:measurement(epis_be_t0, epistemic_collapse, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(epis_be_t0, observed).
narrative_ontology:measurement(epis_be_t6, epistemic_collapse, base_extractiveness, 6, 0.51).
narrative_ontology:measurement_basis(epis_be_t6, observed).
narrative_ontology:measurement(epis_be_t12, epistemic_collapse, base_extractiveness, 12, 0.61).
narrative_ontology:measurement_basis(epis_be_t12, observed).
narrative_ontology:measurement(epis_be_t18, epistemic_collapse, base_extractiveness, 18, 0.69).
narrative_ontology:measurement_basis(epis_be_t18, observed).
narrative_ontology:measurement(epis_be_t24, epistemic_collapse, base_extractiveness, 24, 0.76).
narrative_ontology:measurement_basis(epis_be_t24, observed).
narrative_ontology:measurement(epis_be_t30, epistemic_collapse, base_extractiveness, 30, 0.8).
narrative_ontology:measurement_basis(epis_be_t30, observed).
narrative_ontology:measurement(epis_be_t36, epistemic_collapse, base_extractiveness, 36, 0.82).
narrative_ontology:measurement_basis(epis_be_t36, projected).

% Suppression requirement over time
narrative_ontology:measurement(epis_su_t0, epistemic_collapse, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(epis_su_t0, observed).
narrative_ontology:measurement(epis_su_t6, epistemic_collapse, suppression_requirement, 6, 0.63).
narrative_ontology:measurement_basis(epis_su_t6, observed).
narrative_ontology:measurement(epis_su_t12, epistemic_collapse, suppression_requirement, 12, 0.71).
narrative_ontology:measurement_basis(epis_su_t12, observed).
narrative_ontology:measurement(epis_su_t18, epistemic_collapse, suppression_requirement, 18, 0.78).
narrative_ontology:measurement_basis(epis_su_t18, observed).
narrative_ontology:measurement(epis_su_t24, epistemic_collapse, suppression_requirement, 24, 0.84).
narrative_ontology:measurement_basis(epis_su_t24, observed).
narrative_ontology:measurement(epis_su_t30, epistemic_collapse, suppression_requirement, 30, 0.89).
narrative_ontology:measurement_basis(epis_su_t30, observed).
narrative_ontology:measurement(epis_su_t36, epistemic_collapse, suppression_requirement, 36, 0.91).
narrative_ontology:measurement_basis(epis_su_t36, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(epistemic_collapse, indexical_realism).
narrative_ontology:affects_constraint(epistemic_collapse, distributed_verification).
narrative_ontology:affects_constraint(epistemic_collapse, post_evidentiary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the visual_evidentiary_authority kernel. The epistemic_collapse reading asserts irreversible loss of visual verification capability. Sibling readings model alternative structural relationships: indexical_realism (authority restored via cryptographic provenance), distributed_verification (authority migrates to network consensus), post_evidentiary (society adapts to post-visual epistemology). These are not different perspectives on one constraint—they are different constraints with different ε values, different victim sets, and different persistence mechanisms, linked because they are readings of the same contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(epistemic_collapse, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
