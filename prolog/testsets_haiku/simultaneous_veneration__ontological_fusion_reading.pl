% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__ontological_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__ontological_fusion_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: simultaneous_veneration__ontological_fusion_reading
 *   human_readable: Honji-Suijaku Ontological Fusion (Buddhist Institutional Reading)
 *   domain: religious/institutional/metaphysical
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the simultaneous_veneration
 *   kernel: the honji-suijaku (original essence / manifest traces)
 *   metaphysical framework that claims kami and buddhas are ontologically
 *   identical beings — kami are Buddha-manifestations at the phenomenal
 *   level, Buddhas are the hidden metaphysical reality. This reading was
 *   developed and enforced by Buddhist institutional hierarchies from roughly
 *   the Heian period onward, integrating Japan's indigenous kami traditions
 *   into Buddhist cosmology. The reading provided intellectual coherence for
 *   simultaneous worship, but at the cost of subordinating indigenous kami
 *   autonomy to Buddhist institutional interpretation. The Meiji state
 *   terminated enforcement of this reading in 1868 (shinbutsu bunri,
 *   separation of kami and Buddhism), revealing the reading's dependence on
 *   institutional power rather than metaphysical necessity. This constraint
 *   is CLAIMED as tangled_rope (genuine coordination function — resolving
 *   apparent theological incoherence — PLUS asymmetric extraction — kami
 *   autonomy subordinated to Buddhist authority) and the metrics reflect
 *   substantially extractive, actively enforced operation. Sibling readings
 *   (domain_partition and pragmatic_incoherence) would instantiate different
 *   constraints with different ε values and beneficiary/victim structures;
 *   they are NOT part of this story.
 *
 * KEY AGENTS:
 *   - Buddhist institutional hierarchy: agenda-setter, institutional power, enforces honji-suijaku interpretation monopoly, collects interpretive authority and resource flows
 *   - Indigenous kami traditions: victims, moderate power, constrained exit (identity-locked to shrine-keeping roles), lose autonomy and interpretive authority under Buddhist institutional framework
 *   - Local shrine keepers: dual-positioned (payer + beneficiary), identity-locked to kami traditions but gain Buddhist institutional legitimacy at cost of autonomy
 *   - Buddhist practitioners: beneficiaries, gain coherent metaphysical framework legitimating simultaneous practice
 *   - Competing interpretations (domain_partition, pragmatic_incoherence readings): excluded from institutional authority, marginalized as heterodox or unsophisticated
 *   - Meiji state: external authority that terminated enforcement and revealed constraint's power-dependence
 *   - Comparative scholars: analysts examining the reading from outside both traditions, producing corroboration from outside benefiting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, 0.72).
domain_priors:suppression_score(simultaneous_veneration__ontological_fusion_reading, 0.68).
domain_priors:theater_ratio(simultaneous_veneration__ontological_fusion_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__ontological_fusion_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__ontological_fusion_reading, "Honji-Suijaku Ontological Fusion (Buddhist Institutional Reading)").
narrative_ontology:topic_domain(simultaneous_veneration__ontological_fusion_reading, "religious/institutional/metaphysical").

domain_priors:requires_active_enforcement(simultaneous_veneration__ontological_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__ontological_fusion_reading, '2088c2cf-e7d5-45db-9511-d64afbff7c18').
narrative_ontology:cs_kernel_codification('2088c2cf-e7d5-45db-9511-d64afbff7c18', fixed_text).
narrative_ontology:cs_authority_grounding('2088c2cf-e7d5-45db-9511-d64afbff7c18', lineage).
narrative_ontology:cs_interpretation_layer_present('2088c2cf-e7d5-45db-9511-d64afbff7c18').
narrative_ontology:cs_reading_relation('2088c2cf-e7d5-45db-9511-d64afbff7c18', simultaneous_veneration__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('2088c2cf-e7d5-45db-9511-d64afbff7c18', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('2088c2cf-e7d5-45db-9511-d64afbff7c18', foundational, kami_buddha_ontological_identity).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_identity, holdable).
narrative_ontology:cs_axiom_grounding('2088c2cf-e7d5-45db-9511-d64afbff7c18', kami_buddha_ontological_identity, deontological).
narrative_ontology:cs_axiom('2088c2cf-e7d5-45db-9511-d64afbff7c18', foundational, hierarchical_manifestation_structure).
narrative_ontology:cs_axiom_status(hierarchical_manifestation_structure, holdable).
narrative_ontology:cs_axiom_grounding('2088c2cf-e7d5-45db-9511-d64afbff7c18', hierarchical_manifestation_structure, conventional).
narrative_ontology:cs_reference_frame('2088c2cf-e7d5-45db-9511-d64afbff7c18', unified_buddhist_metaphysical_framework).
narrative_ontology:cs_drift_state('2088c2cf-e7d5-45db-9511-d64afbff7c18', meiji_state_intervention, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('2088c2cf-e7d5-45db-9511-d64afbff7c18', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_traditions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_practitioners).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, local_shrine_keepers).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, local_shrine_keepers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buddhist temples and monastic institutions claim authority to interpret both kami and Buddhist phenomena through honji-suijaku metaphysics. They set the official reading of what kami are (manifestations of Buddhas), train monks and scholars in this interpretation, control textual authority and transmission, adjudicate disputes about proper veneration, and collect donations and patronage from both Buddhist and kami practitioners. They actively suppress competing interpretations (domain_partition, pragmatic_incoherence) as heterodox or unsophisticated. The institutional hierarchy benefits from the reading by consolidating authority over Japan's religious landscape, eliminating competing sources of spiritual authority, and generating revenue streams from both Buddhist and kami-worship contexts. They have high power (institutional control of resources, textual authority, training institutions) and multiple exit options (can shift theological framings, negotiate with state authorities, reposition institutions) — giving them arbitrage-grade escape options if the reading becomes costly.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, generational, arbitrage, national).

% Local kami shrines, regional kami deities, and indigenous spiritual practitioners are reinterpreted as Buddhist phenomena under honji-suijaku. Kami are no longer understood as autonomous spiritual entities with their own agency, authority, and purposes — they become manifestations or servant-expressions of deeper Buddhist beings. This reinterpretation subordinates kami to Buddhist institutional frameworks: kami shrines are incorporated into Buddhist temple complexes or brought under Buddhist doctrinal authority; kami veneration is reframed as one form of Buddhist practice rather than an alternative path; local spiritual leaders lose independent authority and must operate within Buddhist-approved frameworks. The cost to kami traditions is substantial: loss of interpretive autonomy, subordination of spiritual authority, incorporation into institutional hierarchies they do not control. Exit from this constraint is costly (constrained, not trapped) because the reading has become hegemonic — practitioners can theoretically reject it, but doing so means abandoning the mainstream religious framework and operating as marginal or heterodox.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_traditions, payer,
    moderate, biographical, constrained, regional).

% Buddhist-tradition practitioners benefit from honji-suijaku by receiving a coherent metaphysical framework that explains how simultaneous kami and Buddhist veneration is not incoherent or contradictory. Under this reading, worshiping at both kami shrines and Buddhist temples is theologically legitimate — kami worship is simply worship of Buddha-manifestations, so both practices express the same underlying Buddhist truth. This resolves cognitive dissonance and provides intellectual satisfaction. Practitioners also benefit from the institutional infrastructure (temples, trained clergy, ritual specialists) that Buddhist institutions provide. However, they are also constrained by the reading: their spiritual autonomy is bounded by Buddhist institutional interpretations, and they cannot easily adopt alternative framings (domain_partition or pragmatic_incoherence) without incurring institutional disapproval or social stigma.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_practitioners, beneficiary,
    organized, biographical, constrained, national).

% Local shrine keepers (kannushi) maintain ancestral kami shrines and perform kami rituals, often for generations. Under honji-suijaku, their role is reinterpreted: they are keepers of Buddhist-manifestation-kami, not independent spiritual authorities. They lose the authority to define what their kami are, what purposes they serve, and what the proper forms of veneration should be. These decisions are increasingly mediated through Buddhist institutional authority. However, shrine keepers also benefit from honji-suijaku's hegemony: their shrines gain legitimacy by association with Buddhist institutions, they can access Buddhist infrastructure and resources, and their role is preserved (even if subordinated). The identity-lock is severe: shrine keepers' identity, career path, and social position are constituted through kami-tradition continuity. Exiting the constraint would mean abandoning their role and their identity as keepers of a lineage. This makes their exit psychologically and socially unavailable, even though the reading subordinates their authority. The directionality override (d = 0.85 for moderate power) reflects this: ordinary moderate-power agents would have d ≈ 0.5-0.6, but shrine keepers' identity-lock and lack of practical exit options drives d upward toward victim status despite some benefit-accrual.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, local_shrine_keepers, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, local_shrine_keepers, beneficiary).

% Practitioners and scholars who advocate domain_partition_reading (kami and buddhas are genuinely distinct, governing separate domains) or pragmatic_incoherence_reading (simultaneous veneration was never coherent) are structurally excluded from institutional authority and textual interpretation. Their readings are marginalized as: heterodox (deviant from Buddhist orthodoxy), unsophisticated (failing to grasp honji-suijaku's philosophical depth), or historically inaccurate (misunderstanding the true lineage of kami-Buddha relationships). They cannot easily establish competing institutions because Buddhist institutions control the resources, infrastructure, training capacity, and social legitimacy needed to maintain an alternative reading at scale. Their exit from the honji-suijaku framework is trapped: they can argue against it, but doing so means positioning themselves as marginal or heterodox within the dominant religious landscape.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, competing_interpretations, excluded,
    moderate, biographical, trapped, national).

% In 1868, the Meiji state mandated shinbutsu bunri (separation of kami and Buddhism), explicitly terminating state support for honji-suijaku fusion and reestablishing Shinto as a separate institutional framework. The state examined the claim that kami and buddhas are ontologically identical and decided the claim was incompatible with state goals of building a unified kami-centered national identity. The state's action reveals that the constraint was not a natural or necessary fact but a power-dependent arrangement that could be reversed by external authority. The state's position is analytical: it observes the constraint from outside the Buddhist-kami religious system, makes a policy judgment, and uses state power to alter the constraint's enforcement. The rapid reorganization of Japanese religion after 1868 (into separate Shinto and Buddhist institutions) demonstrates that honji-suijaku was vulnerable to external pressure and dependent on institutional continuity, not grounded in metaphysical necessity.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, meiji_state, observer,
    institutional, generational, analytical, national).

% Scholars of Japanese religion, comparative theology, and institutional history examine honji-suijaku from outside both Buddhist and Shinto traditions, asking whether it describes metaphysical fact or serves institutional interests. They produce historical analysis of when and why honji-suijaku theory emerged, examine competing readings' plausibility, analyze how state termination of enforcement affected religious practice (providing evidence about whether the reading was fragile or robust), and assess whether kami practitioners maintained coherent alternatives under different readings. Their analysis is the external corroboration source for R5 (founding_problem_corroboration): outside the benefiting parties (Buddhist institutions), scholars attest that the founding problem (theological incoherence of simultaneity) was not intrinsic but arose specifically under Buddhist metaphysical frameworks, and that practitioners have successfully maintained kami traditions under non-fusion readings both before honji-suijaku systematization and after Meiji separation.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, comparative_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:fixing_cost_class(simultaneous_veneration__ontological_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates two historically distinct religious traditions — indigenous kami veneration and imported Buddhism — into a single coherent metaphysical and institutional system. In early Heian Japan, simultaneous practice of kami worship and Buddhist practice generated apparent theological incoherence: how could one venerate both if they represent incompatible understandings of spiritual reality? Honji-suijaku theory resolves this by reinterpreting kami as manifestations of Buddhas — the underlying reality is Buddha-nature (honji), and kami are its visible traces in the world (suijaku). Under this framework, simultaneous veneration becomes coherent: one is worshiping Buddhas in both visible (kami) and explicit (Buddhist) forms. This genuine coordination function allowed practitioners to maintain both traditions within a unified metaphysical worldview.
% TRANSFER_FUNCTION: Transfers interpretive authority from autonomous local kami traditions and shrine keepers to the centralized Buddhist institutional hierarchy. Transfers spiritual legitimacy from kami as independent entities to kami as subordinate manifestations of Buddhas — the ultimate truth-source becomes Buddhist metaphysics, not kami autonomy. Transfers resources (donations, patronage, shrine revenues, ritual authority) from local shrine systems to Buddhist temples and institutions. Transfers doctrinal authority over what counts as proper kami veneration from local practitioners to Buddhist-trained clergy who can adjudicate what the kami really are. The reading creates a hierarchical relationship: kami are subsumed into Buddhism rather than remaining coequal, and Buddhist institutions become the authoritative interpreters of what kami are and how they should be properly venerated.
% ABSENT_VOICES: Practitioners and scholars who advocate domain_partition_reading (kami and buddhas are genuinely distinct, governing separate domains — this-world prosperity vs. afterlife salvation) are excluded from institutional authority and textual interpretation. Their reading offers an alternative coordination that avoids hierarchical subordination: kami govern kami-domain concerns, buddhas govern Buddhist-domain concerns, and simultaneous veneration is natural specialization rather than ontological fusion. This reading's proponents are marginalized as heterodox, unsophisticated, or historically inaccurate. Similarly excluded are pragmatic_incoherence_reading advocates (those who believe simultaneity was never coherent, only sustained by non-enforcement). Both alternative readings would testify that honji-suijaku is an institutional choice to consolidate Buddhist authority, not a metaphysical necessity, and that practitioners were capable of maintaining simultaneous practice under alternative framings. Their exclusion from institutional authority means their corroboration remains outside the official record.
% DISAPPEARANCE_RATIONALE: If honji-suijaku fusion were abandoned, Japanese religious practice would rapidly reorganize: kami traditions would re-establish interpretive autonomy, shrine practices would operate under kami-centered rather than Buddhist frameworks, local religious leaders would regain authority to define what kami are and how proper veneration should proceed, Buddhist-only practitioners would lose the framework legitimating simultaneous kami veneration (forcing either kami-practice abandonment or adoption of an alternative coordination reading), and the resource flows to Buddhist institutions would diminish as shrine donations no longer flow through Buddhist channels. The Meiji separation of 1868 provides historical confirmation: when the state terminated enforcement of honji-suijaku and mandated shinbutsu bunri, Japanese religion reorganized into separate Shinto and Buddhist institutions within a generation. This demonstrates that the reading's disappearance would not merely shift interpretations — it would rearrange the entire institutional landscape and resource-distribution system.
% FOUNDING_PROBLEM: Japan's religious landscape combined indigenous kami worship (veneration of local spiritual entities governing prosperity, agriculture, and community) with imported Buddhism (providing paths to salvation, enlightenment, and cosmic understanding). From roughly the Heian period onward (9th century), honji-suijaku theory emerged to resolve an apparent contradiction: how could one authentically venerate both if they offered different metaphysical understandings? Kami seemed to be local, this-worldly, not necessarily concerned with salvation; Buddhas seemed to be cosmic, trans-worldly, accessible only through particular practices. Honji-suijaku answered: kami and buddhas are ontologically identical — kami are Buddhas appearing in specific forms for specific regions and peoples, and Buddhism is the ultimate truth of which kami-worship is the local manifestation. This reading provided practitioners with a unified metaphysical framework that made simultaneous practice coherent and spiritually legitimate.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist institutional authorities attest that the founding problem (theological incoherence of simultaneous practice) was real and pressing, and that honji-suijaku solved it by providing metaphysical unity. Outside the benefiting parties: (1) Comparative scholars examining historical records attest that pre-Heian simultaneous practice was less systematized, and that the 'incoherence problem' arose specifically as Buddhist metaphysical frameworks became more influential and demanded theoretical reconciliation; (2) Historical analysis of post-Meiji practice shows that practitioners maintained robust simultaneous or kami-focused practice without honji-suijaku after 1868, suggesting the framework was one solution among alternatives, not a necessary resolution; (3) Domain-partition advocates (alive and documented in historical sources) attest they experienced no incoherence in maintaining kami and Buddhist traditions as separately specialized domains — suggesting the 'problem' was generated by Buddhist institutional frameworks rather than intrinsic to simultaneous practice; (4) Scholarly examination of pre-theoretical folk practice shows people maintained simultaneous veneration pragmatically without requiring metaphysical fusion — the coherence-demand came from institutional (Buddhist) philosophical frameworks, not from practitioners' lived experience. The corroboration outside the benefiting parties is divided: some scholars treat honji-suijaku as an intellectually genuine solution; others treat it as institutional rationalization for consolidating Buddhist authority. No scholarly source attests honji-suijaku is metaphysically necessary rather than institutionally chosen.
narrative_ontology:disappearance_verdict(simultaneous_veneration__ontological_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__ontological_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__ontological_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(simultaneous_veneration__ontological_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__ontological_fusion_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__ontological_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__ontological_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.58→0.72 over the interval) because the constraint's primary function shifts from genuine coordination (resolving theological incoherence in early Heian) toward pure extraction (defending Buddhist institutional monopoly over kami interpretation in late Edo/early Meiji). Theater ratio rises from 0.28 to 0.41, indicating increasing ratio of institutional gatekeeping to actual theological work — by the late period, the constraint's function is predominantly maintaining institutional hierarchy rather than resolving genuine metaphysical questions. Suppression rises from 0.52 to 0.68: as competing readings (especially domain_partition) gain scholarly credibility, Buddhist institutions intensify suppression of heterodox interpretations. The measurements are taken on a single shared time grid so every metric is authored at every examined point. The interval spans roughly 1000 years (Heian through Meiji), compressed to 0-100 for analytical tractability.
 *
 * PERSPECTIVAL GAP:
 *   The Buddhist institutional agenda-setter seat and the indigenous kami-tradition victim seat should compute vastly different classifications. From the Buddhist institutional position, honji-suijaku is genuine metaphysical theory coordinating two traditions under unified understanding — low extraction, valid coordination. From the kami-tradition position, the same arrangement is enforced subordination of autonomous spiritual entities to Buddhist institutional authority — high extraction, suppressed alternatives. The engine computes this divergence from the structural data: beneficiary with institutional power and arbitrage exit vs. victim with moderate power and identity-locked, constrained exit. The authored claim (tangled_rope) reflects this structural asymmetry; it is NOT reconciled to a predicted engine output.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutional hierarchy: d ≈ 0.1-0.2 (full beneficiary — claims interpretive monopoly, controls resource flows, defines what counts as legitimate kami theology). Indigenous kami traditions: d ≈ 0.8-0.9 (near-full target — bear subordination costs, lose interpretive autonomy, constrained exit by identity-fusion to shrine roles). Buddhist practitioners: d ≈ 0.4-0.5 (near-symmetric — benefit from coordination function and institutional structure, but partially absorbed into institutional authority hierarchy). Shrine keepers: d ≈ 0.6-0.7 (strong target pull from lost autonomy, partially offset by institutional legitimacy benefits and identity-lock preventing exit). The identity_locked exit status is structurally critical: shrine keepers' identity is constituted through kami-tradition continuity, making exit from this reading psychologically and socially unavailable even when the reading subordinates their authority. This drives d upward for this seat despite some benefit-accrual.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (theological incoherence of simultaneous practice) was LIVE and real in early Heian — kami worship and Buddhism were genuinely in tension. Honji-suijaku resolved this by reinterpreting kami as Buddha-manifestations. By late Edo / early Meiji (interval t=80-100), the founding problem is DEAD — the Meiji state's separation of Shinto and Buddhism demonstrates that practitioners are perfectly capable of maintaining separate traditions without the integrating theory. Yet the constraint persists: Buddhist institutions continue defending honji-suijaku in the face of state termination and scholarly challenge. This is mandatrophy: the constraint has outlived its functional purpose but persists through institutional inertia and authority defense. The theater_ratio's rise (0.28→0.41) and the escalating suppression_requirement (0.52→0.68) are symptomatic: the constraint's function shifts from solving a genuine problem (coordination) to defending an institutional position (extraction). This divergence between founding_problem_status (dead) and disappearance_verdict (world_rearranges) is the mandatrophy flag. The constraint is NOT a pure mountain (metaphysical truth is not natural law) and not pure snare (genuine coordination function existed). It is tangled_rope undergoing mandatrophic decay: the coordination function is atrophied, but extraction persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_truth_vs_institutional_claim,
    'Is honji-suijaku an accurate description of metaphysical reality (kami and buddhas genuinely are ontologically identical), or is it an institutional rationalization developed to justify Buddhist absorption of kami traditions?',
    'This is fundamentally un-empirical: metaphysical claims about the nature of spiritual entities are not testable by external observation. Proxies include: (1) examining the historical timing of honji-suijaku theory development (does it emerge as intellectual response to a genuine problem, or as institutional justification for power consolidation?); (2) examining the constraint''s vulnerability to external authority (Meiji state termination suggests power-dependence rather than truth-dependence); (3) examining whether competing readings (domain_partition, pragmatic_incoherence) can equally explain practitioners'' actual behavior.',
    'If honji-suijaku is metaphysical truth, the constraint is a mountain or rope — the coordination is real, extraction is legitimate cost. If it is institutional rationalization, the constraint is snare or tangled_rope — extraction dominates, coordination is cover story. This reading asserts the former; the omega flags the irreducibility of the question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metaphysical_truth_vs_institutional_claim, conceptual, 'Whether honji-suijaku describes metaphysical reality or serves institutional power.').

omega_variable(
    competing_reading_viability,
    'Could practitioners have maintained simultaneous kami and Buddhist practice equally coherently under domain_partition_reading (kami govern this-world prosperity, buddhas govern afterlife salvation) without honji-suijaku ontological fusion?',
    'Historical examination of pre-Heian practice (before honji-suijaku systematization) and post-Meiji practice (after state-mandated separation): if practitioners maintained robust simultaneous practice under either alternative framing, domain_partition was genuinely viable and honji-suijaku was not necessary coordination but rather institutional choice.',
    'If domain_partition was viable, honji-suijaku is revealed as one choice among alternatives, not a necessary truth — and the extraction becomes clearer (Buddhist institutions chose a framework that subordinates kami to Buddha, despite alternatives existing). If honji-suijaku was necessary for coherence, the coordination function is more real and extraction is legitimate cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_reading_viability, empirical, 'Whether alternative framings could achieve the same coordination function.').

omega_variable(
    identity_lock_mechanism,
    'For shrine keepers and kami-tradition practitioners, is the constraint on kami autonomy maintained by structural barriers (institutional control of resources, legal prohibitions on kami shrines) or by internalized identity-fusion (practitioners'' self-concept fused with the constraint, making exit psychologically unavailable)?',
    'Post-constraint exit trajectory: after Meiji separation, did shrine keepers and kami practitioners rapidly establish independent kami traditions (suggesting identity-lock was primary suppression mechanism and readily overcome), or did they struggle to regain autonomy (suggesting structural barriers persisted after state separation)?',
    'If identity-locked (internalized), the constraint''s suppression migrated with practitioners even after state termination — they carried the constraint''s limitations with them. If structural (external), termination of institutional enforcement should have rapidly restored autonomy. The actual historical record shows partial identity-lock: Shinto revival occurred but remained partially dependent on institutional frameworks. This suggests both mechanisms operate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether suppression of kami autonomy is structural (institutional) or internalized (identity-fused).').

omega_variable(
    sibling_reading_foreclosure,
    'Does honji-suijaku ontological fusion logically foreclose domain_partition_reading (kami and buddhas are genuinely distinct), or can both coexist as live positions?',
    'Logical examination: if one premise directly negates the other (e.g., ''kami are identical to buddhas'' directly contradicts ''kami are genuinely distinct''), then foreclosure holds. If both can be held in different frameworks or by different parties, coexistence is the relationship.',
    'Foreclosure would mean this reading''s adoption by an authority structure makes domain_partition impossible within that framework. Coexistence would mean both are live readings held by different parties. Historical evidence suggests coexistence: domain_partition readings persist in folk practice and scholarly circles even during high enforcement of honji-suijaku, and neither reading logically eliminates the other — they appeal to different epistemic authorities (Buddhist metaphysics vs. practical kami-domain specialization). This reading''s relations should show coexists_with for domain_partition, not forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether this reading''s logical structure precludes sibling readings or permits coexistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__ontological_fusion_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(simu_tr_t0, observed).
narrative_ontology:measurement(simu_tr_t20, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(simu_tr_t20, observed).
narrative_ontology:measurement(simu_tr_t40, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement_basis(simu_tr_t40, observed).
narrative_ontology:measurement(simu_tr_t60, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 60, 0.39).
narrative_ontology:measurement_basis(simu_tr_t60, observed).
narrative_ontology:measurement(simu_tr_t80, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 80, 0.41).
narrative_ontology:measurement_basis(simu_tr_t80, observed).
narrative_ontology:measurement(simu_tr_t100, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 100, 0.41).
narrative_ontology:measurement_basis(simu_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(simu_be_t0, observed).
narrative_ontology:measurement(simu_be_t20, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(simu_be_t20, observed).
narrative_ontology:measurement(simu_be_t40, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(simu_be_t40, observed).
narrative_ontology:measurement(simu_be_t60, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement_basis(simu_be_t60, observed).
narrative_ontology:measurement(simu_be_t80, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 80, 0.72).
narrative_ontology:measurement_basis(simu_be_t80, observed).
narrative_ontology:measurement(simu_be_t100, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 100, 0.72).
narrative_ontology:measurement_basis(simu_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(simu_su_t0, observed).
narrative_ontology:measurement(simu_su_t20, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(simu_su_t20, observed).
narrative_ontology:measurement(simu_su_t40, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(simu_su_t40, observed).
narrative_ontology:measurement(simu_su_t60, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 60, 0.66).
narrative_ontology:measurement_basis(simu_su_t60, observed).
narrative_ontology:measurement(simu_su_t80, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 80, 0.68).
narrative_ontology:measurement_basis(simu_su_t80, observed).
narrative_ontology:measurement(simu_su_t100, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 100, 0.68).
narrative_ontology:measurement_basis(simu_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__ontological_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__ontological_fusion_reading, 0.12).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration__domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration__pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% The simultaneous_veneration kernel decomposes into three readings, each instantiating a different constraint with different ε values and beneficiary/victim structures. This constraint (ontological_fusion_reading) claims kami and buddhas are ontologically identical per honji-suijaku theory; ε is high because ontological unity is enforced and kami autonomy is subordinated. The domain_partition_reading treats them as genuinely distinct (lower ε, specialization is natural). The pragmatic_incoherence_reading denies coherence was ever achieved (high ε, different victim structure — institutional suppression of contradiction rather than of autonomy). All three are linked as network neighbors; pairwise comparison reveals which reading best explains the historical record.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(simultaneous_veneration__ontological_fusion_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
