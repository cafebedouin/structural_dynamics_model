% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__shafii_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jurisprudential_method_kernel__shafii_reading
 *   human_readable: Al-Shafi'i's Four-Tier Jurisprudential Hierarchy: Hadith Authentication as Arbiter
 *   domain: institutional/jurisprudential
 *
 * SUMMARY:
 *   Al-Shafi'i's four-tier jurisprudential hierarchy (Qur'an → Hadith → Ijma
 *   → Qiyas) was presented as a methodological standardization that resolved
 *   inconsistencies across earlier Islamic legal schools. This constraint
 *   story models ONE reading of the contested jurisprudential kernel: the
 *   Shafi'i reading asserts that systematic hierarchy, authenticated hadith
 *   transmission, and methodological rigor are intrinsic to legitimate
 *   law-derivation. Other readings (Hanafi, Maliki, Hanbali) contest this
 *   hierarchy's universality, privileging alternative sources or reasoning
 *   methods. This story traces al-Shafi'i's reading alone—its structural
 *   benefits for hadith scholars, its costs for custom-based and
 *   analogically-driven jurisprudence, and the enforcement mechanisms
 *   required to establish it as canonical.
 *
 * KEY AGENTS:
 *   - Hadith scholars and transmitters: institutional beneficiary; control the authentication gate that every Shafi'i conclusion must pass through; gain prestige and gatekeeping authority
 *   - Customary practice traditions: powerful payer; lose institutional legitimacy as 'amal ahl al-Madina is subordinated to authenticated hadith
 *   - Analogical reasoners (qiyas practitioners): moderate payer with identity-lock; their reasoning authority is demoted from co-equal to subsidiary (fourth tier)
 *   - Earlier jurisprudential schools (proto-Hanafi, proto-Maliki, proto-Hanbali): organized payer; bear retrospective delegitimization as their inconsistencies are highlighted
 *   - Shafi'i school adherents: identity-locked beneficiary; inherit institutional prestige as the 'rationalized' school
 *   - Competing frameworks (Hanafi, Maliki, Hanbali readings): excluded; structurally barred from equal canonical standing as long as Shafi'i's hierarchy dominates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, 0.62).
domain_priors:suppression_score(jurisprudential_method_kernel__shafii_reading, 0.71).
domain_priors:theater_ratio(jurisprudential_method_kernel__shafii_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__shafii_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__shafii_reading, "Al-Shafi'i's Four-Tier Jurisprudential Hierarchy: Hadith Authentication as Arbiter").
narrative_ontology:topic_domain(jurisprudential_method_kernel__shafii_reading, "institutional/jurisprudential").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__shafii_reading, '580d0a7b-3b1f-4cd7-9e5f-7b05b49aa859').
narrative_ontology:cs_kernel_codification('580d0a7b-3b1f-4cd7-9e5f-7b05b49aa859', fixed_text).
narrative_ontology:cs_authority_grounding('580d0a7b-3b1f-4cd7-9e5f-7b05b49aa859', extraction).
narrative_ontology:cs_interpretation_layer_present('580d0a7b-3b1f-4cd7-9e5f-7b05b49aa859').
narrative_ontology:cs_reading_relation('580d0a7b-3b1f-4cd7-9e5f-7b05b49aa859', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('580d0a7b-3b1f-4cd7-9e5f-7b05b49aa859', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('580d0a7b-3b1f-4cd7-9e5f-7b05b49aa859', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('580d0a7b-3b1f-4cd7-9e5f-7b05b49aa859', foundational, hadith_transmission_as_epistemological_gold_standard).
narrative_ontology:cs_axiom_status(hadith_transmission_as_epistemological_gold_standard, holdable).
narrative_ontology:cs_axiom_grounding('580d0a7b-3b1f-4cd7-9e5f-7b05b49aa859', hadith_transmission_as_epistemological_gold_standard, empirically_contingent).
narrative_ontology:cs_axiom('580d0a7b-3b1f-4cd7-9e5f-7b05b49aa859', foundational, hierarchical_source_ordering_resolves_inconsistency).
narrative_ontology:cs_axiom_status(hierarchical_source_ordering_resolves_inconsistency, holdable).
narrative_ontology:cs_axiom_grounding('580d0a7b-3b1f-4cd7-9e5f-7b05b49aa859', hierarchical_source_ordering_resolves_inconsistency, instrumental).
narrative_ontology:cs_reference_frame('580d0a7b-3b1f-4cd7-9e5f-7b05b49aa859', hierarchical_textual_standardization).
narrative_ontology:cs_drift_state('580d0a7b-3b1f-4cd7-9e5f-7b05b49aa859', contemporary_institutional_entrenchment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('580d0a7b-3b1f-4cd7-9e5f-7b05b49aa859', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, hadith_scholars_and_transmitters).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, customary_practice_traditions).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, analogical_extension_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, shafii_school_adherents).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, earlier_schools_of_law).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__shafii_reading, methodological_standardization_resolves_inconsistency).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__shafii_reading, textual_hierarchy_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the authority to authenticate hadith chains (isnads) and assess reliability (sahih, hasan, da'if, fabricated). Under al-Shafi'i's framework, no juristic conclusion is valid without grounding in authenticated hadith. This elevates them from specialists to gatekeepers of the entire legal system. They collect institutional prestige, teaching positions, and the power to validate or reject jurisprudential conclusions. Their technical expertise in transmission chains becomes the central tool for resolving legal disputes.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, hadith_scholars_and_transmitters, beneficiary,
    institutional, civilizational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__shafii_reading, hadith_scholars_and_transmitters, agenda_setter).

% Communities and regional tradition-keepers grounded law in established customs and practices ('amal ahl al-Madina for Medina; local 'urf for regional centers). Under al-Shafi'i, custom is valid only if authenticated through hadith. Established practices must be retroactively justified through hadith chains; if no hadith supports them, they are demoted as bid'ah or custom without textual grounding. The cost is institutional delegitimization; communities lose the ability to ground law in living tradition alone.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, customary_practice_traditions, payer,
    powerful, generational, constrained, regional).

% Juristic reasoners who mastered qiyas (analogical extension) and istihsan (juristic preference/discretion) as primary tools for deriving law in new cases. Under al-Shafi'i's hierarchy, these tools are permitted only when Qur'an, Hadith, and Ijma provide no guidance—they are subordinate, residual tools. Their professional identity and career authority rest on reasoning mastery; the demotion of reasoning to fourth tier costs them prestige and authority. Exit is identity-locked because their entire juristic self-conception is built around analogical reasoning.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, analogical_extension_practitioners, payer,
    moderate, biographical, identity_locked, continental).

% Proto-Hanafi, proto-Maliki, proto-Hanbali schools operated with different methodologies before al-Shafi'i's standardization. They are retrospectively reframed as inconsistent, unsystematic, or erroneous. Earlier juristic conclusions are subject to re-evaluation against the Shafi'i hierarchy; many are invalidated or relegated to school-specific exceptions. These schools bear the cost of institutional delegitimization and loss of canonical standing.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, earlier_schools_of_law, payer,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__shafii_reading, earlier_schools_of_law, excluded).

% Inherit a methodologically coherent, systematized legal school that claims to resolve pre-Shafi'i inconsistencies. They gain institutional prestige as adherents of the 'rationalized' school. The clear decision procedure (follow the hierarchy) provides epistemic certainty and reduces the need to negotiate between competing reasoning methods. Their professional identity becomes fused with the Shafi'i framework; they resist competing methodologies as unsystematic.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, shafii_school_adherents, beneficiary,
    organized, civilizational, identity_locked, continental).

% Judges, muftis, and legal interpreters across regions navigate al-Shafi'i's framework as it competes with other schools. They observe whether the hierarchy simplifies their decisions (coherent procedure) or constrains their responsiveness to local conditions (subordinates custom and local practice). Their decisions create enforcement pressure on the hierarchy; widespread divergence signals that the framework is not universally accepted.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, community_religious_authorities, observer,
    institutional, civilizational, analytical, continental).

% Hanafi (qiyas-privileging), Maliki ('amal-privileging), and Hanbali (text-and-consensus-only) frameworks remain as live jurisprudential positions but are structurally barred from canonical standing as long as al-Shafi'i's hierarchy dominates. Their reasoning methods are delegitimized as unsystematic, particularistic, or innovations. They persist as schools within the broader Islamic tradition but cannot claim equal authority with the canonical Shafi'i framework.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, competing_jurisprudential_frameworks, excluded,
    powerful, civilizational, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__shafii_reading, hadith_scholars_and_transmitters).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__shafii_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, systematic procedure for deriving Islamic law that resolves apparent inconsistencies across pre-Shafi'i schools. By ordering sources hierarchically (Qur'an > Hadith > Ijma > Qiyas) and making hadith authentication the arbiter between competing interpretations, the framework provides a clear decision procedure: when in doubt, consult authenticated hadith; if no hadith exists, seek ijma; only if both are absent may one resort to qiyas. This reduces ad-hoc reasoning and produces consistent conclusions across the scholarly community.
% TRANSFER_FUNCTION: Transfers epistemic and institutional authority from regional custom-keepers, analogical reasoners, and decentralized juristic schools to hadith scholars who control authentication. Customary law is subordinated; qiyas is demoted from co-equal to subsidiary; competing schools are delegitimized. Scholars whose authority rested on mastery of local tradition or sophisticated analogical reasoning lose standing relative to those who control hadith transmission chains.
% ABSENT_VOICES: Customary practice traditions outside the Medina-centric narrative; analogical reasoners who saw qiyas as a co-equal interpretive tool; adherents of the Hanafi, Maliki, and Hanbali schools who reject the hierarchy as artificial standardization. These parties would argue that al-Shafi'i's framework privileges textual sources over lived practice, subordinates reason to transmission, ignores the legitimate diversity of jurisprudential method, and sacrifices flexibility for the appearance of consistency. They are not present in the institutional construction of the canonical hierarchy.
% DISAPPEARANCE_RATIONALE: If al-Shafi'i's hierarchy disappeared, Islamic jurisprudence would reorganize around competing methodologies. Hanafi qiyas-centered reasoning, Maliki custom-and-practice reasoning, Hanbali literal-text reasoning, and Hanbali consensus-only reasoning would remain as live alternatives without the delegitimization the Shafi'i framework imposes. Regional legal practice would revert to school-specific heterogeneity. The institutional prestige of hadith scholars would decline unless another framework elevated them. Customary practices would regain legitimacy as independent sources.
% FOUNDING_PROBLEM: Before al-Shafi'i's early-9th-century codification, Islamic jurisprudential schools operated with inconsistent hierarchies of sources. Pre-Shafi'i jurists drew on Qur'an, Hadith, Ijma, and Qiyas in different orders depending on school and context. Some schools prioritized customary practice ('amal), others qiyas (analogical reasoning), others literal text. This produced divergent legal conclusions on the same matters, creating apparent contradictions and making law seem ad-hoc and inconsistent rather than systematic.
% FOUNDING_PROBLEM_CORROBORATION: Al-Shafi'i and institutional Shafi'i followers attest the founding problem is live—without rigorous methodology, jurisprudence becomes fragmented and inconsistent. Contemporary Hanafi, Maliki, and Hanbali scholars attest the problem has been overstated or mischaracterized. They argue that pre-Shafi'i schools achieved internal consistency through their own methodologies, and that al-Shafi'i's hierarchy is presented as necessary but is actually one choice among legitimate alternatives. Modern historians of Islamic law document genuine heterogeneity pre-Shafi'i and debate whether this heterogeneity was problematic or justified. No voice outside the Shafi'i institutional orbit corroborates al-Shafi'i's framing of pre-Shafi'i jurisprudence as systemically inconsistent; competing schools defend their own methodologies as systematic.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__shafii_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__shafii_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint exhibits medium-high extractiveness (0.62 at interval end) because al-Shafi'i's hierarchy transfers authoritative weight from decentralized custom and flexible reasoning to a centralized authentication gate controlled by hadith scholars. The beneficiary (hadith scholars) gains institutional gatekeeping power; the victims (custom traditions, analogical reasoners) lose standing. Suppression is high (0.71) because the framework's dominance requires active enforcement: competing schools must be delegitimized as methodologically unsystematic, customary practice must be subordinated, and qiyas must be confined to a residual category. Theater is moderate-low (0.28) because the framework's coherence is substantively real—it does provide a systematic procedure—but growing institutional rigidity around hadith authentication suggests performative defense of the hierarchy itself. The measurement series shows rising extractiveness and suppression over the interval, reflecting the framework's deepening institutional entrenchment and the increasing cost of maintaining competing methodologies as subordinate. All metrics are authored at every shared time point.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (hadith scholars) and the payer seats (customary practice, analogical reasoners) should compute to different types. From the hadith scholar's position, al-Shafi'i's framework is genuine coordination—it resolves inconsistency and provides a shared decision procedure. From the payer seats, the same structure operates as enforced hierarchy that subordinates their reasoning authority. The engine computes this divergence from the structural data: beneficiary directionality feeds lower effective extraction; payer directionality feeds higher. The authored claim (tangled_rope) asserts both genuine coordination (resolving inconsistency) AND asymmetric extraction (shifting authority to hadith scholars).
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith scholars sit at near-full beneficiary directionality (d ≈ 0.1–0.2): they control the authentication gate and collect institutional prestige without bearing the costs of legal uncertainty. Customary practice traditions sit at near-full target directionality (d ≈ 0.8–0.9): they bear the cost of delegitimization and subordination; their exit options are constrained by institutional pressure to conform to the Shafi'i hierarchy. Analogical reasoners sit intermediate (d ≈ 0.6–0.7): they are allowed to reason but only within a defined subordinate tier; their identity-lock (professional juristic identity built around qiyas mastery) makes exit costly. The framework itself is institutional (powerful, continental scope), which amplifies effective extraction through its scope: the beneficiary's control spreads across the entire Islamic scholarly world, not just a locality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pre-Shafi'i jurisprudential inconsistency) is CONTESTED rather than resolved. The competing schools (Hanafi, Maliki, Hanbali) each claim their own methodologies achieve consistency; they dispute al-Shafi'i's framing of pre-Shafi'i jurisprudence as chaotic. This contestation prevents the constraint from resting purely on demonstrated necessity. The hierarchy persists not because its founding problem is universally acknowledged as solved, but because institutional investment in the Shafi'i framework has made it the canonical standard. The tangled_rope classification captures this: genuine coordination function (systematic procedure) rides on active enforcement (suppression of competing methodologies and delegitimization of their reasoning moves). Mandatrophy is NOT resolved—the founding problem remains contested, suggesting the constraint may persist partly through institutional inertia rather than universal agreement on necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hadith_authentication_as_extraction_mechanism,
    'Is al-Shafi''i''s elevation of hadith authentication a genuinely necessary coordination mechanism, or does it primarily serve to concentrate gatekeeping authority among hadith scholars?',
    'Comparative analysis of jurisprudential consistency: do Shafi''i conclusions actually show higher consistency than conclusions derived via Hanafi qiyas or Maliki ''amal? Does the hierarchy reduce genuine disputes or simply relocate them to the hadith authentication level?',
    'If hadith authentication solves a real coordination problem, the framework is primarily tangled_rope (coordination + extraction). If consistency is comparable across schools and the hierarchy primarily concentrates authority, the constraint approaches snare (extraction with coordination cover). The measurement series shows rising theater_ratio, suggesting increasing performative maintenance of the hierarchy itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hadith_authentication_as_extraction_mechanism, empirical, 'Whether hadith authentication is a necessary coordination mechanism or a gatekeeping extraction device.').

omega_variable(
    identity_lock_on_analogical_reasoners,
    'Do analogical reasoners remain constrained by the framework, or do competing schools (Hanafi, Maliki) maintain qiyas/istihsan as coordinate sources despite Shafi''i''s subordination?',
    'Institutional history: do Hanafi and Maliki scholars continue to justify qiyas as co-equal to hadith, or do they eventually internalize the Shafi''i hierarchy and treat qiyas as auxiliary? If they internalize, suppression operates through identity-fusion; if they resist, suppression requires active institutional enforcement.',
    'If identity-locked (analogical reasoners internalize the Shafi''i framework), effective suppression rises and the constraint approaches piton (maintained by internalized belief rather than external force). If they resist, suppression requires continuous enforcement and the constraint remains tangled_rope with high enforcement cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_on_analogical_reasoners, empirical, 'Whether competing schools internalize the Shafi''i hierarchy or resist it as an external constraint.').

omega_variable(
    kernel_reading_contestation_ongoing,
    'Does the contested status of the founding problem (''is pre-Shafi''i jurisprudence actually inconsistent?'') persist as active dispute, or has institutional dominance of the Shafi''i school settled the question?',
    'Contemporary jurisprudential scholarship: do modern Hanafi, Maliki, and Hanbali scholars argue for the legitimacy of their own methodologies as equal to Shafi''i, or do they accept the Shafi''i frame as canonical and argue only for exceptions/permutations within it?',
    'If the contest is still live (competing readings actively advocated), the constraint persists through institutional power rather than consensus. If the contest is settled, the constraint may stabilize as canonical rope (universally adopted coordination procedure). Rising theater_ratio suggests the former—increasing performative defense of the hierarchy rather than its disappearance into background consensus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation_ongoing, conceptual, 'Whether the Shafi''i reading remains contested or has achieved de facto canonical status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__shafii_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__shafii_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(juri_tr_t0, observed).
narrative_ontology:measurement(juri_tr_t5, jurisprudential_method_kernel__shafii_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement_basis(juri_tr_t5, observed).
narrative_ontology:measurement(juri_tr_t10, jurisprudential_method_kernel__shafii_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(juri_tr_t10, observed).
narrative_ontology:measurement(juri_tr_t15, jurisprudential_method_kernel__shafii_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement_basis(juri_tr_t15, observed).
narrative_ontology:measurement(juri_tr_t25, jurisprudential_method_kernel__shafii_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement_basis(juri_tr_t25, observed).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__shafii_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(juri_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(juri_be_t0, observed).
narrative_ontology:measurement(juri_be_t5, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement_basis(juri_be_t5, observed).
narrative_ontology:measurement(juri_be_t10, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(juri_be_t10, observed).
narrative_ontology:measurement(juri_be_t15, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(juri_be_t15, observed).
narrative_ontology:measurement(juri_be_t25, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement_basis(juri_be_t25, observed).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(juri_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(juri_su_t0, observed).
narrative_ontology:measurement(juri_su_t5, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(juri_su_t5, observed).
narrative_ontology:measurement(juri_su_t10, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(juri_su_t10, observed).
narrative_ontology:measurement(juri_su_t15, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(juri_su_t15, observed).
narrative_ontology:measurement(juri_su_t25, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(juri_su_t25, observed).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(juri_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__shafii_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__shafii_reading, 0.12).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jurisprudential_method_kernel. The kernel is contested: Islamic legal sources are ordered hierarchically, but which hierarchy and which reasoning methods are valid. The Shafi'i reading asserts Qur'an → Hadith → Ijma → Qiyas with hadith authentication as arbiter. The Hanafi reading privileges qiyas and istihsan as co-equal reasoning tools. The Maliki reading grounds law in Median custom and practice ('amal ahl al-Madina). The Hanbali reading rejects qiyas and istihsan as bid'ah and privileges literal text and companion opinions. Each reading instantiates a different constraint with different beneficiaries, victims, and ε values. They are linked through network.affects_constraints because adoption of one reading constrains the institutional legitimacy of others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__shafii_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
