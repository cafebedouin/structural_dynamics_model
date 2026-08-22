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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: jurisprudential_method_kernel__shafii_reading
 *   human_readable: al-Shafi'i's Methodological Hierarchy: Qur'an-Hadith-Ijma-Qiyas
 *   domain: legal/religious/institutional
 *
 * SUMMARY:
 *   al-Shafi'i's methodological hierarchy (Qur'an → Hadith → Ijma → Qiyas)
 *   was codified in the early 9th century as a systematic framework for
 *   Islamic jurisprudence. The framework claims to resolve inconsistencies
 *   among earlier schools by imposing a uniform source-ranking that all
 *   jurists can apply. Hadith scholars become the arbiters of law through
 *   their authentication and transmission expertise; customary practices and
 *   analogical reasoning lose independent standing. The Shafi'i school and
 *   its institutional successors enforce the hierarchy through teaching,
 *   jurisprudential authority, and influence over legal institutions. This is
 *   ONE READING of the contested jurisprudential kernel: how divine law
 *   should be systematically derived. The sibling readings—Hanafi (reason +
 *   analogy; istihsan legitimacy), Maliki (Medinan practice as source),
 *   Hanbali (literal text; no innovation)—all propose different source
 *   hierarchies for the same kernel. This story instantiates the Shafi'i
 *   reading alone, not the contest itself.
 *
 * KEY AGENTS:
 *   - hadith_scholars_transmitters: gain institutional authority over jurisprudential gatekeeping via hadith authentication
 *   - shafii_legal_school: agenda-setter, administers the hierarchy, provides consistent methodology
 *   - customary_practice_advocates: lose independent evidentiary standing; custom now requires hadith authentication
 *   - analogical_extension_proponents: constrained to fourth-tier subordinate role; creative reasoning loses scope
 *   - madina_school_survivors: Maliki-school beneficiaries of local Medinan practice lose that independent source
 *   - early_juristic_schools: excluded from methodological authority; reframed as less systematic
 *   - observer_comparative_jurisprudence: analytical seat examining how the hierarchy's clarity generated institutional power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, 0.68).
domain_priors:suppression_score(jurisprudential_method_kernel__shafii_reading, 0.52).
domain_priors:theater_ratio(jurisprudential_method_kernel__shafii_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__shafii_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__shafii_reading, "al-Shafi'i's Methodological Hierarchy: Qur'an-Hadith-Ijma-Qiyas").
narrative_ontology:topic_domain(jurisprudential_method_kernel__shafii_reading, "legal/religious/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__shafii_reading, '4c8f9914-8208-4328-803a-aa9cf03c2e4f').
narrative_ontology:cs_kernel_codification('4c8f9914-8208-4328-803a-aa9cf03c2e4f', fixed_text).
narrative_ontology:cs_authority_grounding('4c8f9914-8208-4328-803a-aa9cf03c2e4f', lineage).
narrative_ontology:cs_interpretation_layer_present('4c8f9914-8208-4328-803a-aa9cf03c2e4f').
narrative_ontology:cs_reading_relation('4c8f9914-8208-4328-803a-aa9cf03c2e4f', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c8f9914-8208-4328-803a-aa9cf03c2e4f', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c8f9914-8208-4328-803a-aa9cf03c2e4f', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('4c8f9914-8208-4328-803a-aa9cf03c2e4f', foundational, hadith_transmission_arbiter).
narrative_ontology:cs_axiom_status(hadith_transmission_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('4c8f9914-8208-4328-803a-aa9cf03c2e4f', hadith_transmission_arbiter, empirically_contingent).
narrative_ontology:cs_axiom('4c8f9914-8208-4328-803a-aa9cf03c2e4f', foundational, source_hierarchy_primacy).
narrative_ontology:cs_axiom_status(source_hierarchy_primacy, holdable).
narrative_ontology:cs_axiom_grounding('4c8f9914-8208-4328-803a-aa9cf03c2e4f', source_hierarchy_primacy, deontological).
narrative_ontology:cs_reference_frame('4c8f9914-8208-4328-803a-aa9cf03c2e4f', quranic_and_hadith_primacy_with_systematic_methodology).
narrative_ontology:cs_drift_state('4c8f9914-8208-4328-803a-aa9cf03c2e4f', contemporary_islamic_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4c8f9914-8208-4328-803a-aa9cf03c2e4f', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, hadith_scholars_transmitters).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, shafii_legal_school).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, customary_practice_advocates).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, analogical_extension_proponents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, madina_school_survivors).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, ijma_consensus_holders).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, quranic_textualists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, madina_school_survivors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hadith authentication and transmission expertise becomes the critical gatekeeping function in jurisprudence. Under the Shafi'i hierarchy, all second-order sources (Hadith) must pass through the authentication expertise of hadith scholars: isnad (chain) verification, transmitter reliability assessment, and detection of fabrication. This institutional role gives hadith scholars leverage over jurisprudential conclusions—a ruling can be blocked if its underlying hadith is deemed weak or fabricated. Earlier methodologies distributed evaluative authority more widely (custom practitioners, judges, analogical reasoners); the Shafi'i hierarchy concentrates it in hadith expertise.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, hadith_scholars_transmitters, beneficiary,
    organized, generational, constrained, global).

% Al-Shafi'i codifies and institutionalizes the four-tier hierarchy through his foundational work al-Risala and through the Shafi'i school's subsequent jurisprudential scholarship and teaching authority. The school becomes the custodian of the standard methodology, training generations of jurists in the hierarchy, exemplifying its application through jurisprudential opinions, and maintaining its authority through institutional continuity. The school does not wield coercive power but institutional authority—the authority to set the agenda for how jurisprudence is taught, practiced, and evaluated. The hierarchy's clarity and reproducibility make it teachable, which amplifies the school's institutional reach.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, shafii_legal_school, agenda_setter,
    institutional, civilizational, mobile, global).

% Local customs and established practices ('amal, 'urf) lose independent evidentiary status. Under the Shafi'i hierarchy, customary practice is admissible only if authenticable through Qur'an, hadith, ijma, or qiyas. A practice that evolved organically in a community but cannot be traced to these sources is no longer recognized as valid law. Communities that had legal authority through their established customs (e.g., Medinan practice under the Maliki reading) face their practices subordinated to the requirement of hadith authentication. Oral traditions and locally-rooted legal knowledge lose their independent standing.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, customary_practice_advocates, payer,
    moderate, biographical, constrained, local).

% Analogical reasoning (qiyas) is relegated to the fourth and final tier, applied only after exhausting Qur'an, hadith, and ijma. Earlier methodologies (particularly Hanafi) allowed qiyas and juristic preference (istihsan) more independent scope for extending law to novel cases through creative reasoning about the ratio legis (underlying legal reason). The Shafi'i hierarchy restricts this: analogy must be grounded in explicit textual sources, the underlying reason must be evident in the texts, and extension is subordinate to the other sources. Jurists who built reputations on inventive analogical interpretation lose methodological scope.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, analogical_extension_proponents, payer,
    moderate, biographical, constrained, regional).

% The Maliki school's foundation—that Medinan practice ('amal ahl al-Madina) is an independent source because Medina preserved the Prophet's practice most faithfully—is superseded by the requirement that all practice must be authenticated through hadith. Living continuity loses its independent legitimacy. Communities that relied on unbroken local practice as evidence are constrained. However, if their practices can be authenticated through hadith, they gain textual legitimacy they lacked under the Maliki reading. The constraint is a payer role because it suppresses the Maliki methodology, but a secondary beneficiary role because some practices may gain firmer grounding through hadith authentication.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, madina_school_survivors, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__shafii_reading, madina_school_survivors, beneficiary).

% Pre-standardized schools (proto-Hanafi, Syrian juristic traditions, Iraqi jurisprudence) are marginalized by the clarity and institutional authority of the Shafi'i method. Their jurisprudential output is not invalidated, but the framework for evaluating new cases and resolving disputes is now set by the Shafi'i hierarchy. Schools operating on different source-ranking principles (elevating istihsan, custom, or literal interpretation) find their methodologies reframed as either deficient (lacking systematicity) or deviant. They are excluded from the primary conversation about how law should be derived, though they can continue practicing within their traditions in regions where the Shafi'i hierarchy is not enforced.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, early_juristic_schools, excluded,
    organized, generational, constrained, regional).

% Ijma (consensus of qualified scholars) is elevated to the third position, formalized as an explicit, recognized source with clear rank. This dignifies and structures the consensus-building process. Scholars whose jurisprudential conclusions enjoy broad agreement among competent jurists gain the institutional backing of the formal source hierarchy. Consensus becomes definitive, overriding individual interpretation. The Shafi'i method provides a clear gate for when consensus is achieved and how it operates. Benefit accrues to consensus-builders and to the authority of collective jurisprudential opinion.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, ijma_consensus_holders, beneficiary,
    organized, generational, mobile, global).

% The Qur'an remains at the apex of the hierarchy, with clear preference for literal and manifest readings (dhahir) over analogical extension. Textualists who favor deriving law directly from Qur'anic verses and avoiding reinterpretation gain institutional authority from the framework. The hierarchy forbids subordinating clear Qur'anic rulings to analogical reasoning or custom. The Hanbali and Shafi'i methods align on this textualist priority, validating approaches that resist creative reinterpretation.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, quranic_textualists, beneficiary,
    organized, generational, mobile, global).

% Observes the entire methodological structure and institutional dynamics: how the hierarchy's clarity and reproducibility generated institutional authority, how the fixing of source-order changed what counts as valid evidence and who holds the authority to make that determination, how the constraint's persistence is tied to the Shafi'i school's institutional continuity, and how sibling readings (Hanafi, Maliki, Hanbali) maintained parallel authority in regions where the Shafi'i hierarchy was not the dominant institutional framework.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, observer_comparative_jurisprudence, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__shafii_reading, shafii_legal_school).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__shafii_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves inconsistencies among earlier jurisprudential schools by establishing a clear, teachable, reproducible methodology: a uniform rank-ordering of sources (Qur'an → Hadith → Ijma → Qiyas) that allows different jurists to reach consistent conclusions when applying the same sources in the same hierarchical order. Enables systematic legal reasoning across the Islamic world without requiring centralized authority by ensuring all jurists apply the same source-weighing logic.
% TRANSFER_FUNCTION: Transfers interpretive authority from customary practices and analogical reasoning (which were more democratized, locally rooted, and flexible under earlier methodologies) to hadith scholars and the institutional authority of the Shafi'i school. Moves the power to authenticate law from locally-rooted practice to textually-grounded, transmitted hadith. Also transfers authority over methodological disputes from multiple competing schools to the Shafi'i hierarchy's rank-ordering.
% ABSENT_VOICES: Hanafi, Maliki, and Hanbali schools are not entirely absent but are structurally marginalized by the Shafi'i framework. Communities whose authority rested on local custom ('amal) and jurists whose reputations derived from creative analogical reasoning (istihsan) are absent from the institutional seat of lawmaking under this hierarchy. They would argue that their methodologies were equally systematic and that the Shafi'i hierarchy privileges textual authentication over living practice and reason—a false standardization, not a genuine coordination.
% DISAPPEARANCE_RATIONALE: If the Shafi'i methodology vanished, jurisprudential authority would fragment again along Hanafi, Maliki, and Hanbali lines. Communities would revert to local customary practice as independent sources. Analogical reasoning would expand beyond the fourth-tier subordinate role. Consistency across regions and schools would decay. The Islamic legal world reorganized around this hierarchy—institutions trained students in it, courts applied it, scholars built reputations through mastering and exemplifying it. Its disappearance would leave a vacuum filled by competing methodologies, decentralizing law and distributing authority back to customary practitioners and analogical reasoners.
% FOUNDING_PROBLEM: Earlier schools of jurisprudence (proto-Hanafi, Maliki, Hanbali, Syrian jurisprudence) applied inconsistent methodologies for weighing sources: some elevated custom higher than others, some permitted analogical reasoning more flexibly, some disagreed on the weight of ijma. This produced conflicting rulings on the same issues from different schools, making Islamic law appear unstable and arbitrary. Al-Shafi'i's hierarchy was designed to provide a unified, systematic method that all schools could apply, resolving inconsistencies through methodological clarity.
% FOUNDING_PROBLEM_CORROBORATION: Al-Shafi'i and the Shafi'i school attest the founding problem as live—asserting that earlier inconsistencies required methodological standardization for legal stability. Historians and comparative jurisprudence scholars outside the benefiting schools confirm that earlier schools applied different source hierarchies and produced different rulings on comparable issues. However, Hanafi, Maliki, and Hanbali scholars attest that their methodologies were equally systematic within their own frameworks and that the founding problem was exaggerated to justify a power grab for the Shafi'i method. Anthropological evidence from regions that maintained non-Shafi'i methodologies shows that local legal reasoning remained consistent and functional despite methodological diversity—the founding problem may have been false or overstated.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__shafii_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__shafii_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__shafii_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises over the interval (0.42 → 0.68) as the Shafi'i hierarchy becomes institutionalized: initial adoption is primarily coordinating (high clarity, genuine resolution of earlier inconsistencies), but as the method becomes standard teaching and institutional practice, the extraction component grows. Theater (0.28 at interval end) reflects that the hierarchy's legitimacy as a coordination mechanism is performatively maintained—didactic texts and jurisprudential scholarship continually exemplify the method's consistency, reinforcing its authority. But the underlying extraction (suppression of custom and analogical creativity) requires ongoing institutional enforcement: courts must reject custom-based arguments, hadith experts must maintain gatekeeping authority, schools must teach the hierarchy as the only valid method. Suppression (0.52 at interval end) is moderate-high: the constraint is not enforced through coercive power (Islamic law has no centralized enforcement apparatus) but through institutional authority (schools, courts, scholarly consensus) and internalized belief (communities come to accept that custom lacks validity without hadith authentication). Accessibility collapse (0.71) is high: once the hierarchy is adopted, alternatives largely close off—a jurist who wants to apply law must use the Shafi'i method or explicitly defend a deviation; the intellectual terrain shifts so that custom-based reasoning appears methodologically deficient rather than merely different.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter's seat (Shafi'i school), the constraint is coordination with authority vesting in systematic method. From the target seats (custom advocates, analogical reasoners), it is enforced extraction. From the beneficiary seats (hadith scholars), it is legitimate authority concentration. The engine's per-seat computation should show these divergences: the same structural inputs (beneficiary declarations, exit constraints, power atoms) produce different type classifications for different seats—Rope-like from the Shafi'i school, Tangled Rope or Snare from the constrained seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith scholars benefit from the hierarchy (expert authority); customary practice advocates lose independent standing (constrained exit, suppressed methodology); analogical reasoners lose methodological scope (subordinated to textual sources). The Shafi'i school sets and maintains the hierarchy, collecting institutional authority and influence. Beneficiary seats derive low d-values; target seats derive high d-values. Directionality overrides are not needed—the structural derivation (beneficiary + powerful + arbitrage → low d; victim + moderate + constrained → high d) captures the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (inconsistent methodologies producing divergent rulings) is contested as live or dead. The Shafi'i school and early adopters attest it as live—the hierarchy is essential to maintain consistency. Other schools and communities attest it as either dead (the earlier schools were sufficiently consistent) or false (the hierarchy creates false uniformity by suppressing valid methodological alternatives). The constraint's persistence depends on institutional teaching and court adoption. If the founding problem is truly dead (methodological consistency achieved without the Shafi'i hierarchy in some regions), the constraint's persistence is sustained by inertia and institutional power rather than functional necessity. The measurement series shows extraction rising over time (0.42 → 0.68) while theater stays moderate (0.28), suggesting the constraint is shifting from coordination-heavy toward extraction-heavy as it becomes established: initial adoption solves the consistency problem, but later adoption by institutional authority is sustained more by institutional power than by continuous methodological necessity. This is not mandatrophy (functional death + inertial persistence) yet, but a drift toward it—early theater-low periods show the hierarchy genuinely coordinating; later theater-low periods show the hierarchy performing its role without evidence that it is still solving the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    methodological_superiority_vs_institutional_power,
    'Did the Shafi''i hierarchy resolve earlier schools'' inconsistencies because it was intellectually superior, or because it was backed by institutional authority and didactic systematicity that outcompeted other methodologies?',
    'Historical analysis of jurisprudential reasoning quality in each school before and after the Shafi''i codification; examination of whether Hanafi, Maliki, and Hanbali scholars found the Shafi''i hierarchy *logically* superior to their own methods, or whether they adopted it for institutional/political reasons.',
    'If superior by reasoning, the constraint is genuine coordination (solving a real methodological problem). If adopted for institutional reasons, the constraint is extractive: the hierarchy is a cover story for concentrating authority in the Shafi''i school and hadith scholars.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodological_superiority_vs_institutional_power, conceptual, 'Whether the constraint''s explanatory power derives from logical coherence or institutional dominance.').

omega_variable(
    textual_authentication_vs_living_practice,
    'Is the requirement that all law be authenticable through hadith transmission a feature necessary for legal stability, or does it artificially suppress locally-evolved legal practices that function effectively without explicit textual grounding?',
    'Comparative study of legal outcomes in Shafi''i regions (where custom requires hadith authentication) versus Maliki regions (where Medinan practice is independent source) on identical legal questions. Assessment of legal coherence and community acceptance in each framework.',
    'If local practice performs equally well without hadith authentication, suppression is high and the constraint is extractive (privileges textual scholars over communities). If hadith authentication produces measurably more stable or consistent law, suppression is lower and the constraint is partly coordinating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_authentication_vs_living_practice, empirical, 'Whether hadith authentication requirement improves legal outcomes or merely centralizes authority.').

omega_variable(
    reading_committer_ambiguity,
    'Is the Shafi''i hierarchy a reading of the jurisprudential kernel (''divine law requires a unified method for deriving it''), or is it a different kernel altogether (''legal authority derives from hadith expertise'')?',
    'Examination of how the Shafi''i school grounds its legitimacy: as a clarification of divine requirements (reading of the shared kernel) or as a new institutional claim (new kernel). Textual analysis of al-Shafi''i''s al-Risala and foundational texts of Hanafi/Maliki/Hanbali schools to determine whether they are reading the same kernel differently or defending different kernels.',
    'If a reading, the sibling relationships (coexists_with, forecloses, influences) characterize it accurately. If a different kernel, this constraint should be decomposed into a separate family with different network relationships.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, conceptual, 'Whether this constraint is one reading of a shared kernel or claims a distinct kernel.').

omega_variable(
    living_tradition_suppression_mechanism,
    'Is the suppression of customary practice structural (requirements of hadith authentication create barriers) or internalized (communities come to believe custom has no validity unless authenticated)?',
    'Post-suppression trajectory: do communities that lose independent custom authority spontaneously recover local reasoning, or do they remain convinced that textual authentication is necessary? Study of how quickly and completely communities abandoned custom in favor of hadith-authenticated reasoning.',
    'If structural, suppression measure of 0.52 may understate the constraint''s extraction on local communities. If internalized, the constraint carries more inertial persistence—communities enforce it on themselves.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(living_tradition_suppression_mechanism, empirical, 'Whether suppression of custom derives from institutional barriers or internalized belief.').

omega_variable(
    hadith_authenticity_counterfactual,
    'If hadith transmission standards had remained loose and contestable (as they were in earlier periods), would the Shafi''i hierarchy have been adopted, or does the adoption depend on pre-existing high standards of hadith authentication?',
    'Historical study of hadith authentication practices in the century preceding al-Shafi''i; assessment of whether the scientific methods of hadith criticism (chain analysis, transmitter reliability assessment) were mature enough to support the hierarchy as a coordination mechanism.',
    'If hadith standards were already high, the hierarchy coordinates on existing standards. If the hierarchy enabled the tightening of hadith authentication standards, the constraint is partly extractive—it motivated the elevation of a particular methodology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hadith_authenticity_counterfactual, empirical, 'Whether the hierarchy coordinates on pre-existing hadith standards or motivated their elevation.').

omega_variable(
    kernel_reading_family_membership,
    'Are the Hanafi, Maliki, and Hanbali methodologies readings of the same ''how is divine law systematically derived'' kernel as the Shafi''i reading, or do they constitute different kernels (e.g., ''reason and tradition together derive law'' versus ''only transmitted text derives law'')?',
    'Textual analysis of foundational sources (al-Risala, al-Muwatta, Musnad, and early Hanafi principles) to determine whether all four schools are proposing different orderings of the same sources, or whether they disagree on which entities count as sources at all.',
    'If readings of the same kernel, the family decomposition is correct and sibling relationships (coexists_with) characterize them. If different kernels, this story should be decomposed into a separate family with different network structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_family_membership, conceptual, 'Whether the four schools are competing readings of one kernel or defending separate kernels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__shafii_reading, 0, 160).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__shafii_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__shafii_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement(juri_tr_t80, jurisprudential_method_kernel__shafii_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(juri_tr_t120, jurisprudential_method_kernel__shafii_reading, theater_ratio, 120, 0.25).
narrative_ontology:measurement(juri_tr_t160, jurisprudential_method_kernel__shafii_reading, theater_ratio, 160, 0.28).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(juri_be_t80, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 80, 0.6).
narrative_ontology:measurement(juri_be_t120, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 120, 0.65).
narrative_ontology:measurement(juri_be_t160, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 160, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(juri_su_t80, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 80, 0.47).
narrative_ontology:measurement(juri_su_t120, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 120, 0.5).
narrative_ontology:measurement(juri_su_t160, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 160, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__shafii_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__shafii_reading, 0.12).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a shared kernel (jurisprudential method for deriving Islamic law) that decomposes into four constraint stories, one per school. Each reading instantiates a different constraint with its own source hierarchy, beneficiary/victim structure, and extraction profile. The family is linked by network.affects_constraints; sibling readings are declared in cs_structure.reading_relations. The Shafi'i reading (this story) influences the other readings by providing a methodological gold standard that later schools either adopt or explicitly defend against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__shafii_reading, organized, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
