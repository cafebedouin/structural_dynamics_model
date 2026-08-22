% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanbali_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanbali_reading
 *   human_readable: Hanbali Textualist-Traditionist Reading of the Jurisprudential Method Kernel
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   This story authors the Hanbali reading of the shared
 *   jurisprudential-method kernel: law derives strictly from the literal text
 *   of Qur'an and Hadith and from Companion opinions; analogical reasoning
 *   (qiyas) and juristic preference (istihsan) are treated as bid'ah —
 *   innovations that corrupt the kernel; only genuine unanimous consensus
 *   (ijma) is admitted as a supplementary source. This is one of four sibling
 *   readings of the same kernel (Hanafi, Maliki, Shafi'i are the others) —
 *   each is authored as its own constraint with its own ε, per the
 *   ε-invariance principle. The Hanbali reading is distinguished by its
 *   categorical rejection of qiyas as a source, which the Hanafi reading
 *   treats as central and the Shafi'i reading tiers below ijma. This story's
 *   ε is authored for the Hanbali arrangement as the Hanbali reading's own
 *   proponents understand it operating — not for a hypothetical purified
 *   alternative.
 *
 * KEY AGENTS:
 *   - textualist_hadith_scholars: Primary agenda-setter and beneficiary (institutional/arbitrage) — controls what counts as valid text and Companion opinion
 *   - hanbali_madhhab_institutions: Institutional beneficiary (institutional/arbitrage) — patronage and legitimacy flow from methodological purity claims
 *   - rationalist_jurists: Primary target (moderate/constrained) — delegitimized as innovators, barred from consideration
 *   - customary_practice_communities: Secondary target (powerless/trapped) — local practice denied standing as a source
 *   - novel_case_litigants: Diffuse victim (powerless/trapped) — bear the cost of unresolved cases awaiting impossible unanimity
 *   - comparative_legal_historians: Analytical observer — traces the divergence of readings from the shared kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, 0.71).
domain_priors:suppression_score(jurisprudential_method_kernel__hanbali_reading, 0.68).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanbali_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanbali_reading, "Hanbali Textualist-Traditionist Reading of the Jurisprudential Method Kernel").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanbali_reading, "religious/legal/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanbali_reading, '79097264-4bc4-4a5c-a7de-7317022b8328').
narrative_ontology:cs_kernel_codification('79097264-4bc4-4a5c-a7de-7317022b8328', fixed_text).
narrative_ontology:cs_authority_grounding('79097264-4bc4-4a5c-a7de-7317022b8328', lineage).
narrative_ontology:cs_interpretation_layer_present('79097264-4bc4-4a5c-a7de-7317022b8328').
narrative_ontology:cs_reading_relation('79097264-4bc4-4a5c-a7de-7317022b8328', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('79097264-4bc4-4a5c-a7de-7317022b8328', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('79097264-4bc4-4a5c-a7de-7317022b8328', jurisprudential_method_kernel__shafii_reading, influences).
narrative_ontology:cs_axiom('79097264-4bc4-4a5c-a7de-7317022b8328', foundational, analogical_reasoning_is_corrupting_innovation).
narrative_ontology:cs_axiom_status(analogical_reasoning_is_corrupting_innovation, holdable).
narrative_ontology:cs_axiom_grounding('79097264-4bc4-4a5c-a7de-7317022b8328', analogical_reasoning_is_corrupting_innovation, conventional).
narrative_ontology:cs_axiom('79097264-4bc4-4a5c-a7de-7317022b8328', foundational, only_unanimous_consensus_binds).
narrative_ontology:cs_axiom_status(only_unanimous_consensus_binds, holdable).
narrative_ontology:cs_axiom_grounding('79097264-4bc4-4a5c-a7de-7317022b8328', only_unanimous_consensus_binds, conventional).
narrative_ontology:cs_reference_frame('79097264-4bc4-4a5c-a7de-7317022b8328', companion_era_textual_practice).
narrative_ontology:cs_drift_state('79097264-4bc4-4a5c-a7de-7317022b8328', post_abbasid_madhhab_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('79097264-4bc4-4a5c-a7de-7317022b8328', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, textualist_hadith_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, hanbali_madhhab_institutions).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, customary_practice_communities).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, novel_case_litigants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, sunni_ruling_authorities).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, sunni_ruling_authorities).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanbali_reading, sunnah_literalism_doctrine).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanbali_reading, salaf_precedent_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicate what counts as valid textual derivation and which hadith chains are sound. Their scholarly authority rests entirely on being the recognized interpreters of literal text and Companion opinion; every ruling that forecloses analogical reasoning increases dependence on their specific expertise in hadith transmission and text collation.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, textualist_hadith_scholars, agenda_setter,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanbali_reading, textualist_hadith_scholars, beneficiary).

% Schools, endowments, and judicial appointments organized around the textualist method receive patronage, students, and state recognition precisely because they claim methodological purity against 'corrupting' innovation. Institutional survival is tied to maintaining the doctrine that qiyas and istihsan are illegitimate.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, hanbali_madhhab_institutions, beneficiary,
    institutional, generational, arbitrage, regional).

% Jurists who would resolve novel cases through analogical extension of clear textual principles are delegitimized as innovators (ahl al-bid'ah) under this reading. Their reasoning is barred from consideration in jurisdictions where the Hanbali reading holds sway, cutting off their professional standing and their rulings' enforceability.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists, payer,
    moderate, biographical, constrained, regional).

% Local communities whose established practices (urf) do not map onto explicit text or unanimous consensus find their customs delegitimized regardless of longstanding social function. They cannot appeal to local practice as a source of law; only textual or consensus-based rulings are heard, and dissent from remote authorities overrides lived local norms.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, customary_practice_communities, payer,
    powerless, biographical, trapped, local).

% People facing legal questions with no direct textual precedent (new commercial instruments, unprecedented disputes) find the system structurally unable to resolve their cases without either straining a literal text to fit or declaring the matter unresolvable until impossible unanimous consensus is reached. They bear the cost of judicial paralysis or forced textual over-extension.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, novel_case_litigants, payer,
    powerless, immediate, trapped, local).

% Rulers who patronize Hanbali scholars gain a rigid, predictable, hard-to-manipulate legal framework that resists elite juristic reinterpretation in their favor, but also resists administrative flexibility needed to govern efficiently across changing circumstances and new domains of governance (taxation, trade regulation, land tenure).
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, sunni_ruling_authorities, beneficiary,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanbali_reading, sunni_ruling_authorities, payer).

% Study how the four Sunni schools diverged methodologically from a shared kernel (Qur'an, Hadith, early practice) and trace how each reading's self-understanding as 'restoring' rather than 'inventing' method shaped its institutional trajectory and relationship to state power.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanbali_reading, textualist_hadith_scholars).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a maximally conservative, low-discretion method for deriving law that minimizes the risk of individual scholars' reasoning substituting for divine text — coordinating a community of practice around resistance to interpretive drift and protecting against the corruption the reading identifies in analogical schools.
% TRANSFER_FUNCTION: Moves interpretive authority and legal legitimacy from rationalist jurists, customary communities, and anyone whose case lacks direct textual or consensus grounding, to scholars credentialed in hadith transmission and Companion-opinion collation; also transfers dispute-resolution capacity away from flexible local practice toward centralized textual gatekeeping.
% ABSENT_VOICES: Rationalist jurists and their clients are present in the broader jurisprudential debate but structurally excluded from this reading's own adjudicative process — under Hanbali method their reasoning is not merely disagreed with but categorized as bid'ah, disqualifying it from consideration rather than debating it on the merits. Customary communities are rarely represented in scholarly consensus-formation at all.
% DISAPPEARANCE_RATIONALE: If this reading's institutional hold disappeared, jurisdictions currently bound by it would regain access to analogical reasoning and customary practice as legitimate sources, novel commercial and social questions could be resolved without waiting for impossible unanimous consensus, and rationalist jurists would regain professional standing — courts, madrasas, and fatwa councils organized around textualist purity would need to reconstitute their legitimacy claims entirely.
% FOUNDING_PROBLEM: Early Islamic jurisprudence faced a proliferation of local juristic opinion and speculative reasoning (ra'y) that traditionists saw as drifting from the Prophet's actual practice and risking arbitrary law-making by scholars substituting personal judgment for revealed text.
% FOUNDING_PROBLEM_CORROBORATION: Hanbali scholars themselves attest the problem (unchecked ra'y) remains live and their method the necessary corrective. Comparative legal historians and rival-school jurists (Hanafi, Shafi'i) attest the founding problem was real in the 8th-9th century but was already substantially addressed by hadith-criticism methodology and Shafi'i's systematization — the persistence of blanket qiyas rejection past that point serves institutional and doctrinal boundary-maintenance more than the original textual-corruption concern.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanbali_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanbali_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71) because the reading systematically transfers interpretive legitimacy and case-resolution capacity away from rationalist jurists and customary communities toward a narrow textualist-traditionist scholarly class, and because the requirement of unanimous consensus (rather than majority or qualified consensus) is structurally almost never achievable — functionally foreclosing an entire supplementary source while claiming to preserve it. Suppression (0.68) reflects the active delegitimization of qiyas-based rulings as bid'ah — a categorical exclusion, not a mere disagreement, backed by scholarly and often judicial enforcement. Theater ratio is moderate-low but rising (0.10 to 0.28) — the coordination function (guarding against interpretive drift) was substantively real in early formation but an increasing share of activity over time is doctrinal boundary-maintenance and credentialing rather than active resolution of the corruption problem it names.
 *
 * PERSPECTIVAL GAP:
 *   From the textualist scholar's seat, the reading is a restoration of the kernel's original purity against subsequent corruption — a defensive, not extractive, posture. From the rationalist jurist's seat or the novel-case litigant's seat, the identical structure operates as an arbitrary foreclosure of legitimate legal reasoning that leaves real disputes unresolved. The engine computes these divergent per-seat classifications from the declared power/exit/scope data; this story does not average or reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   Textualist hadith scholars and Hanbali institutions sit near the full-beneficiary end: their authority is structurally created and sustained by the categorical exclusion of alternative reasoning methods, and they have durable institutional exit options (arbitrage) via patronage networks. Rationalist jurists and customary communities sit near the full-target end: their professional and social standing is directly diminished by the same exclusion, and their exit options are constrained or trapped — a rationalist jurist cannot simply relocate their reasoning into legitimacy within a jurisdiction bound by this reading, and a local community cannot appeal past the doctrine to have its custom recognized. Sunni ruling authorities are dual-positioned: they benefit from a rigid, elite-reinterpretation-resistant framework but pay a cost in administrative flexibility.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unchecked juristic speculation drifting from Prophetic practice) was substantially addressed once hadith-criticism methodology matured and other schools (notably Shafi'i) systematized sourcing hierarchies. The Hanbali reading's continued categorical rejection of qiyas — rather than a qualified, criteria-bound admission of analogy — persists status quo tangled-rope classification: it retains a genuine coordination function (protecting textual fidelity) while the requirement of literal unanimity has hardened into a mechanism that forecloses resolution of novel cases altogether, which serves the institutional interests of the textualist scholarly class more than it serves the original anti-corruption purpose. Classifying this as tangled_rope rather than snare acknowledges the real, non-fabricated coordination function (textual fidelity has genuine value) while registering the asymmetric extraction from rationalist jurists and customary communities enforced through active delegitimization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'Is the disagreement between the Hanbali and Hanafi/Shafi''i readings located in what counts as a legitimate SOURCE of law (textual literalism vs. reasoned extension) or in who holds the AUTHORITY to declare a source legitimate (traditionist hadith-scholars vs. jurist-reasoners)?',
    'Comparative doctrinal-historical analysis of founding texts (Ahmad ibn Hanbal''s own methodological statements vs. Abu Hanifa''s and al-Shafi''i''s) to determine whether the dispute is primarily epistemic (what is a valid source) or primarily institutional (who adjudicates).',
    'If primarily epistemic, the readings genuinely coexist as different theories of legal epistemology; if primarily institutional, the Hanbali reading''s rejection of qiyas is better read as a professional-boundary mechanism protecting hadith-transmission expertise against jurist-reasoner competition, raising its extractiveness reading further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Whether the Hanbali/Hanafi split is epistemic or institutional-professional in character.').

omega_variable(
    unanimous_consensus_achievability,
    'Was the Hanbali requirement of literal unanimous consensus (rather than qualified majority consensus, as some later Hanbali jurists themselves softened toward) intended as an achievable standard or as a de facto veto mechanism against novel rulings?',
    'Historical survey of how often unanimous ijma was actually invoked to resolve a live legal question in Hanbali jurisprudence versus how often the requirement resulted in declared judicial silence (tawaqquf) on unresolved matters.',
    'If the requirement was rarely if ever met and mostly produced judicial silence, this substantially raises confidence that the consensus requirement functions as extraction (foreclosing resolution) rather than genuine coordination (achieving durable agreement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unanimous_consensus_achievability, empirical, 'Whether unanimous-consensus requirement is a workable standard or a functional veto.').

omega_variable(
    natural_law_vs_constructed_reading,
    'Does the Hanbali reading represent a genuine recovery of the kernel''s original, uncorrupted form (as its proponents claim), or is ''textual literalism plus Companion opinion plus unanimous consensus'' itself a later methodological construction contingent on Ahmad ibn Hanbal''s specific historical context (reaction to Mu''tazilite rationalism and the Mihna)?',
    'Historical-critical analysis of early Islamic legal practice prior to the formal madhhab system, checking whether analogical reasoning was in fact used by the earliest generations the Hanbali reading claims as its textual and practical basis.',
    'If early practice already included informal analogical reasoning, the Hanbali reading''s claim to be restoring an uncorrupted original method is itself a constructed historical narrative that benefits textualist scholars by asserting priority and purity — reinforcing this as a false-summit-adjacent reading rather than a pure recovery.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, conceptual, 'Whether the Hanbali reading recovers or constructs its claimed original method.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanbali_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(juri_tr_t20, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(juri_tr_t60, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(juri_tr_t80, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement(juri_tr_t100, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(juri_be_t20, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(juri_be_t60, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(juri_be_t80, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(juri_be_t100, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 100, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(juri_su_t20, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(juri_su_t60, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(juri_su_t80, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 80, 0.65).
narrative_ontology:measurement(juri_su_t100, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanbali_reading, identity_coordination).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, shafii_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the shared jurisprudential_method_kernel, each authored as a separate constraint per the ε-invariance principle: hanafi_reading (qiyas/istihsan as central legitimate tools), hanbali_reading (this story — categorical rejection of qiyas, literalism, unanimous consensus only), maliki_reading (living Medinan practice as source), shafii_reading (strict four-tier hierarchy standardizing hadith transmission as arbiter). Each carries its own ε reflecting how thoroughly that reading forecloses non-textual reasoning: Hanbali is authored with the highest ε among the four due to its categorical rejection of analogy entirely, versus Shafi'i's tiered admission of qiyas as a fourth-order source and Hanafi's central use of it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
