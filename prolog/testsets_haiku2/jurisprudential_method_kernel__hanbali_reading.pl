% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: jurisprudential_method_kernel__hanbali_reading
 *   human_readable: Hanbali Textualist Jurisprudential Method: Literal Qur'an/Hadith Plus Companion Opinion
 *   domain: religious/legal/epistemological
 *
 * SUMMARY:
 *   The Hanbali textualist reading of Islamic jurisprudential method asserts
 *   that law derives ONLY from the literal text of Qur'an and Hadith, plus
 *   the explicit opinions of the Prophet's Companions. Analogical reasoning
 *   (qiyas) and juristic preference (istihsan)—tools the Hanafi, Maliki, and
 *   Shafi'i schools employ—are condemned as bid'ah (blameworthy innovation)
 *   that corrupt the divine kernel. Only unanimous scholarly consensus (ijma)
 *   is valid beyond these sources. This reading benefits textualist scholars
 *   and Hadith specialists by concentrating interpretive authority on literal
 *   transmission and Companion preservation; it harms rationalist jurists and
 *   customary practice communities who rely on analogical extension to make
 *   law responsive to novel cases. The claim/metric gap is deliberate and
 *   structural: this reading claims to be pure coordination (locking
 *   interpretation to a single text), but the authored metrics describe
 *   substantially extractive operation (0.78) because the method requires
 *   active suppression (0.71) of alternative juristic tools and imposes high
 *   accessibility collapse (0.68)—alternatives exist and practitioners
 *   resist. The constraint is a READING of the jurisprudential-method kernel,
 *   not a description of how all Islamic jurisprudence works; it is one
 *   party's answer to how the kernel should be interpreted and enforced.
 *
 * KEY AGENTS:
 *   - Hanbali textualist scholars: control interpretive authority by claiming literal text is the only legitimate source
 *   - Rationalist jurists (Hanafi, Maliki, Shafi'i schools): employ analogical reasoning and juristic preference, declare them valid tools, resist the textualist delegitimization
 *   - Hadith transmitters and Companion-opinion preservers: gain authority under the textualist method
 *   - Customary practice communities: lose authority when their traditions are reframed as bid'ah
 *   - Lay Muslim communities: excluded from the kernel contest; subject to whatever method local authorities enforce
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, 0.78).
domain_priors:suppression_score(jurisprudential_method_kernel__hanbali_reading, 0.71).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanbali_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanbali_reading, "Hanbali Textualist Jurisprudential Method: Literal Qur'an/Hadith Plus Companion Opinion").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanbali_reading, "religious/legal/epistemological").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanbali_reading, '075829a4-d59e-40b2-81c9-c5b16a5ce0ca').
narrative_ontology:cs_kernel_codification('075829a4-d59e-40b2-81c9-c5b16a5ce0ca', fixed_text).
narrative_ontology:cs_authority_grounding('075829a4-d59e-40b2-81c9-c5b16a5ce0ca', lineage).
narrative_ontology:cs_interpretation_layer_present('075829a4-d59e-40b2-81c9-c5b16a5ce0ca').
narrative_ontology:cs_reading_relation('075829a4-d59e-40b2-81c9-c5b16a5ce0ca', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('075829a4-d59e-40b2-81c9-c5b16a5ce0ca', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('075829a4-d59e-40b2-81c9-c5b16a5ce0ca', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('075829a4-d59e-40b2-81c9-c5b16a5ce0ca', foundational, analogical_reasoning_corrupts_kernel).
narrative_ontology:cs_axiom_status(analogical_reasoning_corrupts_kernel, holdable).
narrative_ontology:cs_axiom_grounding('075829a4-d59e-40b2-81c9-c5b16a5ce0ca', analogical_reasoning_corrupts_kernel, deontological).
narrative_ontology:cs_axiom('075829a4-d59e-40b2-81c9-c5b16a5ce0ca', foundational, literal_text_and_companion_opinion_are_sufficient).
narrative_ontology:cs_axiom_status(literal_text_and_companion_opinion_are_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('075829a4-d59e-40b2-81c9-c5b16a5ce0ca', literal_text_and_companion_opinion_are_sufficient, conventional).
narrative_ontology:cs_reference_frame('075829a4-d59e-40b2-81c9-c5b16a5ce0ca', literal_text_and_companion_preservation).
narrative_ontology:cs_drift_state('075829a4-d59e-40b2-81c9-c5b16a5ce0ca', contemporary_islamic_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('075829a4-d59e-40b2-81c9-c5b16a5ce0ca', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, hanbali_textualist_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, literalist_juristic_lineages).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, customary_practice_communities).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, jurists_employing_analogical_reasoning).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, companion_opinion_transmitters).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, consensus_establishing_authorities).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, other_juridical_schools).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defend and transmit the literal-text method as the only legitimate path to divine law. They set the interpretive standard by controlling education in madrasas, issuing fatwa, and transmitting juristic lineage. Their authority depends on the claim that reason (qiyas, istihsan) corrupts the kernel; their institutional position is threatened if analogical reasoning is readmitted as valid.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, hanbali_textualist_scholars, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Employ analogical reasoning, juristic preference, and reasoned extension to make the law work in novel cases (contracts for new goods, technical questions unaddressed in Hadith). The Hanbali method accuses them of bid'ah and delegitimizes their reasoning process, forcing them to either adopt the textualist frame or defend their methods against the institutional authority the frame commands.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists, payer,
    institutional, civilizational, constrained, global).

% Live under laws derived from local practice, analogical extension, and juristic preference (the living traditions the Maliki school honors, or the istihsan the Hanafi school permits). Under the Hanbali reading, their customary laws are reframed as bid'ah and illegitimate; their exit is to restructure their practice around literal Hadith or abandon their tradition entirely.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, customary_practice_communities, payer,
    moderate, biographical, constrained, regional).

% Hadith scholars and transmitters of Companion (Sahaba) opinions gain interpretive authority under the Hanbali method because their role is to preserve and authenticate the literal text and its immediate context. They are the official readers of what the kernel actually says.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, companion_opinion_transmitters, beneficiary,
    institutional, civilizational, mobile, global).

% The Hanafi, Maliki, and Shafi'i schools each employ methods the Hanbali reading condemns as innovation. They carry institutional authority, scholarly tradition, and regional followings; the Hanbali claim that their methods corrupt the kernel creates a structural competition for legitimacy without foreclosing any school logically—each can coexist by appealing to its own adherents.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, other_juridical_schools, payer,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanbali_reading, other_juridical_schools, observer).

% Are subject to whatever juristic method their local authorities adopt and enforce. They have no seat at the table where jurisprudential methods are debated; their exclusion is structural—the kernel contest is between learned authorities, and lay communities experience its outcome as settled.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, lay_muslim_communities, excluded,
    powerless, biographical, constrained, local).

% Under the Hanbali reading, only unanimous consensus (ijma) is valid beyond literal text and Companion opinion. This concentrates authority in whoever can speak for the consensus of the learned. The requirement for unanimity makes consensus rare, which actually reinforces deference to literal text.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, consensus_establishing_authorities, beneficiary,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared epistemological standard for deriving law from a sacred kernel (Qur'an and Hadith), enabling Muslims across regions and generations to recognize law as legitimate. The literal-text method coordinates on a single interpretive anchor rather than allowing each jurist to rationalize differently.
% TRANSFER_FUNCTION: Moves interpretive authority from generalist rationalist jurists (who can analogize and prefer) to specialist textualist scholars (who read Hadith and preserve Companion opinions). It transfers the capacity to legitimate novel legal claims from those employing reason to those guarding literal transmission.
% ABSENT_VOICES: Lay Muslim communities have no formal seat; practical jurists in remote regions who rely on analogical reasoning to solve real cases are excluded from the kernel contest. Philosophers and rational theologians (mu'tazila-influenced thinkers) are structurally shut out because their tools (reason, analogy) are declared bid'ah. Hadith forgers and weak transmitters would object to the weight placed on Companion opinion if authentication standards were contested, but they are absent from the primary dispute.
% DISAPPEARANCE_RATIONALE: If the Hanbali textualist method disappeared and analogical reasoning were readmitted as legitimate, the entire institutional authority structure of textualist jurisprudence would collapse. Rationalist schools would regain legitimacy, customary practices would be reframed as valid extensions rather than corruption, and the coordination on literal text would dissolve into competing methodological schools. Regional legal systems would restructure around their preferred juristic methods.
% FOUNDING_PROBLEM: In early Islam, interpretive chaos threatened legal coherence—different regions and different jurists arrived at inconsistent legal conclusions by extending Qur'an and Hadith differently. Some scholars feared that reason (qiyas, istihsan) was being abused to justify any outcome. The textualist method was constructed to lock interpretation to the literal text and Companion practice, preventing speculative analogies from overriding explicit sources.
% FOUNDING_PROBLEM_CORROBORATION: Hanbali scholars and their defenders attest the problem is live—they cite ongoing disputes and rationalist abuses as evidence that reason-based methods produce illegitimate rulings. Rationalist jurists (Hanafi, Maliki, Shafi'i schools) and their modern defenders attest the problem is partly solved by disciplined use of analogy and that the textualist response creates NEW problems by making law rigid and unresponsive to novel cases. Contemporary Islamic legal scholars and comparative-law studies outside the beneficiary circle document that all major schools coexist and that the founding problem (interpretive chaos) has been managed by methodological pluralism rather than textualist exclusivity.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanbali_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanbali_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.78) and rising across the interval because the textualist method's grip on interpretive authority strengthens—it succeeds in reframing rationalist methods as corruption, and that success transfers power from generalist jurists to specialists. Suppression is substantial (0.71) because maintaining the literal-text standard requires actively excluding alternative reasoning tools: suppressing qiyas, delegitimizing istihsan, enforcing the bid'ah label on deviations. Theater is moderate (0.42) because the literal-text method does have a real coordination function (settling interpretation to a single anchor), but as it strengthens, an increasing share of enforcement effort goes to policing boundaries rather than solving coordination problems. The measurement series tracks the consolidation of textualist authority: extractiveness and suppression both rise as the method becomes more institutionalized and its hold on jurisprudential legitimacy deepens. The interval spans centuries of institutional development, measured at key transition points where textualist authority becomes more entrenched.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Hanbali textualist scholars) experiences the method as pure coordination—a shared standard that enables lawmaking. From the payer seats (rationalist jurists, customary practice communities), the same structure operates as extraction: they lose interpretive authority, their methods are delegitimized, and their exit options narrow. The engine computes divergence from the structural data: the beneficiaries' power is institutional and their time horizon civilizational (they control succession and teaching); the payers' exit is constrained or identity-locked (they cannot abandon juristic reasoning without abandoning their professional and intellectual identity). This structural asymmetry drives the computed per-seat divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanbali textualist scholars are the beneficiaries (d near 0.0 to 0.2): they collect interpretive authority, set the standard, and their position strengthens over time. Rationalist jurists are targets (d near 0.7 to 0.9): they bear the cost of delegitimization, their methods are declared invalid, and their exit is constrained by institutional inertia and professional identity-lock. Customary practice communities are targets (d near 0.75 to 0.85): they lose the authority to maintain their traditions under the local juristic methods they prefer; exit means abandoning their own law. Companion-opinion transmitters are beneficiaries (d near 0.1 to 0.25): their role as guardians of the literal text is elevated. No directionality overrides are needed because the structural derivation (beneficiary/victim declarations + power levels + exit options) produces accurate directionality values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was interpretive chaos—different jurists reaching inconsistent conclusions through unbridled reasoning. The textualist method was built to solve this by locking interpretation to literal text. The mandatrophy question: Is that founding problem still live, or has it been solved by other means (methodological pluralism, institutional structure, tradition)? The authored founding_problem_status is 'contested' because Hanbali defenders attest the problem remains (rationalist abuses continue) while rationalist schools and external observers attest the problem is substantially managed. The rising extractiveness and theater_ratio measurements suggest the method persists less because it solves the original problem and more because it concentrates authority—a classic mandatrophy signature. The constraint prevents misclassifying this as pure rope (coordination) by naming the victims and measuring suppression high: rationalist jurists are paying the cost of the method's persistence, not benefiting from it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bid_ah_definition_boundary,
    'What constitutes bid''ah (blameworthy innovation) versus legitimate juristic development? Is analogical reasoning inherently corrupt, or is it a valid tool when used disciplinedly?',
    'Philological study of early Islamic sources and Hanbali juristic texts to establish what Ahmad ibn Hanbal and his immediate successors actually condemned versus what later textualists attributed to them. Comparison with how other schools (Maliki, Hanafi) justify their own analogical practices.',
    'If analogical reasoning is shown to have been used legitimately in early Islamic jurisprudence, the textualist reading must reframe the constraint not as ''analogical reasoning is forbidden'' but as ''analogical reasoning must follow strict rules.'' This would reclassify the constraint from tangled_rope toward rope (less extraction, less suppression). If bid''ah is shown to be a historically contingent label applied to Hanafi/Maliki methods for political rather than juristic reasons, the extraction becomes predominantly political (beneficiaries are textualist political actors, not coordinate-solving authorities).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bid_ah_definition_boundary, empirical, 'Whether analogical reasoning is structurally forbidden or contingently delegitimized.').

omega_variable(
    kernel_vs_reading_stability,
    'Is the Qur''an-and-Hadith kernel stable enough to support textualist interpretation, or does the kernel itself require interpretive reasoning to be applied to new contexts?',
    'Pragmatic test: Can Islamic law operate under strict textualism in rapidly changing societies without breaking down (modern financial instruments, digital commerce, medical ethics)? If textualism requires continuous juristic adaptation (which is then claimed to be non-qiyas reasoning but functions equivalently), the distinction between textualism and analogical reasoning collapses empirically.',
    'If textualism breaks down in practice and is supplemented with reasoning that functions like qiyas but is relabeled to preserve textualist identity, the constraint''s type shifts from tangled_rope to piton (persistence by inertia and semantic relabeling rather than functional legitimacy). If strict textualism suffices, the constraint''s extractiveness may be lower than authored—it would be functional coordination, not masked extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_vs_reading_stability, empirical, 'Whether textualism is operationally viable or requires disguised analogical reasoning to function.').

omega_variable(
    institutional_capture_vs_methodological_truth,
    'Is the textualist method''s institutional strength driven by its epistemic validity (it actually produces better law) or by its capture of madrasa authority and control of succession?',
    'Institutional history comparing the rise of textualist schools to political and educational patronage patterns. Jurisprudential comparison: do textualist fatwas reliably solve practical problems better than rationalist ones, or do they require supplementation and reinterpretation?',
    'If the method''s strength is primarily institutional rather than epistemic, the constraint is better classified as snare (extraction disguised as methodology) or piton (persistence by institutional inertia) than as tangled_rope. The beneficiary would shift from ''textualist scholars'' to ''whoever controls madrasa authority,'' and the suppression would target practical alternative reasoning rather than defending an internally coherent method.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_vs_methodological_truth, conceptual, 'Whether textualist dominance is grounded in juristic validity or institutional capture.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'To what extent is the suppression of analogical reasoning structural (enforced by institutional authority, fatwa, exclusion from teaching) versus internalized (rationalist jurists accepting the textualist frame as legitimate even when not under direct enforcement)?',
    'Historical and ethnographic study of how rationalist jurists respond when textualist enforcement machinery is absent or weak. Do they spontaneously adopt textualist methods or revert to analogical reasoning? What happens in Muslim-majority regions where non-Hanbali schools dominate authority?',
    'If suppression is primarily structural, removing institutional enforcement would dissolve the constraint quickly—exit options would open and payer seats would revert to analogical reasoning. If suppression is internalized (scholars have genuinely accepted the textualist frame as epistemically valid), the constraint persists even without enforcement—the constraint is stronger than the structural measure suggests. High internalization would support reclassification as mountain-like (felt as natural law rather than imposed rule).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression is structural/institutional or internalized/epistemic.').

omega_variable(
    kernel_reading_committer_frame,
    'This constraint is ONE reading of the jurisprudential-method kernel. How defensible is the textualist reading compared to its siblings, and what would change the reading''s empirical or normative status?',
    'Comparative jurisprudential analysis: do the four schools (Hanbali, Hanafi, Maliki, Shafi''i) each claim to follow Qur''an and Hadith, but disagree on method? If so, the dispute is methodological, not factual—no empirical evidence resolves it. Examine whether modern Islamic legal reform and comparative law have converged on any methodological standard.',
    'If the readings are genuinely incommensurable (each party believes its method is valid and others are not, and no shared metric exists), the kernel contest is permanent—coexistence is the stable outcome, not displacement. If evidence accumulates that some methods work better for modern governance (e.g., the Shafi''i hierarchy is more predictable than textualism), that evidence would shift legitimacy over time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Whether the kernel contest is empirically resolvable or permanently contested.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanbali_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(juri_tr_t0, observed).
narrative_ontology:measurement(juri_tr_t2, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 2, 0.31).
narrative_ontology:measurement_basis(juri_tr_t2, observed).
narrative_ontology:measurement(juri_tr_t4, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 4, 0.34).
narrative_ontology:measurement_basis(juri_tr_t4, observed).
narrative_ontology:measurement(juri_tr_t7, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 7, 0.38).
narrative_ontology:measurement_basis(juri_tr_t7, observed).
narrative_ontology:measurement(juri_tr_t10, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(juri_tr_t10, observed).
narrative_ontology:measurement(juri_tr_t14, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 14, 0.42).
narrative_ontology:measurement_basis(juri_tr_t14, observed).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(juri_be_t0, observed).
narrative_ontology:measurement(juri_be_t2, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 2, 0.68).
narrative_ontology:measurement_basis(juri_be_t2, observed).
narrative_ontology:measurement(juri_be_t4, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 4, 0.71).
narrative_ontology:measurement_basis(juri_be_t4, observed).
narrative_ontology:measurement(juri_be_t7, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 7, 0.75).
narrative_ontology:measurement_basis(juri_be_t7, observed).
narrative_ontology:measurement(juri_be_t10, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 10, 0.77).
narrative_ontology:measurement_basis(juri_be_t10, observed).
narrative_ontology:measurement(juri_be_t14, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 14, 0.78).
narrative_ontology:measurement_basis(juri_be_t14, observed).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(juri_su_t0, observed).
narrative_ontology:measurement(juri_su_t2, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 2, 0.59).
narrative_ontology:measurement_basis(juri_su_t2, observed).
narrative_ontology:measurement(juri_su_t4, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement_basis(juri_su_t4, observed).
narrative_ontology:measurement(juri_su_t7, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 7, 0.67).
narrative_ontology:measurement_basis(juri_su_t7, observed).
narrative_ontology:measurement(juri_su_t10, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement_basis(juri_su_t10, observed).
narrative_ontology:measurement(juri_su_t14, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 14, 0.71).
narrative_ontology:measurement_basis(juri_su_t14, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanbali_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__hanbali_reading, 0.18).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__shafii_reading).

% DUAL FORMULATION NOTE:
% The jurisprudential-method kernel decomposes into four constraint stories, one per reading (Hanbali, Hanafi, Maliki, Shafi'i). Each reading instantiates a different constraint with a different ε, different beneficiary/victim structure, and different type classification. The Hanbali reading rejects analogical reasoning (qiyas) and juristic preference (istihsan) as bid'ah; the Hanafi reading embraces them as valid juristic tools; the Maliki reading grounds law in Medinan practice; the Shafi'i reading establishes a strict four-tier hierarchy. Each constraint story addresses the same kernel (how to extend Qur'an and Hadith to novel cases) but from a different reading, producing different classifications. The readings are linked via network.affects_constraints because each reading's acceptance influences the legitimacy of the others—adopting the Hanbali textualist method delegitimizes the Hanafi analogical method, while Hanafi dominance in a region elevates analogical reasoning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
