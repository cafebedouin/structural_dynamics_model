% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanafi_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jurisprudential_method_kernel__hanafi_reading
 *   human_readable: Hanafi Jurisprudential Method: Rationalist Extension of Divine Intent
 *   domain: legal/philosophical/institutional
 *
 * SUMMARY:
 *   Islamic jurisprudence contests the proper relationship between textual
 *   sources (Qur'an and hadith) and human reason. The Hanafi reading
 *   instantiates one pole of this contest: reason, deployed as systematic
 *   analogical reasoning (qiyas) and juristic preference (istihsan), is a
 *   legitimate tool for extending divine intent to novel legal cases. This
 *   reading was developed in early Islamic jurisprudence to address cases not
 *   explicitly covered in the textual kernel and became institutionally
 *   dominant through Ottoman legal governance and modern Islamic state
 *   apparatus. The rival Hanbali reading rejects rationalist extension as
 *   bid'ah (innovation), while Maliki jurisprudence grounds authenticity in
 *   living Medinan tradition, and Shafi'i jurisprudence establishes a strict
 *   hierarchy with hadith transmission as arbiter. The Hanafi reading
 *   benefits rationalist jurists and institutional governance while
 *   marginalizing textualist and literalist claims to exclusive authenticity.
 *   The constraint operates as a tangled rope: genuine coordination function
 *   (addressing novel legal questions through reason) combined with
 *   asymmetric extraction (textualist interpretive claims displaced from
 *   authority, lay textual understanding systematically subordinated to
 *   trained rationalist mediation).
 *
 * KEY AGENTS:
 *   - Rationalist jurists (Hanafi-trained): benefit from validation of analogical reasoning and juristic preference as legitimate jurisprudential labor
 *   - Hanafi institutional apparatus: agenda-setter; administers and enforces the rationalist method through teaching, fatwa networks, judicial systems
 *   - Textualist legal claims: payer; marginalized as epistemically unsophisticated within the dominant Hanafi framework
 *   - Literal interpretation adherents: payer and excluded; lack institutional resources to maintain competing methodologies at scale; identity-locked to textual authenticity
 *   - Hanbali school: institutional payer; textualist position subordinated despite regional institutional strength
 *   - Novel case stakeholders (powerless): beneficiary but dependent; benefit from reasoned legal rulings on modern questions but exercise no control over derivation
 *   - Maliki and Shafi'i schools: distinct institutional competitors; not directly extracted by Hanafi method but occupy subordinate epistemic positions
 *   - Islamic institutional governance: joint beneficiary and agenda-setter; depends on Hanafi rationalist latitude for legal innovation and governance reform
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, 0.68).
domain_priors:suppression_score(jurisprudential_method_kernel__hanafi_reading, 0.41).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanafi_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanafi_reading, "Hanafi Jurisprudential Method: Rationalist Extension of Divine Intent").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanafi_reading, "legal/philosophical/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanafi_reading, '8a6bba39-ff25-4a8c-910e-aa4254e66d40').
narrative_ontology:cs_kernel_codification('8a6bba39-ff25-4a8c-910e-aa4254e66d40', fixed_text).
narrative_ontology:cs_authority_grounding('8a6bba39-ff25-4a8c-910e-aa4254e66d40', lineage).
narrative_ontology:cs_interpretation_layer_present('8a6bba39-ff25-4a8c-910e-aa4254e66d40').
narrative_ontology:cs_reading_relation('8a6bba39-ff25-4a8c-910e-aa4254e66d40', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a6bba39-ff25-4a8c-910e-aa4254e66d40', jurisprudential_method_kernel__maliki_reading, influences).
narrative_ontology:cs_reading_relation('8a6bba39-ff25-4a8c-910e-aa4254e66d40', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('8a6bba39-ff25-4a8c-910e-aa4254e66d40', foundational, reason_is_legitimate_jurisprudential_tool).
narrative_ontology:cs_axiom_status(reason_is_legitimate_jurisprudential_tool, holdable).
narrative_ontology:cs_axiom_grounding('8a6bba39-ff25-4a8c-910e-aa4254e66d40', reason_is_legitimate_jurisprudential_tool, instrumental).
narrative_ontology:cs_axiom('8a6bba39-ff25-4a8c-910e-aa4254e66d40', foundational, analogical_extension_is_faithful_derivation).
narrative_ontology:cs_axiom_status(analogical_extension_is_faithful_derivation, holdable).
narrative_ontology:cs_axiom_grounding('8a6bba39-ff25-4a8c-910e-aa4254e66d40', analogical_extension_is_faithful_derivation, deontological).
narrative_ontology:cs_reference_frame('8a6bba39-ff25-4a8c-910e-aa4254e66d40', quranic_textual_kernel_extended_by_reason).
narrative_ontology:cs_drift_state('8a6bba39-ff25-4a8c-910e-aa4254e66d40', contemporary_institutional_governance, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8a6bba39-ff25-4a8c-910e-aa4254e66d40', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, rationalist_jurists).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, hanafi_institutional_apparatus).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, textualist_legal_claims).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, literal_interpretation_adherents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, novel_case_stakeholders).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, islamic_institutional_governance).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, hanbali_school).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, reason_as_divine_tool).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, living_jurisprudential_tradition).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, analogical_extension_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trained in sophisticated analogical reasoning (qiyas) and juristic preference (istihsan). The Hanafi method validates their intellectual labor as a legitimate tool for extending divine intent to novel cases not explicitly covered in the Qur'an or authenticated hadith. Their professional prestige, institutional authority, and career advancement depend on the standing of rationalist jurisprudence within Islamic legal tradition. They benefit from a methodological framework that elevates reason to coordinate alongside textual sources.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, rationalist_jurists, beneficiary,
    organized, generational, mobile, global).

% The Hanafi school's institutional survival and authority depend on maintaining the legitimacy of rationalist jurisprudence. Hadith scholars, mufti networks, Islamic courts applying Hanafi methodology, and teaching institutions across the Ottoman legacy, post-colonial states, and diaspora communities administer and enforce the Hanafi method. They set standards for acceptable jurisprudential practice, train successive generations, and defend the method against rival readings. Institutionally extracting authority from the claim that reason, properly deployed, remains a faithful servant of divine intent.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hanafi_institutional_apparatus, agenda_setter,
    institutional, civilizational, trapped, global).

% The claim that law derives exclusively from literal textual sources (Qur'an and authenticated hadith) is marginalized by the Hanafi framework, which admits rationalist extension as valid derivation. Textualist scholars must either adopt the rationalist framework, operate in parallel institutional spaces (Hanbali and literalist networks), or engage in sustained critique from a subordinate epistemic position. Their textual literalism bears the cost of being read as narrow or insufficiently responsive to novel circumstances that the Hanafi method claims to address through reason.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, textualist_legal_claims, payer,
    moderate, civilizational, constrained, global).

% Lay believers and jurists who hold that the Qur'an and hadith are self-interpreting and require no additional rationalist mediation are systematically excluded from the Hanafi jurisprudential conversation on the ground that their position is epistemically unsophisticated or theologically inadequate. Their identity as Muslims committed to textual authenticity becomes framed as obstacle to legal reasoning rather than as legitimate epistemic position. They lack institutional resources to maintain competing jurisprudential schools or publish alternate methodologies at scale.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, literal_interpretation_adherents, payer,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, literal_interpretation_adherents, excluded).

% Operates a rival jurisprudential method grounding itself in Medinan practice ('amal) as a coordinate source alongside Qur'an and hadith. The Hanafi elevation of rationalist reasoning (qiyas/istihsan) partially displaces the Maliki reliance on living tradition as the arbiter of authenticity. Maliki institutional authority is strongest in regional networks (North Africa, West Africa) but is marginalized in global Islamic institutions increasingly shaped by Ottoman (Hanafi) and modern nation-state (Hanafi/Shafi'i) precedent.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, maliki_school, excluded,
    institutional, civilizational, constrained, regional).

% The Hanbali school explicitly rejects rationalist extension (qiyas and istihsan) as bid'ah (innovation corrupting the kernel). The Hanafi framework's success in establishing rationalist jurisprudence as authentically Islamic places the Hanbali textualist position at a perpetual epistemic disadvantage within broader Islamic discourse, even though Hanbali institutional networks (Saudi judicial apparatus, certain reform movements) remain powerful regionally. They bear the cost of defending their method while being positioned as insufficiently responsive to legal novelty.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hanbali_school, payer,
    institutional, civilizational, constrained, regional).

% Al-Shafi'i's methodological systematization (strict four-tier hierarchy: Qur'an → Hadith → Ijma → Qiyas) occupies a middle position: it validates qiyas as a legitimate source but subordinates it to hadith transmission as the arbiter. The Shafi'i school observes the Hanafi-Hanbali contest from a position of methodological distinction but does not directly benefit or bear extraction costs from the Hanafi rationalist framing; it competes on a different axis (transmission rigor vs. extension latitude).
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, shafii_school, observer,
    institutional, civilizational, constrained, regional).

% Individuals facing legal questions not explicitly addressed in Qur'an or authenticated hadith (modern financial instruments, bioethics, technology governance, etc.) benefit from a jurisprudential method that claims to derive valid answers through rationalist extension. They are dependent on the legitimacy of the Hanafi method to obtain authoritative rulings; if only literal textual sources were valid, many modern questions would be classified as outside the law's scope. Their benefit is asymmetric: they depend on rationalist jurisprudence but exercise no control over its deployment.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, novel_case_stakeholders, beneficiary,
    powerless, immediate, trapped, universal).

% Modern Islamic states, supra-national Islamic organizations, and transnational Islamic financial institutions depend on the Hanafi method's rationalist latitude to justify governance, legal reform, and economic innovation within an Islamic framework. The method allows institutional leadership to claim continuity with Islamic law while adapting to novel circumstances. The institutional apparatus benefits from the method's flexibility while enforcing it as the authoritative standard.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, islamic_institutional_governance, agenda_setter,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, islamic_institutional_governance, beneficiary).

% Comparative legal scholars, Islamic studies academics, and jurisprudential historians who examine the kernel contest from outside any confessional commitment. They observe the Hanafi reading as one interpretive instantiation among several, tracking how methodological claims generate institutional winners and losers.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanafi_reading, hanafi_institutional_apparatus).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of legal innovation: how to extend divine law to circumstances not explicitly addressed in the Qur'an or hadith without abandoning the claim that law derives from revelation. The Hanafi method coordinates around reason as a legitimate tool for analogical extension, allowing Islamic law to address novel cases while maintaining continuity with the textual kernel.
% TRANSFER_FUNCTION: Transfers epistemological authority from literal textual interpretation to trained rationalist jurists. Jurists with advanced training in qiyas and istihsan become the authoritative mediators between divine text and legal ruling; their professional labor (analogical reasoning, juristic preference) is validated and compensated through institutional authority. Textualist interpretive claims are displaced from the center of Islamic jurisprudential legitimacy.
% ABSENT_VOICES: Literal textualist interpreters and believers committed to the self-sufficiency of Qur'an and hadith are structurally excluded from the Hanafi jurisprudential conversation — their position is framed as epistemically unsophisticated rather than as a legitimate alternative reading. Lay believers whose understanding of Islamic law is based on direct textual engagement rather than trained rationalist analysis are effectively absent from the scholarly discourse that sets binding precedent.
% DISAPPEARANCE_RATIONALE: If the Hanafi rationalist method vanished overnight, Islamic law would fragment into literal textualism (Hanbali, literalist) and living-tradition frameworks (Maliki, practice-based). Governance, Islamic finance, and legal rulings on novel cases would reorganize around either textualist constraint (narrow scope of valid rulings) or competing institutional methodologies. The global Islamic institutional apparatus built on Hanafi jurisprudence would require wholesale reconstitution or would fragment into regional schools of authority.
% FOUNDING_PROBLEM: Early Islamic jurisprudence faced the challenge of addressing legal questions that arose after the Prophet's death but were not explicitly covered in the Qur'an or authenticated hadith. The Hanafi method was developed to resolve this through systematic analogical reasoning (qiyas) and juristic preference (istihsan), extending divine intent through reason to novel cases.
% FOUNDING_PROBLEM_CORROBORATION: The Hanafi institutional apparatus and rationalist jurists affirm the founding problem is live: novel cases continue to emerge (modern finance, bioethics, technology) and require reasoned extension of divine intent. Hanbali textualists and literalist scholars contest this, arguing the founding problem is overstated — that textual sources are sufficient and rationalist extension represents innovation (bid'ah) rather than legitimate jurisprudential development. Comparative legal historians and Islamic studies scholars outside the confessional frame document the institutional history: the founding problem was real in early Islamic jurisprudence and drove methodological innovation, but remains contested whether rationalist extension is the correct solution or an over-reach.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanafi_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanafi_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanafi_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.58 to 0.68 over the interval, driven by increasing institutionalization of the Hanafi method in modern Islamic states and financial institutions. The method claims legitimacy as a faithful tool for extending divine intent, but the measurable effect is elevated authority for rationalist jurists and subordination of textual literalism. Suppression is moderate (0.35-0.42) because the constraint does not rely primarily on coercive exclusion — textualists can operate in parallel institutional spaces (Hanbali courts, literalist networks). Rather, suppression operates through epistemic framing: textualist positions are classified as insufficiently sophisticated, not as legitimate alternatives. The textualist interpretation is accessible and unambiguous, but the Hanafi method's institutional validation makes it the default framework for serious jurisprudential work. Theater is low-moderate (0.15-0.23) because the coordination function (reasoning about novel cases) is real and necessary; the performative element is moderate because some juristic preference (istihsan) operates more as post-hoc rationalization than genuine analogical work. The measurement series shows suppression and theater stabilizing by t=14, suggesting institutional consolidation: the method is no longer under intense contestation at the institutional level, even though textualist resistance remains at ~0.71.
 *
 * PERSPECTIVAL GAP:
 *   From the Hanafi institutional perspective, the method is genuine coordination: a principled way to extend divine law to novel cases while remaining faithful to revelation. From the textualist perspective, the same method is extractive: it displaces textual literalism from authority and forces believers to accept rationalist mediation of divine law. From the institutional governance perspective, the method is coordinative necessity: it allows Islamic law to remain relevant to modern governance. From the perspective of lay believers committed to textual authenticity, the method is excludable: their direct engagement with Qur'an and hadith is systematically devalued in favor of trained rationalist expertise. The engine should compute different types for different seats: beneficiary jurists and institutional apparatus should compute as beneficiaries under coordination (low d); textualist schools and literal adherents should compute as targets under extraction (high d). The coordinated benefit (novel case resolution) and the asymmetric extraction (displacement of textual authority) are structurally coupled in the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist jurists (organized, mobile exit) benefit directly from validation of their professional labor and are positioned as authoritative mediators; d approaches 0.2 (beneficiary). Hanafi institutional apparatus (institutional, trapped exit) is the agenda-setter and primary captor of authority; d approaches 0.15 (primary beneficiary). Textualist legal claims (moderate power, constrained exit) are systematically marginalized; d approaches 0.75 (target). Literal interpretation adherents (powerless, identity-locked exit) are the deepest targets — their identity as committed textual believers is reframed as obstacle to legitimate jurisprudence; d approaches 0.85 (severe target). Hanbali school (institutional, constrained exit) occupies an interesting middle: institutionally powerful regionally but epistemically subordinated globally; d approaches 0.65 (moderate-to-high target, unable to exit despite institutional resources because the global legitimacy landscape has shifted). Novel case stakeholders (powerless, trapped exit) nominally benefit from legal rulings on modern questions, but benefit is asymmetric and dependent; d approaches 0.4 (weak beneficiary, functionally dependent). Islamic institutional governance (institutional, trapped exit) is a secondary beneficiary — depends on the method's flexibility for legal innovation; d approaches 0.3 (beneficiary but shares agenda-setter role with Hanafi apparatus). The divergence in d-values should produce seat-specific type classifications: beneficiary seats compute as contributing to rope/coordination; target seats compute as extractive/snare dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not exhibit classic mandatrophy (founding problem dead, arrangement persisting). The founding problem — addressing novel legal cases within an Islamic framework — remains live and contested. The mandatrophy risk lies in a different vector: the Hanafi method's claim that reason is a faithful servant of divine intent becomes progressively less defensible as rationalist jurisprudence increasingly departs from textual literalism (as reflected in the rising extractiveness trajectory 0.58→0.68). At some point, the rationalization (reason faithfully serves divine intent) becomes so decoupled from practice (reason generates rulings that texts would not support) that the founding justification collapses. The mismatch consumer should flag: if the founding_problem_status is live but the theater_ratio is rising, the justification machinery is under strain. The constraint remains classified as tangled_rope (genuine coordination + asymmetric extraction), not as a degraded piton, because the coordination function is still materially necessary and the institutional force defending the method is active, not theatrical maintenance. However, the rising extraction without corresponding rise in coordination benefit (theater rising faster than suppression declining) suggests future mandatrophy risk if the textualist counter-argument (rationalist extension is bid'ah, not legitimate derivation) gains institutional traction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reason_as_faithful_tool_vs_innovation,
    'Is rationalist jurisprudence (qiyas, istihsan) a faithful extension of divine intent into novel cases, or is it bid''ah (innovation) that corrupts the textual kernel?',
    'Textual-historical analysis: do the Qur''an and hadith themselves sanction analogical reasoning as method, or is reason''s application fundamentally post-textual? Institutional-historical analysis: what was the actual historical relationship between early Islamic jurisprudence and the textual sources? Did rationalist jurisprudence emerge from exegetical necessity or from jurists'' interpretive preference?',
    'If rationalist extension is authentic derivation from the kernel, the Hanafi reading legitimates reason as divine tool and the constraint remains tangled_rope (coordination + extraction of authority). If rationalist extension is bid''ah, the Hanbali position is correct and the Hanafi method represents institutional capture of divine law by jurists; the constraint would shift to snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reason_as_faithful_tool_vs_innovation, conceptual, 'Whether rationalist jurisprudence is a legitimate extension of divine law or corruption of the textual kernel.').

omega_variable(
    textualism_as_epistemically_inadequate,
    'Is textual literalism epistemically unsophisticated (as the Hanafi frame asserts), or is it a coherent jurisprudential position that the Hanafi method has systematically marginalized?',
    'Comparative jurisprudential analysis: what can literalist jurisprudence accomplish in addressing novel cases? Can textual sources be interpreted creatively within a literalist framework? Institutional-historical analysis: did literalism fail to gain institutional dominance due to genuine inadequacy, or due to Ottoman and post-colonial institutional preferences for rationalist flexibility?',
    'If literalism is genuinely inadequate, the Hanafi suppression of textual authority reflects epistemic reality and the constraint is legitimate coordination. If literalism is a coherent alternative that has been marginalized, the suppression mechanism (framing textual positions as unsophisticated) is a primary extraction tool and the constraint is better classified as snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualism_as_epistemically_inadequate, empirical, 'Whether textual literalism is epistemically inadequate or a systematically marginalized alternative.').

omega_variable(
    identity_locked_suppression_mechanism,
    'Is the suppression of textual interpretation internalized (believers have fused their identity with literalism and cannot exit without existential crisis) or structural (institutional barriers prevent literalist jurisprudence from gaining resources and authority)?',
    'Post-exit trajectory: when believers or jurists adopt the Hanafi method and abandon textual literalism, does suppression persist (internalized identity pattern) or dissolve (structural barrier removed)? Institutional analysis: what resources, publications, teaching positions, and jurisprudential platforms are available to literalist scholars, relative to Hanafi rationalists?',
    'If suppression is internalized, the constraint''s effective extraction is higher than the 0.41 measure suggests — the target carries the suppression after institutional exit. If suppression is primarily structural, the 0.41 measure captures the active institutional force, and exit is genuinely possible if institutional barriers were removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_suppression_mechanism, empirical, 'Whether suppression of textual authority is internalized or structural.').

omega_variable(
    novel_case_dependency_asymmetry,
    'Do novel case stakeholders genuinely benefit from rationalist jurisprudence, or is their dependency itself a form of extraction — they are forced to accept reasoned rulings on questions they might prefer textual law did not address?',
    'Counterfactual analysis: if Hanbali textualism prevailed and rationalist extension were unavailable, would novel case stakeholders be better or worse off? Would they remain unruled (textualist constraint) or would alternative jurisprudential systems emerge? Stakeholder testimony: do those receiving rulings on novel cases prefer the Hanafi method or contest it?',
    'If novel case stakeholders genuinely benefit, the Hanafi method is coordinative and the constraint remains tangled_rope. If the stakeholders are co-opted through dependency (accepting rationalist rulings because textual sources are unavailable), extraction is higher and the constraint approaches pure snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(novel_case_dependency_asymmetry, preference, 'Whether novel case stakeholders genuinely benefit from rationalist jurisprudence or are extracted through dependency.').

omega_variable(
    institutional_consolidation_vs_contested_kernel,
    'Is the Hanafi method institutionally so dominant that the jurisprudential contest is effectively resolved in practice, or does the contest remain live and capable of reshaping institutional arrangements?',
    'Institutional-political analysis: can Hanbali or literalist jurisprudence regain institutional prominence if political conditions shift (e.g., literalist movements gain state power, institutional governance realigns)? Trajectory analysis: is the rising extractiveness (0.58→0.68) sustainable, or does increasing institutional tension suggest future instability?',
    'If consolidation is near-total, the constraint is stable and theater_ratio will remain moderate. If the contest is live, theater_ratio may rise as institutional forces invest in maintaining rationalist legitimacy against textualist counter-pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_consolidation_vs_contested_kernel, empirical, 'Whether the jurisprudential contest is institutionally settled or remains live.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanafi_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(juri_tr_t0, observed).
narrative_ontology:measurement(juri_tr_t2, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 2, 0.17).
narrative_ontology:measurement_basis(juri_tr_t2, observed).
narrative_ontology:measurement(juri_tr_t4, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 4, 0.19).
narrative_ontology:measurement_basis(juri_tr_t4, observed).
narrative_ontology:measurement(juri_tr_t7, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 7, 0.21).
narrative_ontology:measurement_basis(juri_tr_t7, observed).
narrative_ontology:measurement(juri_tr_t10, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement_basis(juri_tr_t10, observed).
narrative_ontology:measurement(juri_tr_t14, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 14, 0.22).
narrative_ontology:measurement_basis(juri_tr_t14, observed).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(juri_be_t0, observed).
narrative_ontology:measurement(juri_be_t2, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 2, 0.61).
narrative_ontology:measurement_basis(juri_be_t2, observed).
narrative_ontology:measurement(juri_be_t4, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 4, 0.64).
narrative_ontology:measurement_basis(juri_be_t4, observed).
narrative_ontology:measurement(juri_be_t7, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 7, 0.67).
narrative_ontology:measurement_basis(juri_be_t7, observed).
narrative_ontology:measurement(juri_be_t10, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 10, 0.69).
narrative_ontology:measurement_basis(juri_be_t10, observed).
narrative_ontology:measurement(juri_be_t14, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 14, 0.68).
narrative_ontology:measurement_basis(juri_be_t14, observed).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(juri_su_t0, observed).
narrative_ontology:measurement(juri_su_t2, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 2, 0.37).
narrative_ontology:measurement_basis(juri_su_t2, observed).
narrative_ontology:measurement(juri_su_t4, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 4, 0.39).
narrative_ontology:measurement_basis(juri_su_t4, observed).
narrative_ontology:measurement(juri_su_t7, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 7, 0.41).
narrative_ontology:measurement_basis(juri_su_t7, observed).
narrative_ontology:measurement(juri_su_t10, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(juri_su_t10, observed).
narrative_ontology:measurement(juri_su_t14, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 14, 0.41).
narrative_ontology:measurement_basis(juri_su_t14, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanafi_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__hanafi_reading, 0.12).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__hanbali_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__shafii_reading).

% DUAL FORMULATION NOTE:
% The jurisprudential_method_kernel decomposes into four constraint stories, one per reading: Hanafi (rationalist extension), Hanbali (literal textualism), Maliki (living tradition), and Shafi'i (methodological hierarchy). Each story has a distinct ε, beneficiary/victim structure, and institutional dynamics. The ε-invariance principle requires separate stories because measuring the constraint through a Hanafi lens (high ε for rationalist extension, coordination + extraction) versus a Hanbali lens (ε for textualism constraint, bid'ah resistance) yields structurally different constraints. The four stories form a kernel family linked by network.affects_constraints. Each reading's institutional dominance shapes the others: Hanafi institutional dominance marginalizes the Hanbali position; Hanbali counter-movements (reform movements, literalist revival) create institutional pressure on Hanafi dominance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__hanafi_reading, powerless, 0.85).
constraint_indexing:directionality_override(jurisprudential_method_kernel__hanafi_reading, moderate, 0.75).
constraint_indexing:directionality_override(jurisprudential_method_kernel__hanafi_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
