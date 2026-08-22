% ============================================================================
% CONSTRAINT STORY: notability_guidelines__inclusionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__inclusionist_reading, []).

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
 *   constraint_id: notability_guidelines__inclusionist_reading
 *   human_readable: Wikipedia Notability Guidelines as Exclusionary Gatekeeping (Inclusionist Reading)
 *   domain: digital_commons_governance/epistemic_justice/platform_constitutionalism
 *
 * SUMMARY:
 *   Wikipedia's Notability Guidelines (WP:N) establish criteria for
 *   determining which subjects merit encyclopedia articles. Officially, these
 *   criteria are neutral: a subject is notable if it has been covered
 *   substantially in independent, reliable sources. The inclusionist reading
 *   interprets WP:N as a structural gatekeeping apparatus that systematically
 *   excludes marginalized knowledge by embedding Western institutional
 *   epistemology as the unquestioned baseline. Under this reading,
 *   'reliability' is not an objective epistemic property but a euphemism for
 *   'institutional credentialing'; subjects whose knowledge exists in
 *   non-English-language publishing, community documentation, or outside the
 *   Western academic system face systematic deletion not because their
 *   sources are low-quality but because the gate is calibrated to exclude
 *   them. The snare operates through the appearance of neutral quality
 *   standards; the extraction is epistemic: whose knowledge gets to exist in
 *   the global commons.
 *
 * KEY AGENTS:
 *   - institutional_knowledge_producers (universities, academic publishers, Western think tanks) — benefit from WP:N's validation of institutional sources; set enforcement standards
 *   - marginalized_communities (Global South scholars, indigenous researchers, women in niche fields, disability advocates) — bear the cost of systematic deletion; trapped in the constraint by WP:N's global scope and monolingual-in-effect gate
 *   - wikipedia_volunteer_editors (diverse backgrounds) — differentially positioned: Western editors benefit from informal networks of source legitimacy; editors from underrepresented regions face higher barriers and invisible labor costs
 *   - institutional_publishers (academic journals, university presses, major media) — benefit without administering; WP:N drives researchers toward publication pathways these institutions control
 *   - wikipedia_as_institution (Wikimedia Foundation, governance structures) — maintains WP:N as doctrine; preserves Western epistemology as neutral baseline
 *   - deletion_review_process (AfD mechanisms) — operates as enforcement apparatus; appears democratic but systematically privileges editors with cultural capital
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, 0.82).
domain_priors:suppression_score(notability_guidelines__inclusionist_reading, 0.79).
domain_priors:theater_ratio(notability_guidelines__inclusionist_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__inclusionist_reading, snare).
narrative_ontology:human_readable(notability_guidelines__inclusionist_reading, "Wikipedia Notability Guidelines as Exclusionary Gatekeeping (Inclusionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__inclusionist_reading, "digital_commons_governance/epistemic_justice/platform_constitutionalism").

domain_priors:requires_active_enforcement(notability_guidelines__inclusionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__inclusionist_reading, '8bf9735b-11d7-449c-9600-4df6e0b1052b').
narrative_ontology:cs_kernel_codification('8bf9735b-11d7-449c-9600-4df6e0b1052b', fixed_text).
narrative_ontology:cs_authority_grounding('8bf9735b-11d7-449c-9600-4df6e0b1052b', extraction).
narrative_ontology:cs_interpretation_layer_present('8bf9735b-11d7-449c-9600-4df6e0b1052b').
narrative_ontology:cs_reading_relation('8bf9735b-11d7-449c-9600-4df6e0b1052b', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8bf9735b-11d7-449c-9600-4df6e0b1052b', notability_guidelines__deliberative_reading, influences).
narrative_ontology:cs_axiom('8bf9735b-11d7-449c-9600-4df6e0b1052b', foundational, reliable_sources_embed_institutional_power).
narrative_ontology:cs_axiom_status(reliable_sources_embed_institutional_power, holdable).
narrative_ontology:cs_axiom_grounding('8bf9735b-11d7-449c-9600-4df6e0b1052b', reliable_sources_embed_institutional_power, deontological).
narrative_ontology:cs_axiom('8bf9735b-11d7-449c-9600-4df6e0b1052b', foundational, epistemological_diversity_requires_pluralized_standards).
narrative_ontology:cs_axiom_status(epistemological_diversity_requires_pluralized_standards, holdable).
narrative_ontology:cs_axiom_grounding('8bf9735b-11d7-449c-9600-4df6e0b1052b', epistemological_diversity_requires_pluralized_standards, deontological).
narrative_ontology:cs_reference_frame('8bf9735b-11d7-449c-9600-4df6e0b1052b', institutional_knowledge_hegemony).
narrative_ontology:cs_drift_state('8bf9735b-11d7-449c-9600-4df6e0b1052b', contemporary_equity_pressures, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8bf9735b-11d7-449c-9600-4df6e0b1052b', '').
narrative_ontology:cs_kernel_id(notability_guidelines__inclusionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, western_academic_establishment).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, english_language_publishing_industry).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, marginalized_communities).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, indigenous_knowledge_systems).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, global_south_scholars).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, women_in_niche_fields).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, disability_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, wikipedia_volunteer_editors).
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, institutional_publishers).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, wikipedia_volunteer_editors).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, knowledge_systems_outside_academy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Universities, academic publishers, major think tanks, and established research institutions benefit from notability standards that privilege 'peer-reviewed' sources originating from credentialed institutions in the Global North. They enforce WP:N through editing decisions, speedy deletion practices, and the AfD (Articles for Deletion) process. Their sources dominate 'reliable sources' lists; their credentialing structures define what counts as legitimate knowledge. They collect cultural authority and control over the commons' epistemic baseline.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, institutional_knowledge_producers, agenda_setter,
    institutional, generational, arbitrage, global).

% Indigenous peoples, Global South scholars, women in underfunded fields, disability advocates, and other groups whose knowledge production systems do not align with Western academic publishing infrastructure face systematic deletion when attempting to document their own histories, expertise, or contributions. Their knowledge sources—community documentation, oral tradition, independent publishing, activism-based research—are dismissed as non-'reliable' under WP:N. Exit is impossible: Wikipedia is often the only free encyclopedia accessible in their regions, yet their participation produces deletion outcomes that reinforce their erasure from global knowledge commons.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, marginalized_communities, payer,
    powerless, biographical, trapped, global).

% Volunteer editors, especially those from marginalized backgrounds who attempt inclusionist edits, bear the cost of labor-intensive AfD disputes and face social pressure/blocking from deletionist editors. Editors from Global North institutions enjoy easier time establishing 'notability' for their contributions and benefit from informal networks that guide source-legitimacy judgments. Editors from underrepresented regions face higher barriers to credibility and must perform invisible additional labor to justify inclusion.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikipedia_volunteer_editors, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__inclusionist_reading, wikipedia_volunteer_editors, beneficiary).

% Indigenous knowledge systems, oral traditions, community-generated documentation, and non-English publishing ecosystems are structurally locked into marginalized status because WP:N treats Western academic publishing as the canonical knowledge infrastructure. To be 'notable' means to be documented in outlets controlled by institutions these systems did not build and whose legitimacy they do not accept. Yet the constraint's scope (global, monolingual-in-effect) means that Wikipedia's gatekeeping becomes a de facto arbiter of whose knowledge gets to exist in the digital commons. Identity-locked: these systems cannot 'become acceptable sources' without surrendering their epistemic autonomy.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, knowledge_systems_outside_academy, payer,
    powerless, civilizational, identity_locked, global).

% Academic journals, university presses, and major publishing houses benefit from WP:N's validation of 'peer-reviewed' sources as the epistemic baseline. The constraint drives researchers and subjects toward publication pathways controlled by these institutions, increasing subscription revenue, citation capture, and institutional prestige. They benefit from WP:N without administering it; the constraint operates as an externally-enforced market protection for credentialed knowledge production.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, institutional_publishers, beneficiary,
    institutional, generational, arbitrage, global).

% The Wikimedia Foundation and Wikipedia's governance structures maintain WP:N as operational doctrine. They authorize the deletionist interpretation through policy maintenance, dispute resolution systems that bias toward 'reliable sources,' and resistance to structural amendments that would decentralize source legitimacy. The institution preserves notability standards that protect Wikipedia's appearance of epistemic neutrality while embedding Western institutional epistemology as the neutral baseline.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikipedia_as_institution, agenda_setter,
    institutional, generational, analytical, global).

% Publishing ecosystems in Africa, Asia, Latin America, and other regions would be legitimate knowledge sources if WP:N's reliable-source definition were restructured to recognize non-English-language peer review, open-access journals, and regional academic standards. These publishers exist and maintain quality control through their own traditions; they are structurally excluded not by absence but by a gate that defines them as 'not reliable' a priori.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, global_south_publishing, excluded,
    powerless, biographical, trapped, global).

% Wikipedia's Articles for Deletion process and dispute resolution mechanisms operate as the enforcement apparatus. Editors collectively vote on whether articles meet WP:N; the process appears democratic but systematically benefits editors with cultural capital who can cite 'reliable sources' and trusted institutional affiliations. The process is also the site where resistance emerges: discussions about whether standards are exclusionary, whether 'reliable sources' are truly objective, and whether WP:N perpetuates bias.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, deletion_review_process, observer,
    institutional, biographical, analytical, global).

% Scholars working on topics defined as inherently 'non-notable' (e.g., marginal religious traditions, disability history, activist genealogies, local environmental knowledge) face structural deletion regardless of source quality. Even rigorous self-published research or local journalism is dismissed because WP:N privileges outlets (academic journals, major newspapers) that do not cover these topics. Their exclusion is not due to poor scholarship but to the structure that determines which scholarship gets to exist in Wikipedia's knowledge commons.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, epistemically_excluded_scholars, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:fixing_cost_class(notability_guidelines__inclusionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Wikipedia seeks to maintain a coherent, publicly-editable encyclopedia by establishing criteria for notability: articles should document subjects covered substantially in independent, reliable sources. This solves a genuine coordination problem: without some boundary, editorial labor would disperse infinitely; vandalism and promotional content would proliferate.
% TRANSFER_FUNCTION: Transfers epistemic authority from diverse knowledge-production systems to Western institutional sources. Marginalized communities transfer their knowledge's claim to legitimacy toward institutions (academic publishers, major media, Western universities) that do not recognize their own knowledge-production systems. The transfer is enforced through the deletion mechanism: if your knowledge cannot cite institutional sources, your article is deleted.
% ABSENT_VOICES: Global South scholars, indigenous knowledge keepers, disability advocates, community historians, and researchers working on topics not covered by Western academic or major-media outlets are systematically absent from notability deliberations. They would contest the framing of 'reliability' as neutral and would demand pluralized standards recognizing their own epistemic traditions. Their absence is structural: they are rare Wikipedia editors, their languages are underrepresented, their sources are pre-judged as non-reliable, and their labor in defending inclusion is invisible and unvalued.
% DISAPPEARANCE_RATIONALE: If WP:N and its enforcement disappeared, Wikipedia would become a commons where marginalized communities could document their own knowledge without gatekeeping. New policies would emerge; their shape would determine whether the commons becomes more inclusive or chaotic. Knowledge systems currently excluded (indigenous tradition, Global South scholarship, disability expertise, activist research) would gain permanent housing. Institutional knowledge producers would lose their monopoly on the encyclopedia's epistemic baseline. Global knowledge infrastructure would shift from Western-institutional toward pluralized.
% FOUNDING_PROBLEM: Early Wikipedia faced degradation from vandalism, hoaxes, promotional content, and articles on non-notable subjects. Some boundary on inclusion appeared necessary to maintain editorial coherence. Notability standards emerged as the mechanism to distinguish encyclopedic subjects from personal interests and spam.
% FOUNDING_PROBLEM_CORROBORATION: Deletionists attest the problem is live. Inclusionists and external researchers (Lam et al., Moss, Eom et al.) attest the problem is substantially solved by technology and community review, and that WP:N now functions as gatekeeping rather than quality control. Academic research on Wikipedia's gender and geography bias, conducted by scholars outside Wikipedia's governance, corroborates systematic asymmetry: institutional subjects face lower deletion thresholds; marginalized subjects face higher thresholds despite comparable source quality.
narrative_ontology:disappearance_verdict(notability_guidelines__inclusionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__inclusionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__inclusionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(notability_guidelines__inclusionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__inclusionist_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__inclusionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(notability_guidelines__inclusionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end, rising over time) because marginalized communities are systematically excluded from the commons not by quality failure but by a gate calibrated to their exclusion. The constraint persists because institutional knowledge producers benefit from it without appearing to benefit—WP:N looks neutral while operating as gatekeeping. Suppression is high (0.79) because the constraint prevents alternatives: marginalized communities cannot opt out of Wikipedia's relevance (it is often the only free, global encyclopedia accessible to them), cannot change the gate from inside (their participation in deletion disputes is structurally devalued), and cannot appeal to an objective standard (the gate is presented as already objective). Theater is substantial (0.62) because WP:N's stated function (maintaining quality) masks its operating function (gatekeeping knowledge legitimacy). The apparent neutrality of the standard is the mechanism that sustains suppression—marginalized communities cannot easily contest a gate that presents itself as objective. The rising trajectory reflects increasing institutionalization: as WP:N becomes more codified and defended through policy architecture, the extraction deepens. Accessibility collapse (0.71) reflects the reality that once someone understands WP:N operates as a gate, they see the constraint everywhere: every deletion, every source-vetting process, every 'reliability' decision. Resistance (0.58) reflects that contestation exists—inclusionist editors, bias-reduction workshops, and academic attention to Wikipedia's reproducing inequities—but is structurally constrained by the power imbalance between volunteer editors and institutional knowledge producers.
 *
 * PERSPECTIVAL GAP:
 *   The institutional knowledge producer and the marginalized community perceive structurally different constraints. For the institutional producer, WP:N is a quality filter—a genuine solution to a real coordination problem (how to maintain an encyclopedia without infinite articles on non-subjects). From this seat, the constraint appears as rope: coordination enabling a shared knowledge commons. For the marginalized community, WP:N is an apparatus of exclusion—a mechanism by which their knowledge is pre-rejected. From this seat, the constraint is snare: their knowledge is trapped outside, the gate appears immovable because it is defended by actors who benefit from its current calibration, and resistance is costly. The engine computes these divergences from the structural data: institutional_knowledge_producers are agenda-setters with arbitrage exit (they can reshape standards, exit to their own institutions, or arbitrage between systems); marginalized_communities are payers with trapped exit (Wikipedia is their only option, they cannot unilaterally change the standard, and their alternative is erasure). Different power atoms, different exit options, different roles → different directionalities → different computed types. The story-level claim is snare; the deletionist reading would claim rope; the deliberative reading would claim tangled_rope. These are not observer-dependent phenomena—they are properties of the structural relationship between agent and constraint. The reading declares which structural relationship is the real one.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional knowledge producers: d ≈ 0.1-0.15 (near-beneficiary end). They collect from WP:N; their sources are pre-legitimized; the constraint operates to their advantage. Their exit is high (arbitrage: they can appeal Wikipedia to revise policy, can publish in their own institutional venues, can arbitrage between endorsing Wikipedia or ignoring it). Low d reflects that they are net beneficiaries with substantial optionality. Marginalized communities: d ≈ 0.85-0.95 (near-target end). They pay the cost of exclusion; their knowledge is pre-rejected; the constraint operates against them. Their exit is trapped (Wikipedia is often the only accessible global encyclopedia; their knowledge cannot exist elsewhere in the commons; their participation in deletion disputes is de-valued). High d reflects that they are targeted by extraction with minimal exit. Wikipedia as institution: d ≈ 0.25-0.35 (beneficiary-leaning, not agenda-setter). They benefit from WP:N by maintaining the appearance of neutral quality while embedding Western epistemology. Their exit is mobile (they could revise policy, could pluralize notability standards), but they have not done so, suggesting they benefit from the current arrangement. Volunteer editors from marginalized backgrounds: d ≈ 0.70-0.80 (target end, but intermediate). They bear labor costs in defending inclusionist edits, face social pressure, but also potentially benefit from the commons. Their exit is constrained (they can leave Wikipedia but lose opportunity to participate in the commons; their labor investments are not recognized). The overrides needed: none—the structural derivation from beneficiary/victim + exit captures the directionality accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   WP:N's founding problem (preventing vandalism and low-quality content) was genuinely live in Wikipedia's early years when the project was building. The problem is now substantially dead: Wikipedia's technology (edit tracking, vandalism reversal, community review) has evolved; quality control mechanisms exist at many layers; vandalism is a management problem, not a structural threat to the encyclopedia. Yet WP:N persists and has been strengthened over time through policy codification, speedy deletion practices, and resistance to pluralizing notability standards. The constraint exhibits mandatrophy: it has outlived its founding function and now operates primarily to extract epistemic authority from marginalized communities toward institutional producers. The divergence between disappearance_verdict (world_rearranges—marginalized knowledge could exist in the commons if WP:N were removed) and founding_problem_status (contested, but observable dead for institutional quality-control purposes) is the mandatrophy signature. The arrangement persists not because it solves the founding problem but because institutional knowledge producers benefit from it. Deletionist editors defend WP:N as quality protection; marginalized communities experience it as structural exclusion. This divergence—gatekeeping defended as quality control—is the mechanism of mandatrophy. A genuine commitment to the founding problem would require finding quality-control mechanisms that do not presuppose Western institutional epistemology as the baseline for legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reliable_sources_definition,
    'Is ''reliability'' a property of sources or a property of the institutional context that produces them? Can non-Western, non-English-language publishing systems be recognized as equally reliable without requiring convergence to Western academic standards?',
    'Empirical: comparative study of peer-review practices in Global South academic systems; documentation of quality-control mechanisms in non-English-language publishing. Normative: deliberation among editors from diverse epistemic traditions about what constitutes reliable evidence for different types of knowledge claims.',
    'If reliability is contextual and relative, WP:N could be restructured to pluralize notability standards rather than impose a single Western baseline—reducing extraction and reopening the gate. If reliability is objective, the current structure is defensible as neutral quality control.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reliable_sources_definition, conceptual, 'Whether ''reliability'' is an intrinsic property of sources or a property of the evaluating community''s epistemic framework.').

omega_variable(
    coordination_extraction_inseparability,
    'Are the coordination function (maintaining a coherent encyclopedia) and the extraction function (gatekeeping knowledge legitimacy) structurally inseparable? Could quality control exist without epistemological gatekeeping?',
    'Natural experiment: observe Wikipedia communities (e.g., regional Wikipedias with different governance) that implement alternative notability standards. Compare vandalism rates, editor retention, and content quality against communities using strict WP:N. If alternatives maintain quality, the functions are separable.',
    'If separable, the extraction is pure rent-seeking layered onto a real coordination function (tangled_rope). If inseparable, the extraction is the price of coordination (tipping toward rope). If evidence shows current standards do not even improve quality (e.g., non-notable subjects have lower vandalism rates than notable ones), the coordination story collapses and the constraint is pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_inseparability, empirical, 'Whether WP:N''s quality-control and knowledge-gatekeeping functions are structurally coupled.').

omega_variable(
    identity_lock_mechanism,
    'For knowledge systems that cannot conform to Western institutional publishing (indigenous oral tradition, community documentation, activist research), is the lock to marginalized status truly irreversible, or are there pathways to institutional legitimacy without surrendering epistemic autonomy?',
    'Documentary: track cases where non-Western knowledge systems have been successfully documented in Wikipedia without requiring institutional publication first. Investigate whether the constraint permits ''alternative pathways'' or whether all pathways converge to Western academic gating.',
    'If no pathways exist to legitimacy without surrendering epistemic autonomy, the identity-lock is near-complete and the constraint is a pure snare. If alternative pathways exist but are invisible or costly to discover, the constraint is snare with imperfect suppression and resistance is possible. If alternative pathways are accessible and honored, the extraction is lower than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether marginalized knowledge systems can achieve Wikipedia notability without institutional credentialing.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the inclusionist reading''s core premise (WP:N systematically excludes marginalized knowledge) logically foreclose the deletionist reading''s core premise (WP:N is a neutral quality filter), or do both remain defensible within different epistemic frameworks?',
    'Logical: analyze whether ''systematic exclusion'' and ''neutral quality filter'' can both be true (e.g., the gate could be calibrated to exclude unintentionally, or deliberately, or as a side effect of quality control, or all three). If both can be true, the readings coexist; if one necessitates the other is false, foreclosure holds.',
    'If foreclosure holds, one reading must be wrong. If coexistence holds, both readings reflect partial truths and the engine should compute per-seat type divergence. The reading_relations field stakes this interpretation; the resolution feeds back to whether this kernel exhibits genuine logical contradiction or productive friction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the inclusionist and deletionist readings are logically incompatible or coexistent interpretations.').

omega_variable(
    resistance_sustainability,
    'Can inclusionist resistance (bias-reduction workshops, policy reform campaigns, academic attention) persist and grow without converting into institutional power to revise standards? Or does resistance exhaust itself when it fails to capture the governance apparatus?',
    'Temporal: track inclusionist campaigns over 5+ years. Measure: policy changes adopted, articles restored, new notability pathways opened. If changes persist, resistance is sustainable; if resistance cycles back to defeat, it is performative.',
    'If resistance is performative, the constraint exhibits inertial stability (piton-like) despite visible contestation. If resistance accumulates power, the constraint is unstable and may transition toward tangled_rope (contested negotiation) or rope (reformed coordination). High theater (0.62) suggests some performativity; this omega tests whether it is exhausting or accumulating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_sustainability, empirical, 'Whether inclusionist resistance can sustain institutional change or is structurally contained.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__inclusionist_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__inclusionist_reading, theater_ratio, 0, 0.51).
narrative_ontology:measurement(nota_tr_t5, notability_guidelines__inclusionist_reading, theater_ratio, 5, 0.54).
narrative_ontology:measurement(nota_tr_t10, notability_guidelines__inclusionist_reading, theater_ratio, 10, 0.57).
narrative_ontology:measurement(nota_tr_t15, notability_guidelines__inclusionist_reading, theater_ratio, 15, 0.6).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__inclusionist_reading, theater_ratio, 20, 0.61).
narrative_ontology:measurement(nota_tr_t25, notability_guidelines__inclusionist_reading, theater_ratio, 25, 0.62).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__inclusionist_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(nota_be_t5, notability_guidelines__inclusionist_reading, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(nota_be_t10, notability_guidelines__inclusionist_reading, base_extractiveness, 10, 0.76).
narrative_ontology:measurement(nota_be_t15, notability_guidelines__inclusionist_reading, base_extractiveness, 15, 0.79).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__inclusionist_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement(nota_be_t25, notability_guidelines__inclusionist_reading, base_extractiveness, 25, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__inclusionist_reading, suppression_requirement, 0, 0.74).
narrative_ontology:measurement(nota_su_t5, notability_guidelines__inclusionist_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(nota_su_t10, notability_guidelines__inclusionist_reading, suppression_requirement, 10, 0.77).
narrative_ontology:measurement(nota_su_t15, notability_guidelines__inclusionist_reading, suppression_requirement, 15, 0.78).
narrative_ontology:measurement(nota_su_t20, notability_guidelines__inclusionist_reading, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(nota_su_t25, notability_guidelines__inclusionist_reading, suppression_requirement, 25, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__inclusionist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(notability_guidelines__inclusionist_reading, 0.05).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, notability_guidelines__deletionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, notability_guidelines__deliberative_reading).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, wikipedia_gender_bias_reproduction).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, english_language_hegemony_digital_commons).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel notability_guidelines. The deletionist_reading claims WP:N is a necessary quality filter (Mountain or Rope); this reading claims it is a gatekeeping Snare. The deliberative_reading claims it is a perpetually renegotiated tangled_rope. All three readings share the same policy text (the kernel) but interpret its operation and effects differently. They are not compatible descriptions of one constraint—they are three structurally distinct constraints produced by three incompatible epistemic frames. The network edges link them to enable contamination analysis: if the inclusionist reading's evidence for systematic exclusion (empirical omega_reliable_sources_definition, omega_coordination_extraction_inseparability) is confirmed, the deletionist reading's classification as Mountain/Rope becomes unsustainable, and the constraint's type must shift across all three readings' seats. Conversely, if the deletionist reading's evidence for quality-control necessity proves robust, the inclusionist reading's victim set may be reconsidered. The edges preserve the option to revise all three interpretations in light of shared empirical discovery.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
