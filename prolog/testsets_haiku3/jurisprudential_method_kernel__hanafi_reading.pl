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
 *   human_readable: Hanafi Jurisprudential Method: Qiyas and Istihsan as Law-Making
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   The Hanafi school of Islamic jurisprudence grounds legal derivation in
 *   Qur'an and Hadith filtered through qiyas (analogical reasoning) and
 *   istihsan (juristic preference). This reading treats reason as a
 *   legitimate tool for extending divine intent to novel cases. The
 *   constraint emerges at the intersection of exegetical authority and
 *   institutional power: the Hanafi method requires recognizing jurists
 *   trained in rationalist logic as co-arbiters of law alongside literal text
 *   transmitters. This is CLAIMED as tangled_rope (genuine coordination
 *   problem: novel cases need answers; juristic reasoning solves it) while
 *   authored metrics show substantial extraction (high suppression of
 *   textualist alternatives, moderating resistance from excluded literalist
 *   movements). The claim/metric divergence is the analytical object — the
 *   engine measures whether the coordination story is the whole account or
 *   whether extraction has become dominant.
 *
 * KEY AGENTS:
 *   - Hanafi jurists: institutional agenda-setters maintaining the methodological apparatus; identity-locked to the tradition
 *   - Rationalist legal scholars: institutional beneficiaries whose intellectual project is vindicated by treating reason as legitimate
 *   - Textualist Islamic movements: organized payers bearing suppression of their methodological claim
 *   - Literal hadith claimants: moderate-power payers marginalized within the scholarly hierarchy
 *   - Islamic court systems: institutional agenda-setters and beneficiaries deriving flexibility from the method
 *   - Novel-case plaintiffs: powerless beneficiaries dependent on the jurist's interpretive capacity; trapped exit
 *   - Excluded alternative readings: moderate-power excluded stakeholders (Maliki, Shafi'i, Hanbali schools) kept peripheral
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, 0.68).
domain_priors:suppression_score(jurisprudential_method_kernel__hanafi_reading, 0.45).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanafi_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanafi_reading, "Hanafi Jurisprudential Method: Qiyas and Istihsan as Law-Making").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanafi_reading, "religious/legal/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanafi_reading, '25167ef9-d63b-40f6-901b-66b3a5fd319c').
narrative_ontology:cs_kernel_codification('25167ef9-d63b-40f6-901b-66b3a5fd319c', fixed_text).
narrative_ontology:cs_authority_grounding('25167ef9-d63b-40f6-901b-66b3a5fd319c', lineage).
narrative_ontology:cs_interpretation_layer_present('25167ef9-d63b-40f6-901b-66b3a5fd319c').
narrative_ontology:cs_reading_relation('25167ef9-d63b-40f6-901b-66b3a5fd319c', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_reading_relation('25167ef9-d63b-40f6-901b-66b3a5fd319c', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('25167ef9-d63b-40f6-901b-66b3a5fd319c', jurisprudential_method_kernel__shafii_reading, influences).
narrative_ontology:cs_axiom('25167ef9-d63b-40f6-901b-66b3a5fd319c', foundational, reason_as_derivation_tool).
narrative_ontology:cs_axiom_status(reason_as_derivation_tool, holdable).
narrative_ontology:cs_axiom_grounding('25167ef9-d63b-40f6-901b-66b3a5fd319c', reason_as_derivation_tool, deontological).
narrative_ontology:cs_axiom('25167ef9-d63b-40f6-901b-66b3a5fd319c', foundational, juristic_preference_legitimate).
narrative_ontology:cs_axiom_status(juristic_preference_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('25167ef9-d63b-40f6-901b-66b3a5fd319c', juristic_preference_legitimate, instrumental).
narrative_ontology:cs_reference_frame('25167ef9-d63b-40f6-901b-66b3a5fd319c', quranic_hadith_base_extended_by_reason).
narrative_ontology:cs_drift_state('25167ef9-d63b-40f6-901b-66b3a5fd319c', contemporary_institutional_dominance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('25167ef9-d63b-40f6-901b-66b3a5fd319c', '2026-06-12T14:23:47Z').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, hanafi_jurists).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, rationalist_legal_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, textualist_islamic_movements).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, literal_hadith_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, islamic_court_systems).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, novel_case_plaintiffs).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, novel_case_plaintiffs).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, reason_extends_divine_intent).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, juristic_preference_legitimacy).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, analogical_extension_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hanafi legal school jurists develop and transmit the methodological apparatus: qiyas (analogical reasoning) and istihsan (juristic preference). They maintain the scholastic infrastructure of precedent, commentary chains, and interpretive authority. Their institutional position depends on reason being recognized as a legitimate tool for extending divine intent to novel cases. Leaving the tradition would require abandoning professional identity, scholarly inheritance, and the entire apparatus of transmission they maintain.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hanafi_jurists, agenda_setter,
    institutional, civilizational, identity_locked, continental).

% Philosophers, logicians, and legal theorists who endorse reason as a tool for interpreting sacred text benefit from the Hanafi methodological framework. Their intellectual project — demonstrating that revelation and reason are harmonious — is vindicated by treating qiyas and istihsan as legitimate sources. They would face professional and reputational pressure if textualist readings gained exclusive authority over method.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, rationalist_legal_scholars, beneficiary,
    institutional, generational, constrained, global).

% Salafi, Wahhabi, and other literalist movements contend that qiyas and istihsan corrupt the kernel by interposing human reason between text and application. They advocate exclusive reliance on explicit Qur'anic and Hadith sources. The Hanafi methodological dominance suppresses their claim to methodological validity; they must marshal countervailing authority or splinter into parallel institutional structures to escape the constraint.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, textualist_islamic_movements, payer,
    organized, generational, mobile, global).

% Hadith scholars and transmitters who believe the tradition's claim to authority depends on remaining literalist (no interpolation through juristic preference) bear costs under the Hanafi framework: their textual authenticity is positioned as incomplete, requiring supplementation through reason. Advancing within the scholarly hierarchy requires adopting rationalist methods; remaining textualist marginalizes their contribution.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, literal_hadith_claimants, payer,
    moderate, biographical, constrained, national).

% Courts in Hanafi-dominant jurisdictions (Ottoman successor states, South Asia, Central Asia) institutionalize the Hanafi method as the binding legal framework. They benefit from the method's flexibility — qiyas and istihsan allow courts to adapt precedent to novel cases without formal amendment — but depend on jurists maintaining the scholastic apparatus that legitimates their decisions.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, islamic_court_systems, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, islamic_court_systems, beneficiary).

% Litigants in cases where no explicit Qur'anic or Hadith precedent exists benefit from the Hanafi method's capacity to extend law through qiyas (finding a similar case and reasoning by analogy). But they depend entirely on the jurist's interpretive authority; they cannot challenge the method itself and bear the risk of idiosyncratic istihsan (juristic preference) applied without transparency.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, novel_case_plaintiffs, beneficiary,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, novel_case_plaintiffs, payer).

% Maliki, Shafi'i, and Hanbali schools offer alternative methodologies but remain peripheral in jurisdictions where Hanafi reasoning became institutionalized. They would advance different claims about the kernel if admitted to the adjudicatory space (e.g., Maliki reliance on Medinan practice, Hanbali literalism); their exclusion is maintained by the Hanafi school's institutional entrenchment.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, excluded_alternative_readings, excluded,
    moderate, generational, trapped, regional).

% The broader Islamic scholarly tradition examines Hanafi methodological claims, contests their scope, produces counterevidence (Hanbali taqlid, Salafi usul), and sometimes synthesizes insights from multiple schools. From this perspective the constraint is one competing framework among others, not universal law.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, islamic_scholarly_community, observer,
    organized, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanafi_reading, hanafi_jurists).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a systematic method for extending divine law to novel cases not explicitly addressed in Qur'an and Hadith, using analogical reasoning and juristic discretion as tools for discovering the underlying divine intent. Solves the practical problem of applying a 7th-century legal corpus to changing historical circumstances without formal amendment or contradiction of sacred text.
% TRANSFER_FUNCTION: Moves methodological authority from literalist hadith transmitters (exclusive authenticity) to rationalist jurists trained in qiyas and istihsan (extended interpretation). Literal textualists lose standing as sole arbiters; their textual mastery is reframed as incomplete without juristic reasoning. Courts and institutional systems gain flexibility to render judgments on novel cases, trading legal predictability for adaptability.
% ABSENT_VOICES: Textualist and literalist movements object that the method corrupts the kernel by admitting human reason as an adjudicator; Hanbali jurists argue bid'ah; Salafi movements advocate exclusive textual adherence. They are structurally excluded from the institutional spaces (courts, law schools, scholarly hierarchies) where Hanafi method becomes binding. If present, they would demand methodological parity and contest the legitimacy of qiyas and istihsan.
% DISAPPEARANCE_RATIONALE: If the Hanafi methodological constraint vanished (courts ceased relying on qiyas and istihsan; the method lost institutional authority), Islamic jurisprudence would reorganize: either around explicit textualism (narrower case coverage, more frequent legal gaps requiring new legislation), or around alternative schools' methodologies (Maliki reliance on communal practice, Hanbali literalism, Shafi'i's hadith-priority hierarchy). The institutional capacity to render judgments on novel cases would shrink unless alternative methods filled the gap.
% FOUNDING_PROBLEM: Early Islamic jurisprudence faced the problem of applying revealed law to cases the Prophet and Companions did not directly address. How can jurists extend the law to novel circumstances without abandoning its divine grounding? The Hanafi response: use reason (qiyas, istihsan) as a tool to discover the divine intent embedded in the explicit sources, extending the law by finding analogies and preferring outcomes that serve the law's underlying purposes.
% FOUNDING_PROBLEM_CORROBORATION: Sunni Islamic jurisprudence universally affirms that law must address novel cases — the problem persists in every historical era and every jurisdiction. The Hanafi approach is corroborated by institutional practice: courts in Hanafi-dominant regions have consistently used qiyas and istihsan to render judgments, and the method has persisted across centuries and geographies (Ottoman law, Mughal courts, contemporary Islamic law in multiple nations). Textualist movements contest the Hanafi solution, arguing the founding problem should be solved through literal textual adherence or formal legislative amendment, not juristic reasoning — this contest is documented in historical disputes (Hanbalite critiques, modern Salafi arguments) and represents an alternative but challenged reading.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanafi_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is 0.68 at interval end because the method privileges rationalist training and juristic discretion, creating a gate through which one must pass to hold legal authority. Hadith scholars without logic training are structurally disadvantaged. Suppression is 0.45 (moderate) because textualist movements retain scholarly presence and can argue their case — the suppression is institutional (courts favor Hanafi reasoning) rather than violent. Theater is 0.28 (moderate-low) because the method is genuinely used to render judgments on novel cases, not merely performed; but a growing share of methodological activity in institutional contexts serves to entrench the Hanafi hierarchy against textualist challenge, not to discover novel law. Accessibility collapse is 0.62: once one understands that reason extends divine intent, the alternative (pure literalism without analogical extension) becomes unworkable for governing societies with changing circumstances. Resistance is 0.72 because literalist and Hanbali movements continue to produce countervailing scholarship and maintain scholarly presence, preventing the method from achieving monopolistic dominance. The measurement series tracks the long interval (0–1400 in Islamic calendar years, roughly 0–1050 CE to present day): extractiveness rises as the method becomes institutionalized in courts and schools, plateaus as alternative schools entrench in parallel networks, and remains stable as the Hanafi-textualist balance becomes quasi-equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   From the Hanafi jurist and court seats, this constraint solves a real coordination problem (novel cases need legal resolution) and represents genuine jurisprudential development. From the textualist payer seats, the constraint is an extraction mechanism using institutional entrenchment and rational authority to suppress literalist alternatives. The engine computes per-seat classification from the structural data: an institutional agenda-setter controlling the method will classify the constraint differently than an organized movement excluded from the institutional space. This perspectival divergence is not an error — it is the core analytical signal the framework captures.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanafi jurists: d near 0.0 (full beneficiary). They set the methodological agenda, train successors, and benefit directly from reason being recognized as legitimate. Their time horizon is civilizational; they are identity-locked to the tradition (leaving means abandoning professional identity and scholarly inheritance spanning generations). Rationalist scholars: d near 0.15 (beneficiary with constrained exit). Their intellectual project is vindicated, but they depend on institutional acceptance and would face reputational pressure if textualism gained exclusive authority. Textualist movements: d near 0.85 (near-full target). They bear suppression of their methodological claim and are organizationally excluded from courts; their exit is mobile (they can splinter into parallel institutions) but costly in terms of institutional influence. Literal hadith claimants: d near 0.75 (high target). Structurally disadvantaged within the scholarly hierarchy; their textual mastery is reframed as incomplete. Novel-case plaintiffs: d near 0.5 (symmetric, leaning beneficiary). They benefit from having law extended to their case (genuine coordination good) but bear the cost of uncertainty (the jurist's interpretive discretion could go against them) and cannot exit the judicial system. Courts: d near 0.2 (beneficiary). They derive flexibility and institutional power from the method.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (extending law to novel cases) is live and widely affirmed across Islamic jurisprudence. The Hanafi solution to this problem is real: qiyas and istihsan do extend the explicit sources to novel circumstances. However, mandatrophy emerges when extraction becomes decoupled from the coordination function. In contemporary practice, courts often invoke qiyas and istihsan with minimal transparent reasoning, treating them as cover for discretionary judgment rather than tools for discovering underlying intent. The method's flexibility becomes theater when the primary function is entrenchment of the institutional hierarchy rather than legal extension. Mandatrophy is not yet complete — the method is still genuinely used for novel cases — but the measurement series shows increasing theater_ratio (0.12 → 0.28) as institutional consolidation progresses, suggesting the function is partially atrophying into performance. A tangled_rope reading admits this: the constraint has both a real coordination function and asymmetric extraction; classifying it as snare (pure extraction with cover story) would overstate the suppression and understate the genuine legal work; classifying as rope would understate the suppression of textualist alternatives and the extraction of authority from literal scholarship. The tangled_rope claim holds if the coordination and extraction are structurally yoked — if decoupling them would require abandoning the legal capacity to handle novel cases entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reason_legitimacy_kernel_ambiguity,
    'Is the Hanafi method''s core claim — that reason is a legitimate tool for extending divine intent — an immanent development from the Qur''an and Hadith, or an interpolation of Hellenistic logic into Islamic jurisprudence?',
    'Historical analysis of early jurists'' explicit reasoning and Qur''anic/Hadith textual grounding for qiyas; comparison with parallel developments in Jewish law (Talmudic reasoning), Christian theology (natural law), and Greek philosophy to establish influence pathways.',
    'If immanent development: the method is extracted from the kernel itself and the constraint is coordination (legitimate extension of law). If interpolation: the method is an imposed framework and the constraint becomes extraction (reason masquerading as fidelity to text). Classification shifts from tangled_rope (acknowledged asymmetry) toward snare (extraction with falsified origin).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reason_legitimacy_kernel_ambiguity, empirical, 'Whether reason''s role is intrinsic to the kernel or an external import.').

omega_variable(
    istihsan_discretion_containment,
    'Can istihsan (juristic preference) be contained within principled boundaries, or does institutional practice allow jurists to apply it as an unchecked override of qiyas reasoning?',
    'Comparative analysis of institutional jurisprudence: do courts render transparent reasons for istihsan invocation, or treat it as a discretionary valve? Do scholars police excessive istihsan within the tradition, or do institutional power hierarchies prevent public critique?',
    'If contained: istihsan is a genuine tool for discovering underlying intent (coordination function held). If discretionary: istihsan is the mechanism through which extraction occurs — the method legitimates what is actually personal authority (extraction becomes dominant, theater rises further).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(istihsan_discretion_containment, empirical, 'Whether istihsan operates as principled preference or as unchecked discretion.').

omega_variable(
    textualist_suppression_mechanism,
    'Is the measured suppression of textualist alternatives (0.45) structural (institutional barriers to their advancement) or internalized (textualists have adopted rationalist frameworks and cease fully advocating their position)?',
    'Examine post-exit suppression trajectory: if textualist movements that gain institutional resources (state funding, court access) immediately reassert pure literalist methods without facing internal cognitive capture, suppression is structural. If they gradually adopt rationalist frameworks even after gaining resources, suppression contains internalized components.',
    'If purely structural: the constraint''s suppression is the enforced gate (institutional discrimination). If internalized: the gate has colonized the very cognitive framework of excluded movements, raising the effective suppression above the measured level and suggesting capture mechanisms more severe than institutional exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualist_suppression_mechanism, empirical, 'Whether suppression of literalist alternatives is structural or internalized.').

omega_variable(
    novel_case_dependency_asymmetry,
    'How many contemporary legal cases in Hanafi-dominant jurisdictions actually require novel jurisprudential reasoning via qiyas or istihsan, versus how many could be resolved through literal text, established precedent, or formal legislation?',
    'Quantitative analysis of court dockets in Hanafi-dominant jurisdictions: categorize cases by method of resolution (literal text, precedent, new reasoning, legislative analogue). Measure the ratio of genuinely novel cases to total docket.',
    'If the ratio is high (coordination function is substantive): the constraint solves a real problem for plaintiffs; maintaining the method is justified by coordination. If the ratio is low (most cases are routine or explicitly covered): the novel-case benefit is exaggerated; the method persists primarily for institutional flexibility and extraction, supporting reclassification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(novel_case_dependency_asymmetry, empirical, 'What proportion of institutional legal work actually requires novel jurisprudential reasoning.').

omega_variable(
    reading_alternative_structure,
    'What fundamentally distinguishes the Hanafi reading from its Hanbali, Maliki, and Shafi''i siblings — is it the legitimacy of reason as a tool, or is it something deeper in how the kernel itself is framed?',
    'Comparative textual and institutional analysis: map the explicit premises each school claims about derivation sources, the role of human judgment, and the relationship between text and interpretation. Identify whether the readings diverge on what reason is allowed to do, or on what counts as text in the first place.',
    'If the divergence is on reason''s role: the readings are methodological variants of a shared kernel (coordinate to coexists_with). If the divergence is on what the kernel contains: the readings may actually be reading different kernels, and network relationships should be revised (separate constraints, linked by inferred coupling rather than shared kernel).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_alternative_structure, conceptual, 'Whether the Hanafi reading differs from siblings on method or on kernel definition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanafi_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(juri_tr_t0, observed).
narrative_ontology:measurement(juri_tr_t200, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 200, 0.16).
narrative_ontology:measurement_basis(juri_tr_t200, observed).
narrative_ontology:measurement(juri_tr_t600, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 600, 0.24).
narrative_ontology:measurement_basis(juri_tr_t600, observed).
narrative_ontology:measurement(juri_tr_t1000, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1000, 0.29).
narrative_ontology:measurement_basis(juri_tr_t1000, observed).
narrative_ontology:measurement(juri_tr_t1200, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1200, 0.28).
narrative_ontology:measurement_basis(juri_tr_t1200, observed).
narrative_ontology:measurement(juri_tr_t1400, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1400, 0.28).
narrative_ontology:measurement_basis(juri_tr_t1400, observed).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(juri_be_t0, observed).
narrative_ontology:measurement(juri_be_t200, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 200, 0.52).
narrative_ontology:measurement_basis(juri_be_t200, observed).
narrative_ontology:measurement(juri_be_t600, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 600, 0.65).
narrative_ontology:measurement_basis(juri_be_t600, observed).
narrative_ontology:measurement(juri_be_t1000, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1000, 0.68).
narrative_ontology:measurement_basis(juri_be_t1000, observed).
narrative_ontology:measurement(juri_be_t1200, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1200, 0.67).
narrative_ontology:measurement_basis(juri_be_t1200, observed).
narrative_ontology:measurement(juri_be_t1400, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1400, 0.68).
narrative_ontology:measurement_basis(juri_be_t1400, observed).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(juri_su_t0, observed).
narrative_ontology:measurement(juri_su_t200, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 200, 0.35).
narrative_ontology:measurement_basis(juri_su_t200, observed).
narrative_ontology:measurement(juri_su_t600, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 600, 0.48).
narrative_ontology:measurement_basis(juri_su_t600, observed).
narrative_ontology:measurement(juri_su_t1000, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1000, 0.46).
narrative_ontology:measurement_basis(juri_su_t1000, observed).
narrative_ontology:measurement(juri_su_t1200, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1200, 0.44).
narrative_ontology:measurement_basis(juri_su_t1200, observed).
narrative_ontology:measurement(juri_su_t1400, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1400, 0.45).
narrative_ontology:measurement_basis(juri_su_t1400, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanafi_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__hanafi_reading, 0.12).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__hanbali_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__shafii_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jurisprudential_method_kernel, shared with Hanbali, Maliki, and Shafi'i readings. Each reading instantiates a different constraint by treating the kernel (what law derives from, what counts as valid derivation) differently. The Hanafi reading privileges reason and juristic discretion; the Hanbali reading rejects them as bid'ah; the Maliki reading centers Medinan practice; the Shafi'i reading establishes methodological hierarchy. They coexist as live positions in Islamic jurisprudence, held by different schools and jurisdictions. The Hanafi reading is linked to all siblings via network.affects_constraints because institutional dominance of the Hanafi method creates structural pressure on how alternative readings can operate: literalist movements must work within the Hanafi dominance to maintain credibility, Maliki and Shafi'i schools must justify their alternatives relative to Hanafi reasoning, etc. Upstream from all siblings: the underlying kernel formulation (what counts as derivation source) is contested by all four readings simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__hanafi_reading, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
