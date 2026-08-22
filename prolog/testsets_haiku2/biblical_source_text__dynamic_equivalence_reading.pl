% ============================================================================
% CONSTRAINT STORY: biblical_source_text__dynamic_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__dynamic_equivalence_reading, []).

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
 *   constraint_id: biblical_source_text__dynamic_equivalence_reading
 *   human_readable: Dynamic Equivalence Translation Mandate (Communicative Effectiveness Primary)
 *   domain: religious/textual/epistemological
 *
 * SUMMARY:
 *   The dynamic equivalence reading of the biblical source text constraint
 *   treats communicative effectiveness in the target language as the primary
 *   and overriding goal of translation, subordinating structural fidelity to
 *   the source text. Translators working under this mandate prioritize
 *   intelligibility to lay readers and accessibility in pastoral and
 *   missionary contexts over word-for-word correspondence or preservation of
 *   source-language syntax and morphology. The constraint extracts precision
 *   from academic, scholarly, and word-study contexts (which lose transparent
 *   access to source-language structures) and transfers comprehensibility to
 *   lay, pastoral, and missionary contexts. This is a reading of a contested
 *   kernel—the biblical source text itself—where three distinct readings
 *   compete: formal equivalence (structure primary), dynamic equivalence
 *   (communication primary), and critical reconstruction (historical recovery
 *   primary). This story instantiates only the dynamic equivalence reading as
 *   a single, ε-invariant constraint.
 *
 * KEY AGENTS:
 *   - lay_readers: primary beneficiary (powerless, mobile exit, biographical horizon)
 *   - missionary_organizations: beneficiary and enforcer (organized, constrained exit, generational horizon)
 *   - pastoral_communities: agenda setter and beneficiary (institutional, constrained, generational)
 *   - academic_scholars: victim (powerful, constrained exit, biographical horizon)
 *   - word_study_practitioners: victim (moderate, identity-locked, biographical horizon)
 *   - textual_reconstructionists: victim (moderate, constrained, generational horizon)
 *   - translation_committees: agenda setter (institutional, constrained, generational)
 *   - formal_equivalence_advocates: excluded (powerful, trapped, biographical)
 *   - critical_scholars: excluded (powerful, trapped, biographical)
 *   - denominational_authorities: analytical/meta-setter (institutional, analytical, generational)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__dynamic_equivalence_reading, 0.68).
domain_priors:suppression_score(biblical_source_text__dynamic_equivalence_reading, 0.45).
domain_priors:theater_ratio(biblical_source_text__dynamic_equivalence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__dynamic_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__dynamic_equivalence_reading, "Dynamic Equivalence Translation Mandate (Communicative Effectiveness Primary)").
narrative_ontology:topic_domain(biblical_source_text__dynamic_equivalence_reading, "religious/textual/epistemological").

domain_priors:requires_active_enforcement(biblical_source_text__dynamic_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__dynamic_equivalence_reading, '280f30e3-bfb6-407c-a9ec-164ee15ac5de').
narrative_ontology:cs_kernel_codification('280f30e3-bfb6-407c-a9ec-164ee15ac5de', fixed_text).
narrative_ontology:cs_authority_grounding('280f30e3-bfb6-407c-a9ec-164ee15ac5de', lineage).
narrative_ontology:cs_interpretation_layer_present('280f30e3-bfb6-407c-a9ec-164ee15ac5de').
narrative_ontology:cs_reading_relation('280f30e3-bfb6-407c-a9ec-164ee15ac5de', biblical_source_text__formal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('280f30e3-bfb6-407c-a9ec-164ee15ac5de', biblical_source_text__critical_reconstructive_reading, influences).
narrative_ontology:cs_axiom('280f30e3-bfb6-407c-a9ec-164ee15ac5de', foundational, target_language_communicativeness_primary).
narrative_ontology:cs_axiom_status(target_language_communicativeness_primary, holdable).
narrative_ontology:cs_axiom_grounding('280f30e3-bfb6-407c-a9ec-164ee15ac5de', target_language_communicativeness_primary, instrumental).
narrative_ontology:cs_axiom('280f30e3-bfb6-407c-a9ec-164ee15ac5de', secondary, translator_bears_interpretation_burden).
narrative_ontology:cs_axiom_status(translator_bears_interpretation_burden, holdable).
narrative_ontology:cs_axiom_grounding('280f30e3-bfb6-407c-a9ec-164ee15ac5de', translator_bears_interpretation_burden, conventional).
narrative_ontology:cs_reference_frame('280f30e3-bfb6-407c-a9ec-164ee15ac5de', mid_twentieth_century_accessibility_imperative).
narrative_ontology:cs_drift_state('280f30e3-bfb6-407c-a9ec-164ee15ac5de', contemporary_digital_tools_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('280f30e3-bfb6-407c-a9ec-164ee15ac5de', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(biblical_source_text__dynamic_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, lay_readers).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, missionary_organizations).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, pastoral_communities).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, academic_scholars).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, word_study_practitioners).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, textual_reconstructionists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Access Bible passages through translations prioritizing intelligibility and meaning-impact in contemporary language. They benefit from versions that communicate immediately without requiring linguistic training or reference works. Their experience shapes pastoral preaching and personal devotional practice. They can adopt different translation versions freely but are embedded in denominational and congregational contexts that constrain which versions are recommended or used liturgically.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, lay_readers, beneficiary,
    powerless, biographical, mobile, global).

% Operate under an assumption that effective mission work requires the Bible in the receiving culture's contemporary idiom, prioritizing message transfer over structural preservation. Dynamic equivalence translations enable rapid deployment in new linguistic contexts and address the lived concerns of new believers without requiring them to learn the translator's scholarly apparatus. Their legitimacy rests partly on demonstrating that indigenous readers 'get' the message without institutional mediation.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, missionary_organizations, beneficiary,
    organized, biographical, constrained, global).

% Set the standard for preaching and teaching in congregations by choosing which translation version to use, recommending it to members, and interpreting passages through its lens. They benefit from dynamic equivalence versions by reducing friction in teaching—the text lands in contemporary ears immediately, enabling focus on theological application rather than lexical backstory. They also set the enforcement boundary: a congregation adopting a dynamic equivalence mandate effectively excludes formal-equivalence studies from the pulpit unless explicitly contextualized as 'deeper study.' This enforcement happens through recommendation authority, not prohibition.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, pastoral_communities, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__dynamic_equivalence_reading, pastoral_communities, beneficiary).

% Work in academic and seminarian contexts where dynamic equivalence translations obscure the morphological and lexical precision required for advanced word studies, textual criticism, and syntactic analysis. When preaching or teaching in congregational settings influenced by dynamic equivalence mandates, they must translate the translation back into the source language to make their point, or restrict advanced study to separate forums. They bear the cost of a bifurcated landscape where sermon texts and study texts are treated as different authorities. Their exit options are constrained because the dominance of dynamic equivalence in pastoral contexts limits where their precision-focused work is legible.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, academic_scholars, payer,
    powerful, biographical, constrained, national).

% Teach and practice using lexicographical and morphological methods (tracing Greek roots, parsing verb forms, tracking semantic fields). Dynamic equivalence translations undermine the visibility of the source-language structures their method depends on, forcing them either to shift methods entirely (adopting thematic or theological approaches) or to operate outside the congregational setting where dynamic equivalence is enforced as the legitimate reading. Their professional identity is fused with precision-based methods; they experience dynamic equivalence enforcement as a boundary that says 'your method is not welcome in the primary reading context.'
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, word_study_practitioners, payer,
    moderate, biographical, identity_locked, global).

% Work on the textual basis of the Bible—manuscript variants, scribal practices, hypothetical original texts. Dynamic equivalence translations treat the source text as settled and move directly to meaning-transfer, foreclosing the visibility of textual questions (variant readings, emendations, attestation gaps). When these scholars want to make textual-critical arguments in congregational or pastoral settings, the dynamic equivalence framework treats those arguments as unnecessary complications. Their work sits beneath the visible surface of the translation and is not integrated into the pastoral use case.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, textual_reconstructionists, payer,
    moderate, generational, constrained, national).

% Compose and maintain translations, making thousands of local decisions about how to render the source into the target language. Under dynamic equivalence mandate, they optimize for the comprehensibility and rhetorical impact of each phrase in the receiving community's language, explicitly deprioritizing word-for-word correspondence or morphological transparency. They enforce the mandate by internal review processes that reject source-language structures that would confuse the target reader. They also interpret pastoral feedback and user experience data to refine the translation iteratively, treating 'lay reader comprehension' as the north star for adequacy.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, translation_committees, agenda_setter,
    institutional, generational, constrained, global).

% Argue that fidelity to source-language structure is the translator's primary responsibility and that intelligibility is a secondary consequence that flows from faithful rendering plus community teaching. They contend that dynamic equivalence translations lose the Bible's literary and theological texture and that the burden of learning to read well-translated Scripture rests with the community, not the translator. In congregational and pastoral contexts where dynamic equivalence is the dominant mandate, their arguments are heard as elite gatekeeping. They are excluded from the primary setting where the Bible is read but trapped by the dominance of dynamic equivalence in the publishing and pastoral landscape.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, formal_equivalence_advocates, excluded,
    powerful, biographical, trapped, global).

% Pursue historical reconstruction of the original text as the primary scholarly task, treating both formal and dynamic equivalence translations as downstream of that reconstruction. In pastoral and missionary contexts dominated by dynamic equivalence, their reconstructive work is treated as peripheral—the 'real' text is the received/canonical text, not the hypothetical original. They are excluded from the canonical-reading setting but trapped by the fact that dynamic equivalence translations are based on source texts (usually established texts, not reconstructed) that their own methods would dispute.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, critical_scholars, excluded,
    powerful, biographical, trapped, national).

% Set or endorse translation policies for their congregations and denominations. Many Protestant and evangelical denominations have formally or informally endorsed dynamic equivalence as the translation philosophy for congregational use, citing effectiveness in reaching new believers and enabling immediate pastoral application. They enforce this through publication decisions, pulpit recommendations, and teaching standards. They are the analytical seat: they have the power to change the mandate but face constituency pressure (lay readers expect the versions they know) and institutional inertia.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, denominational_authorities, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__dynamic_equivalence_reading, translation_committees).
narrative_ontology:fixing_cost_class(biblical_source_text__dynamic_equivalence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of making biblical texts accessible and meaningful to readers without classical language training. Dynamic equivalence translation enables congregations, missionaries, and lay readers to encounter the biblical message in their own idiom, reducing the knowledge barrier to engagement with Scripture and freeing pastoral energy from linguistic explanation to theological application.
% TRANSFER_FUNCTION: Moves morphological precision and source-language structural transparency away from the primary reading context (congregations, missions, lay devotion) toward secondary or academic contexts (seminaries, study-reference tools, scholarly forums). Gain in immediate comprehensibility; loss in transparent access to the source language's specific choices.
% ABSENT_VOICES: Formal-equivalence advocates and critical-reconstructive scholars are structurally excluded from the primary congregation-level reading context. They would argue that the mandate sacrifices precision and historical fidelity for ease, but their voices are routed to 'deeper study' forums rather than the canonical-reading frame. Textual reconstructionists are also excluded: the questions they raise about the source text's basis are treated as pre-translation housekeeping, not relevant to the communicative act.
% DISAPPEARANCE_RATIONALE: If the dynamic equivalence mandate vanished, congregations would experience an immediate friction increase—passages would require more explanation, pastoral preaching would slow, and new believers would face steeper linguistic entry. Some congregations would shift to formal-equivalence or critical-reading modes; others would develop expanded teaching structures around source texts. Scholarly work in word studies and textual criticism would gain visibility in pastoral settings. The Bible would remain central, but access would bifurcate along educational lines more starkly.
% FOUNDING_PROBLEM: Early-twentieth-century missionary and evangelical movements encountered the problem that formal, linguistically precise translations kept Scripture locked behind scholarly gatekeeping. Lay readers and indigenous church planters needed the Bible's core message accessible in contemporary idiom without requiring them to become linguists or memorize a scholarly apparatus. The dynamic equivalence reading developed as a solution: let the translator bear the burden of finding the contemporary equivalent, freeing the reader to encounter meaning directly.
% FOUNDING_PROBLEM_CORROBORATION: Missionary organizations and evangelical denominations attest the problem is live: new believers and non-Western churches still face linguistic barriers with formal translations. Academic scholars and textual critics attest the problem is substantially solved by modern teaching resources and reference tools, and that the founding solution has become a cover for routinized extraction of precision. Legislative and policy developments (e.g., ESV adoption in evangelical seminaries as a partial counter-movement) reflect the contested status.
narrative_ontology:disappearance_verdict(biblical_source_text__dynamic_equivalence_reading, contested).
narrative_ontology:founding_problem_status(biblical_source_text__dynamic_equivalence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__dynamic_equivalence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_source_text__dynamic_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__dynamic_equivalence_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__dynamic_equivalence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__dynamic_equivalence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__dynamic_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is moderate-to-high: the constraint's operation systematically obscures source-language precision and makes word-study methods less legible in the primary reading context, forcing academic readers to maintain a parallel reference apparatus. Suppression (0.45) is moderate: the enforcement happens through recommendation and pulpit authority, not legal prohibition, but it creates a strong boundary against formal-equivalence and critical-study intrusions into congregational reading. Theater ratio (0.28) is low-to-moderate: the constraint serves a genuine coordination function (making Scripture accessible), but the measurement series shows a slow rise, suggesting that increasing claims about 'readability' and 'contemporary relevance' now carry more performative weight than early versions, where accessibility was a harder problem. The temporal trajectory shows extractiveness rising steeply from t=0 to t=30 (the growth phase of dynamic equivalence dominance, roughly 1950s–1980s), then leveling off (t=30–60), as the constraint became institutionalized in evangelical and pastoral contexts and formal equivalence advocates shifted to building separate scholarly communities. Suppression requirement stays relatively flat but ticks upward slightly over time, indicating that maintaining the boundary against formal-equivalence intrusions requires modest-but-consistent enforcement effort. The measurement grid is authored on one shared timeline so every metric is present at every time point.
 *
 * PERSPECTIVAL GAP:
 *   The pastoral and missionary beneficiary seats and the agenda-setter seats (pastoral communities, translation committees) experience this constraint as genuine coordination: they solved a real problem of access and the constraint persists because it works. The academic and scholarly payer seats (word_study_practitioners, textual_reconstructionists, critical_scholars) experience the same structure as enforced extraction—their methods are deprioritized and their findings are routed to secondary forums. The formal_equivalence_advocates, though excluded rather than victimized, occupy a peculiar position: they view dynamic equivalence as a loss of fidelity, not a gain in accessibility, and they frame the entire constraint as a false trade-off. The engine will compute these divergent seat experiences from the structural data (beneficiary vs. victim declarations, power levels, exit options, enforcement machinery). The author's claim that this is a tangled rope (genuine coordination + asymmetric extraction) rests on that structural divergence being real and measurable.
 *
 * DIRECTIONALITY LOGIC:
 *   Lay readers sit at low d (beneficiary end): they gain immediate comprehensibility and have mobile exit (they can read any translation they choose, though congregational contexts constrain recommendation). Missionary organizations sit near beneficiary end (d ~0.2): they collects pastoral effectiveness and rapid deployment; their exit is constrained by constituency expectations, but those expectations align with their interests. Pastoral communities sit very near beneficiary end (d ~0.15): they set the mandate and collect the benefit of reduced friction in teaching; their exit is constrained by congregational history but they prefer the constraint and could change it if they chose. Academic scholars sit near target end (d ~0.75): they bear the cost of precision-loss and bifurcated reference apparatus; their exit is constrained by the dominance of dynamic equivalence in pastoral contexts (where much of their teaching and preaching happens). Word_study_practitioners sit at the target end (d ~0.8): their identity-locked exit makes them especially vulnerable—leaving the precision-based method is professionally costly, yet the constraint makes precision-based work illegible in the primary reading context. Textual_reconstructionists sit at target end (d ~0.7): their work is subordinated to the settlement of the source text and is not visible in pastoral translation choices. Formal_equivalence_advocates and critical_scholars are excluded (not coordinated or extracted, but trapped outside the primary setting). Translation committees sit at beneficiary end (d ~0.2): they set the translation rule and collect professional legitimacy and denominational adoption. Denominational authorities sit at the analytical seat (d = 0.5): they have the power to change the mandate but no structural interest in doing so, though they face pressure from academic and scholarly constituencies questioning the lost precision.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—lay readers lacked access to Scripture in contemporary language—is contested in status. Evangelical and missionary organizations attest it is live: new believers and non-Western churches still need accessible Bibles. Academic institutions and seminaries increasingly attest it is dead: modern reference tools, study Bibles, online lexicons, and teaching structures have largely solved the access problem, and the constraint now persists as institutional inertia and pastoral convenience. The disappearance verdict is contested (not unified). The mandatrophy question is whether the constraint's primary function has atrophied (i.e., it solved its founding problem and now persists as a zombie), or whether the founding problem is genuinely live and the constraint remains justified. The measurement data shows extractiveness rising during the growth phase (t=0–30) as the constraint became institutionalized, then plateauing (t=30–60) as academic and scholarly work adapted by creating parallel reference systems. This plateau suggests the constraint is no longer driven by the coordination need (which would show declining extractiveness if the problem were solved and the constraint relaxed) but by institutional preference and path dependence. A genuine mandate-obsolescence case would show theater ratio rising sharply while extractiveness declines; this case shows theater ratio rising modestly while extractiveness plateaus, suggesting the constraint is neither in rapid-decay mode nor in mature-zombie performance mode, but in institutionalized equilibrium.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_natural_law,
    'Is dynamic equivalence a necessary feature of effective translation (grounded in universal principles of language and communication), or is it a contingent institutional choice that depends on treating lay accessibility as the supreme value?',
    'Historical examination of non-Western translation traditions and contemporary non-Christian sacred-text translation practices: if communities that do not adopt dynamic equivalence achieve equal or superior accessibility outcomes, the universality claim is falsified and dynamic equivalence becomes a reading choice, not a law.',
    'If dynamic equivalence is universal, the constraint is closer to a coordination mechanism (rope). If it is a reading choice, the constraint is explicitly a tangled rope (coordination + extraction) and the extractiveness should be re-measured as intentional loss of precision for communicative gain, not accidental degradation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_natural_law, empirical, 'Whether dynamic equivalence is a necessity or a reading choice.').

omega_variable(
    source_language_preservation_vs_reader_burden,
    'Can the coordination benefit of accessibility be decoupled from the extraction cost of precision loss—i.e., can a reading community achieve both morphological transparency and contemporary comprehensibility through teaching and reference infrastructure?',
    'Empirical observation from communities that use formal-equivalence translations with extensive teaching, study resources, and hermeneutical training: if lay readers in such communities achieve comparable comprehension and engagement to dynamic-equivalence communities, the decoupling is feasible and the extraction is optional.',
    'If decoupling is feasible, the constraint becomes a choice to extract precision in exchange for translator burden reduction (moving burden to teachers and learners), not a necessity. If decoupling is infeasible, the extractiveness is the cost of solving the coordination problem and is justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(source_language_preservation_vs_reader_burden, empirical, 'Whether accessibility and precision can be jointly achieved or require structural trade-off.').

omega_variable(
    suppression_internalization_in_scholar_identity,
    'Is the suppression of word-study methods in congregational settings primarily structural (external barrier: translations don''t make the source structures visible) or internalized (scholars have accepted the boundary: word study belongs in the study, preaching belongs in the congregation)?',
    'Post-boundary-shift observation: if institutional norms were to shift and formal-equivalence translations gained denominational adoption, would word-study methods regain visibility in pulpit work, or have scholars accepted the binary division as legitimate?',
    'If suppression is structural, removing the constraint would restore the visibility of word-study methods. If suppression is internalized, the constraint persists as a cognitive boundary even if the translation framework changes, because scholars have incorporated the division into their professional identity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_in_scholar_identity, empirical, 'Whether suppression of word-study methods is structural or internalized in scholar identity.').

omega_variable(
    beneficiary_reading_authenticity,
    'Do lay readers genuinely benefit from dynamic equivalence translations (they understand better, engage more), or do they benefit from the framing that they are the primary audience (the translation is designed for them, so they experience the text as ''for us'')?',
    'Comparative comprehension studies and engagement metrics: measure lay-reader comprehension and theological retention across dynamic-equivalence and formally-translated texts, controlling for study resources and teaching context. If comprehension and retention are equal, the benefit is partly perceptual (the framing benefit), not structural.',
    'If the benefit is purely perceptual, part of the measured extractiveness is the cost of maintaining the beneficiary narrative, not the cost of solving the accessibility problem. If the benefit is structural (measurable comprehension gain), the extractiveness is justified as coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_reading_authenticity, empirical, 'Whether lay-reader benefit is structural or partly narrative/perceptual.').

omega_variable(
    kernel_vs_reading_identity,
    'Is ''the biblical source text'' a fixed kernel that can be read in multiple ways, or does each reading (dynamic, formal, critical) constitute its own kernel, such that there are not three readings of one text but three texts?',
    'Theoretical: if changing the reading changes which text is considered authoritative (e.g., critical reconstruction treats the original as the text, dynamic equivalence treats the received/canonical as the text), then the kernel is not fixed and the readings are not interpretations of one constraint but generators of different constraints.',
    'If readings generate different kernels, this story''s constraint is not a reading of a shared kernel but a constraint in its own right, and the committer frame should be dropped. If readings share a kernel, the constraint is one instantiation of a shared commitment that different parties interpret differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_reading_identity, conceptual, 'Whether multiple readings of one kernel, or multiple kernels generated by reading choices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__dynamic_equivalence_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(bibl_tr_t10, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(bibl_tr_t20, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(bibl_tr_t30, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(bibl_tr_t40, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(bibl_tr_t50, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement(bibl_tr_t60, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(bibl_be_t10, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(bibl_be_t20, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(bibl_be_t30, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(bibl_be_t40, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(bibl_be_t50, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(bibl_be_t60, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(bibl_su_t10, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(bibl_su_t20, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(bibl_su_t30, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement(bibl_su_t40, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(bibl_su_t50, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 50, 0.45).
narrative_ontology:measurement(bibl_su_t60, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 60, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__dynamic_equivalence_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(biblical_source_text__dynamic_equivalence_reading, 0.18).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__critical_reconstructive_reading).

% DUAL FORMULATION NOTE:
% The biblical source text constraint family comprises three structurally distinct constraints instantiating competing readings of the contested kernel 'biblical_source_text'. This story (dynamic_equivalence_reading) derives ε from treating target-language communicativeness as primary. The sibling readings—formal_equivalence_reading and critical_reconstructive_reading—derive their ε values from different primary commitments (source-structure fidelity and historical reconstruction, respectively). These are not three measurements of the same extraction; they are three different constraints with three different ε values. All three stories are linked via network.affects_constraints to capture the structural dependency: each reading's legitimacy claim and institutional position are affected by the presence and arguments of the other readings. The ε values are independent (reading-indexed, not a function of perspective), and the three stories represent a constraint-family decomposition per the ε-invariance principle (DP-001).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
