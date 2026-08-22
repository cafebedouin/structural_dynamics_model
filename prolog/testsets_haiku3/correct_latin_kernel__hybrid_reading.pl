% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__hybrid_reading, []).

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
 *   constraint_id: correct_latin_kernel__hybrid_reading
 *   human_readable: Hybrid Latin Reconstruction Protocol
 *   domain: intellectual_history/philology
 *
 * SUMMARY:
 *   The hybrid reading of the Latin kernel establishes a bifurcated
 *   legitimacy standard: medieval Latin forms are treated as continuous with
 *   Classical morphology (morphology is preserved across time), but medieval
 *   syntax and lexicon are treated as corruptions requiring expert-directed
 *   textual recovery (recovery requires active scholarly reconstruction).
 *   This reading creates a two-tier system: what is morphologically
 *   legitimate need not be functionally continuous, and what is
 *   syntactically/lexically degraded can be recovered through a standardized
 *   protocol rather than accepted as living evolution. The constraint
 *   operates as an institutional gate: only textual judgments that fit the
 *   hybrid framework are recognized as philologically valid, while
 *   continuity-based readings and pragmatic-adequacy standards are excluded
 *   from the canon of approved methodology. The hybrid reading coordinates
 *   scholarly effort around a shared interpretive rule but simultaneously
 *   extracts authority from practitioners who worked on medieval texts
 *   through different frameworks.
 *
 * KEY AGENTS:
 *   - Philological reconstructionists: Institutional agenda-setters who control methodology and canonical authority; the primary beneficiary of the constraint's authority concentration.
 *   - Ecclesiastical authorities: Secondary institutional beneficiaries; the hybrid reading legitimizes medieval liturgical forms as morphologically sound, reducing pressure for radical textual revision.
 *   - Continuity theorists: Primary payer class; their evolutionary framework is marginalized, their scholarship excluded from canonical status, their professional standing eroded.
 *   - Textual modernizers: Secondary payer class with identity lock; their work is delegitimized unless adopting reconstructionist protocols; exit means professional invisibility.
 *   - Classical purists: Excluded parties; their restorationist premise contradicts the hybrid claim that medieval morphology is continuous; their voice is suppressed by the reading's framing.
 *   - Manuscript custodians: Observer seat; see the constraint from outside, but their resource allocation and research directions are shaped by which scholarly framework dominates.
 *   - Transmission historians: Powerful observer seat; their research can shift which framework commands authority by producing evidence about how and why texts changed.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, 0.62).
domain_priors:suppression_score(correct_latin_kernel__hybrid_reading, 0.58).
domain_priors:theater_ratio(correct_latin_kernel__hybrid_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__hybrid_reading, "Hybrid Latin Reconstruction Protocol").
narrative_ontology:topic_domain(correct_latin_kernel__hybrid_reading, "intellectual_history/philology").

domain_priors:requires_active_enforcement(correct_latin_kernel__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__hybrid_reading, 'e66f6895-dc15-4a69-b244-66d92bbb1285').
narrative_ontology:cs_kernel_codification('e66f6895-dc15-4a69-b244-66d92bbb1285', fixed_text).
narrative_ontology:cs_authority_grounding('e66f6895-dc15-4a69-b244-66d92bbb1285', expertise).
narrative_ontology:cs_interpretation_layer_present('e66f6895-dc15-4a69-b244-66d92bbb1285').
narrative_ontology:cs_reading_relation('e66f6895-dc15-4a69-b244-66d92bbb1285', correct_latin_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('e66f6895-dc15-4a69-b244-66d92bbb1285', correct_latin_kernel__discontinuity_reading, influences).
narrative_ontology:cs_axiom('e66f6895-dc15-4a69-b244-66d92bbb1285', foundational, morphology_transmission_continuous).
narrative_ontology:cs_axiom_status(morphology_transmission_continuous, holdable).
narrative_ontology:cs_axiom_grounding('e66f6895-dc15-4a69-b244-66d92bbb1285', morphology_transmission_continuous, empirically_contingent).
narrative_ontology:cs_axiom('e66f6895-dc15-4a69-b244-66d92bbb1285', secondary, syntax_lexicon_recoverable).
narrative_ontology:cs_axiom_status(syntax_lexicon_recoverable, holdable).
narrative_ontology:cs_axiom_grounding('e66f6895-dc15-4a69-b244-66d92bbb1285', syntax_lexicon_recoverable, instrumental).
narrative_ontology:cs_reference_frame('e66f6895-dc15-4a69-b244-66d92bbb1285', classical_morphological_standard_with_medieval_adaptation).
narrative_ontology:cs_drift_state('e66f6895-dc15-4a69-b244-66d92bbb1285', contemporary_corpus_linguistics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e66f6895-dc15-4a69-b244-66d92bbb1285', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__hybrid_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, philological_reconstructionists).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, ecclesiastical_authorities).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, continuity_theorists).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, textual_modernizers).
narrative_ontology:constraint_vindicates(correct_latin_kernel__hybrid_reading, latin_layered_transmission).
narrative_ontology:constraint_vindicates(correct_latin_kernel__hybrid_reading, morphosyntactic_heterogeneity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Specialized scholars who set and enforce the methodological standards for Latin textual criticism. They advance the hybrid reading as authoritative by controlling which reconstruction practices are recognized as legitimate philology, which sources are treated as primary, and which textual variants are accepted into the canonical apparatus. Their institutional standing rests on maintaining this disciplinary consensus. They publish in prestige journals, control peer review, direct graduate training, and determine which editions become canonical. Exit for them means abandoning institutional standing; they have arbitrage options (other philological traditions, other languages) but abandoning Latin reconstructionism would require starting over in a new field.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, philological_reconstructionists, agenda_setter,
    institutional, generational, arbitrage, global).

% Church institutions benefit from the hybrid reading because it validates medieval manuscript traditions as partially legitimate continuations rather than corruptions requiring complete displacement. The reading licenses preservation of medieval liturgical Latin forms and grammar as morphologically sound, reducing pressure to radically revise ecclesiastical texts to match Classical norms. They can operate effectively with either the hybrid or continuity reading (both preserve medieval forms); they have mobile exit because their core mission does not depend on which scholarly framework dominates.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, ecclesiastical_authorities, beneficiary,
    institutional, civilizational, mobile, global).

% Scholars who hold that Medieval Latin represents natural linguistic evolution from Classical Latin—that medieval forms are living language development, not recovery projects. They bear the cost of the hybrid reading by watching their interpretive framework marginalized: their textual judgments are treated as insufficient, their evolutionary model is reframed as unsystematic, and their scholarship is excluded from the canonical reconstruction methodology. Publishing outside the reconstructionist consensus requires visibility work they lack institutional resources for. They are constrained to operating in secondary venues, teaching at lower-status institutions, or abandoning their research program entirely. The directionality override (d=0.88) corrects the derivation: despite moderate power, their structural relationship to this constraint is nearly as extractive as a powerless agent's, because their professional standing depends on acceptance within the reconstructionist framework, making identity lock the binding constraint.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, continuity_theorists, payer,
    moderate, biographical, constrained, global).

% Practitioners and editors working with medieval texts in archives and regional studies who treat medieval Latin forms as functionally adequate for their descriptive purposes. The hybrid reading extracts from them by delegitimizing their textual judgments as insufficiently rigorous, marking their work as non-philological unless it submits to reconstructionist protocols. Their professional identity becomes dependent on adopting the hybrid framework to retain credibility; exit means ceasing to publish in recognized venues, losing research grants, and becoming effectively invisible to the disciplinary conversation. They remain in their local roles but under constant pressure to adopt methodologies they do not endorse.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, textual_modernizers, payer,
    powerless, biographical, identity_locked, local).

% Scholars advocating complete Classical Latin restoration and treatment of medieval forms as degradations to be stripped away. They are structurally excluded from the dominant conversation because the hybrid reading's central claim—that medieval morphology is legitimate—directly contradicts their foundational premise. Their voice in reconstruction methodology is suppressed not by active refusal but by the hybrid reading's framing, which makes their position appear methodologically indefensible. They remain organized (they publish, teach, maintain visibility in Classical philology), but they are trapped because abandoning their premise would mean accepting the hybrid framework, and accepting the hybrid framework would mean abandoning their research program.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, classical_purists, excluded,
    organized, biographical, trapped, global).

% Archivists, librarians, and paleographers who maintain access to medieval manuscripts. They occupy an analytical seat, observing which interpretation frameworks receive resource allocation for manuscript study, digitization, and preservation. Their role is to make manuscripts available; the hybrid reading shapes what questions get asked of those manuscripts and thus what preservation priorities emerge. They see the constraint as one possible framework among others; their scholarly contributions can advance any of the competing readings depending on what they discover about manuscript patterns.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, manuscript_tradition_custodians, observer,
    institutional, civilizational, analytical, global).

% Scholars who trace the history of Latin transmission itself—how texts moved through copying, recopying, commentary, and adaptation across time. They see all three readings (continuity, discontinuity, hybrid) as empirically contestable frameworks rather than settled truth, and their research can shift which framework commands explanatory authority. They observe the constraint from outside the reconstructionist consensus; their analytical power (ability to produce evidence that constrains which reading is plausible) makes them potentially decisive in future framework shifts.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, textual_transmission_historians, observer,
    powerful, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__hybrid_reading, philological_reconstructionists).
narrative_ontology:fixing_cost_class(correct_latin_kernel__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared methodological protocol for determining what counts as legitimate Latin textual recovery: morphological continuity anchors judgments as automatically legitimate, while syntax and lexicon require active scholarly reconstruction. This solves the coordination problem of multiple readers working from fragmented and variant manuscript traditions—a single decision-rule for what is continuous and what is corrupted enables consensus on canonical texts and validated readings. Without the protocol, different scholars would make incompatible textual judgments based on different standards, and no unified discipline of Latin philology could exist.
% TRANSFER_FUNCTION: Transfers interpretive authority from local textual practitioners (continuity readers, pragmatic editors) and evolutionary linguists to specialized philological institutions that control the reconstruction protocols. Moves decision-making power about text legitimacy from the distributed community of manuscript users to the centralized methodological apparatus. Simultaneously transfers institutional standing from continuity theorists to reconstructionists by making continuity-based judgment appear methodologically naive. The arrangement moves prestige, publication venues, funding, and graduate training opportunities from alternative schools to reconstructionists, and concentrates the capacity to legitimate or delegitimize scholarship in a single institutional tradition.
% ABSENT_VOICES: Classical purists (who would argue for complete restoration of Classical norms as the only legitimate recovery goal) are excluded from shaping the hybrid reading's legitimacy by the reading's claim that medieval morphology is continuous—their premise contradicts the reading. Evolutionary linguists working outside the reconstructionist tradition are structurally silenced by the reading's claim that their work requires validation through reconstructionist methods. Local manuscript custodians and regional scholars who have worked with medieval texts on their own terms are marginalized by the framing that their work is non-philological unless it adopts reconstructionist protocols. Contemporary scholars in other linguistic traditions who might offer alternative frameworks for thinking about language transmission are absent from the conversation because it is conducted in Latin philology's specialized vocabulary.
% DISAPPEARANCE_RATIONALE: If the hybrid reading's authority evaporated, textual judgment would decentralize: manuscript editors would work from continuity frameworks or pragmatic adequacy standards, and reconstructionist scholars would lose the institutional standing their methodology confers. The landscape of textual authority would reorganize around competing frameworks (continuity, discontinuity, pragmatic adequacy) rather than the hybrid protocol. Editions would be published using different standards; what counts as an 'error' would become variable rather than standardized. The disciplinary consensus around which reconstructions are canonical would dissolve. Ecclesiastical institutions would face less methodological pressure to revise medieval texts. Graduate training would diversify away from the unified reconstructionist model.
% FOUNDING_PROBLEM: Medieval manuscripts show inconsistency: some forms align with Classical norms, others diverge systematically. Scholars needed a single interpretive framework to distinguish legitimate evolved forms from textual corruptions—a principled way to say which medieval Latin is 'real' and which is degradation that requires correction. The problem is framed as epistemological and methodological: faced with a medieval form that does not match Classical precedent, how do we know whether to accept it as evolved morphology, correct it as syntactic error, or replace it as lexical corruption?
% FOUNDING_PROBLEM_CORROBORATION: Philological reconstructionists attest the problem is live and urgent—inconsistency in manuscripts demands systematic resolution through rigorous methodology. Textual transmission historians attest that the 'problem' might reflect not corruption but the documented history of how texts changed hands through different copying traditions, each with its own linguistic standards and purposes. Continuity theorists attest that the 'problem' is a misframing: inconsistency might reflect living language variation rather than corruption; medieval scribes were capable linguists making reasonable choices for their audiences, not degrading Classical norms. Paleographers attest that manuscript patterns show variation across regions and purposes but not necessarily degradation—different regional traditions, different scribal training, different intended audiences all had different linguistic norms. No single corroboration from outside the reconstructionist beneficiary set; multiple authoritative sources (transmission historians, continuity theorists, paleographers) contest whether the founding problem is real or represents a misframing of normal textual variation and linguistic evolution.
narrative_ontology:disappearance_verdict(correct_latin_kernel__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(correct_latin_kernel__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__hybrid_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins moderate (0.48) and plateaus at 0.62 by interval midpoint, reflecting the gradual establishment of reconstructionist dominance over competing frameworks over roughly 25 years. Theater rises from 0.22 to 0.41 over the first 25 time units, then stabilizes—a pattern consistent with increasing methodological theater (publication of canonical editions, refinement of reconstruction protocols, ceremonial validation of the hybrid framework through textbook canonization and graduate training) that grows most rapidly during the expansion phase (0–25) and then settles into maintenance (25–40). Suppression requirement follows a similar profile, starting at 0.44 and rising to 0.58 by midpoint, stabilizing thereafter. This trajectory suggests three phases: (1) Expansion (0–15): gaining institutional consensus, establishing the hybrid framework as the default methodology, training new scholars in reconstructionist practices. (2) Consolidation (15–30): the framework becomes entrenched, alternative frameworks lose visibility, the barrier to publishing outside the consensus rises substantially. (3) Maintenance (30–40): suppression is routine, theater is performative (canonical editions, ceremonial applications of the protocol, textbook treatments), extractiveness and suppression stabilize. The plateau in extractiveness and theater after interval-midpoint is consistent with a tangled rope in steady state—enough coordination benefit (the shared protocol genuinely enables consensus on textual judgment) that the arrangement persists, but enough asymmetric extraction (authority concentration, methodological gatekeeping, exclusion of alternative frameworks) that suppression remains necessary to prevent defection by payer-seat scholars who would benefit from alternative frameworks receiving equal standing.
 *
 * PERSPECTIVAL GAP:
 *   From the reconstructionist institutional seat: the hybrid reading is genuine coordination—a principled method for textual judgment that brings rational order to medieval textual inconsistency and enables consensus among dispersed scholars. Extractiveness and suppression are legitimate costs of maintaining standards. From the continuity theorist seat: the hybrid reading is enforced extraction—a methodological gate designed to exclude evolutionary linguistics and concentrate interpretive authority in a privileged institutional seat that benefits from gatekeeping. Suppression is experienced as unfair exclusion of legitimate scholarship. From the textual modernizer seat: the hybrid reading is identity capture—professional credibility now requires adopting a framework that labels their prior work as insufficiently rigorous, and their exit is blocked because their professional identity is fused to their current role. From the observer seat (transmission historians): the reading is contestable—an interpretive choice with empirical merits and empirical liabilities, not a discovered truth about Latin's history. The engine computes these divergences: reconstructionist seat computes as ROPE or weak TANGLED_ROPE, continuity theorist computes as SNARE or strong TANGLED_ROPE, textual modernizer computes as SNARE, observer computes as ROPE or MOUNTAIN.
 *
 * DIRECTIONALITY LOGIC:
 *   The automated directionality derivation runs: Beneficiary classes (reconstructionists, ecclesiastical authorities) map to d ≈ 0.1–0.2 (full beneficiary end). Payer classes (continuity theorists, textual modernizers) map to d ≈ 0.8–0.9 (full target end) based on their victim declarations and constrained/identity-locked exits. The classical purists occupy the excluded seat (not in stakeholders derivation). Observers map to d ≈ 0.5 (symmetric). However, the derivation slightly underestimates the extractiveness on continuity theorists because their power is moderate (not powerless), which would normally dampen directionality. The directionality_override (power=moderate, d=0.88) corrects this: despite moderate power, their structural relationship to this constraint is nearly as extractive as a powerless agent's because their professional standing depends on acceptance within the reconstructionist framework. Their moderate institutional power (they can publish, teach, secure grants) is functionally negated by identity lock—their professional identity is bound to the Latin continuity framework, so rejecting it means professional death. This is the identity_lock mechanism: power is not irrelevant, but it is subordinated to identity fusion. The override represents this structural reality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (inconsistency in medieval manuscripts requiring a principled judgment rule) appears live to reconstructionists but contested to continuity theorists and transmission historians. The hybrid reading claims to SOLVE this problem by establishing a protocol; continuity theorists claim the 'problem' is a misframing (inconsistency might reflect living evolution, not corruption, or reflect normal scribal tradition variation). The mandatrophy question: Is the founding problem still what the constraint is doing, or has the constraint's primary function shifted from solving the problem to concentrating authority? Evidence for mandatrophy: (1) Theater ratio rises from 0.22 to 0.41 (steady rise for 40 years), suggesting growing performative activity relative to functional problem-solving. (2) Extractiveness plateaus at 0.62 by year 25 and remains flat thereafter, while theater continues rising—in the maintenance phase (25–40), the transfer function becomes more theatrical (canonical editions, degree programs teaching the hybrid framework, ceremonial applications) than extractive (new methodological advances, novel textual discoveries). (3) The founding_problem_status is CONTESTED, not LIVE—the reconstructionist consensus asserts the problem is live, but transmission historians and continuity theorists contest it, suggesting the problem is not shared across seats. Conclusion: The constraint shows SUBSTANTIAL mandatrophy. The founding problem (medieval textual inconsistency) is still partially live (reconstructionists attest it), but the constraint's operation has shifted significantly toward extractive authority concentration and performative maintenance rather than problem-solving innovation. The hybrid reading increasingly justifies itself by appeal to a problem that is precisely what is contested among authoritative outside voices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    morphosyntactic_boundary_ambiguity,
    'Is the boundary between ''legitimate medieval morphology'' and ''corrupted medieval syntax/lexicon'' a discovered linguistic fact or an enforced analytical convention?',
    'Comparative analysis of manuscript variation patterns: if morphological variation clusters distinctly from syntactic variation across a large sample of medieval manuscripts, with the clustering pattern independent of which Classical reference standard is chosen, the boundary is discoverable. If the clustering depends on which Classical reference standard is selected as the target, or if morphological and syntactic variation are correlated (suggesting a unified system rather than two distinct systems), the boundary is conventional.',
    'If the boundary is discovered (variation clusters distinctly by linguistic level), the hybrid reading captures a real feature of Latin transmission and its classification as tangled_rope (with genuine coordination benefit plus asymmetric extraction) holds. If the boundary is conventional (depends on the choice of reference standard), the reading is revealed as an imposed framework, and the classification reclassifies toward snare (the coordination benefit disappears because the boundary solving the coordination problem is not real, leaving only the extraction mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(morphosyntactic_boundary_ambiguity, empirical, 'Whether the morphology/syntax boundary is discovered or conventional.').

omega_variable(
    recovery_methodology_necessity,
    'Is active scholarly reconstruction (the recovery protocol) necessary to establish medieval syntax and lexicon as corruptions, or can they be accepted as functional alternatives under a continuity framework without losing textual utility?',
    'Pragmatic trial: prepare textual editions using (1) continuity-based judgment (accept medieval forms as functional), (2) reconstruction-based judgment (recover presumed Classical forms). Compare outcomes on readability, functional adequacy for the text''s original purpose (liturgical, legal, literary), meaning preservation, and scholarly utility. If outcomes are equivalent or continuity-based editions are superior, reconstruction is unnecessary. If reconstruction materially improves outcomes, the protocol is justified.',
    'If reconstruction is unnecessary (pragmatic outcomes equivalent or superior under continuity), extractiveness reclassifies upward—suppression is revealed as enforcement of a preferred methodology rather than solving a real coordination/quality problem. Classification flips toward snare. If reconstruction materially improves outcomes (better meaning preservation, fewer ambiguities, greater scholarly utility), the constraint maintains tangled_rope standing with authentic coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recovery_methodology_necessity, empirical, 'Whether textual recovery requires expert reconstruction or accepts pragmatic continuity.').

omega_variable(
    institutional_dependency_vs_truth_claim,
    'Does the hybrid reading persist because it solves the textual judgment problem better than alternatives, or because reconstructionist institutional authority is sufficient to enforce it regardless of comparative explanatory power?',
    'Longitudinal tracking (historical observation): Monitor whether continuity-based scholarship that produces novel insights or superior textual judgments gains institutional standing and citations, or remains marginalized despite quality. If quality scholarship is marginalized when it contradicts the hybrid framework, institutional gatekeeping is the primary mechanism. If continuity-based scholarship gains standing when demonstrably superior, truth-tracking is the mechanism.',
    'If institutional gatekeeping is the mechanism (quality is insufficient for standing), extractiveness is underestimated by formal metrics; the constraint operates as snare (pure extraction with methodological cover). If truth-tracking is the mechanism (quality work gains standing), suppression represents legitimate exclusion of inferior methods, and the tangled_rope classification holds with authentic coordination component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_dependency_vs_truth_claim, empirical, 'Whether the hybrid reading''s persistence rests on truth-tracking or institutional enforcement.').

omega_variable(
    reading_foreclosure_vs_coexistence_status,
    'Does the hybrid reading logically foreclose the continuity reading (both cannot be simultaneously held), or do they coexist as competing interpretive frameworks held by different scholarly communities?',
    'Formal logical analysis: if the hybrid claim ''morphology is continuous, syntax is corrupted'' logically entails ''not all medieval forms represent natural evolution'' (which contradicts continuity reading''s universal claim), foreclosure holds and the readings are in contradiction. If both can be held by different schools with different research agendas (hybrid for reconstructionist methodology, continuity for evolutionary linguistics), coexistence holds.',
    'If foreclosure holds, the hybrid reading actively displaces continuity reading (more aggressive extraction, more necessary suppression), and the network relationship should be ''forecloses''. If coexistence holds, the hybrid reading and continuity reading are parallel frameworks competing for authority rather than logically eliminating each other, and the network relationship should be ''coexists_with'' (current setting).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence_status, conceptual, 'Whether hybrid and continuity readings are logically exclusive or compatible frameworks.').

omega_variable(
    kernel_reading_status_under_linguistic_science,
    'Does the hybrid reading''s axiom of ''morphology transmission continuous'' hold under contemporary linguistic science, or is it an anachronism reflecting 19th-century philological assumptions?',
    'Contemporary historical linguistics review: assess whether the claim that morphological structure transmits more conservatively than syntax/lexicon is supported by comparative study of other language families and historical language change patterns. If supported, the axiom is scientifically grounded. If contradicted (e.g., if syntax can be more conservative than morphology under certain conditions), the axiom is revealed as era-specific.',
    'If the axiom is scientifically grounded, the hybrid reading''s legitimacy is strengthened; if anachronistic, the axiom is overridden (status=''overridden'') and the reading''s internal coherence is weakened. Over-time, overridden axioms accumulate and trigger potential repudiation_pressure in cs_structure.drift_state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_status_under_linguistic_science, empirical, 'Whether the morphology-conservatism axiom survives contemporary linguistic science.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__hybrid_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(corr_tr_t0, observed).
narrative_ontology:measurement(corr_tr_t5, correct_latin_kernel__hybrid_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(corr_tr_t5, observed).
narrative_ontology:measurement(corr_tr_t10, correct_latin_kernel__hybrid_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(corr_tr_t10, observed).
narrative_ontology:measurement(corr_tr_t15, correct_latin_kernel__hybrid_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(corr_tr_t15, observed).
narrative_ontology:measurement(corr_tr_t20, correct_latin_kernel__hybrid_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(corr_tr_t20, observed).
narrative_ontology:measurement(corr_tr_t25, correct_latin_kernel__hybrid_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(corr_tr_t25, observed).
narrative_ontology:measurement(corr_tr_t30, correct_latin_kernel__hybrid_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(corr_tr_t30, observed).
narrative_ontology:measurement(corr_tr_t40, correct_latin_kernel__hybrid_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(corr_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__hybrid_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(corr_be_t0, observed).
narrative_ontology:measurement(corr_be_t5, correct_latin_kernel__hybrid_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(corr_be_t5, observed).
narrative_ontology:measurement(corr_be_t10, correct_latin_kernel__hybrid_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(corr_be_t10, observed).
narrative_ontology:measurement(corr_be_t15, correct_latin_kernel__hybrid_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(corr_be_t15, observed).
narrative_ontology:measurement(corr_be_t20, correct_latin_kernel__hybrid_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(corr_be_t20, observed).
narrative_ontology:measurement(corr_be_t25, correct_latin_kernel__hybrid_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(corr_be_t25, observed).
narrative_ontology:measurement(corr_be_t30, correct_latin_kernel__hybrid_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(corr_be_t30, observed).
narrative_ontology:measurement(corr_be_t40, correct_latin_kernel__hybrid_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(corr_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__hybrid_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(corr_su_t0, observed).
narrative_ontology:measurement(corr_su_t5, correct_latin_kernel__hybrid_reading, suppression_requirement, 5, 0.49).
narrative_ontology:measurement_basis(corr_su_t5, observed).
narrative_ontology:measurement(corr_su_t10, correct_latin_kernel__hybrid_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement_basis(corr_su_t10, observed).
narrative_ontology:measurement(corr_su_t15, correct_latin_kernel__hybrid_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement_basis(corr_su_t15, observed).
narrative_ontology:measurement(corr_su_t20, correct_latin_kernel__hybrid_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement_basis(corr_su_t20, observed).
narrative_ontology:measurement(corr_su_t25, correct_latin_kernel__hybrid_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(corr_su_t25, observed).
narrative_ontology:measurement(corr_su_t30, correct_latin_kernel__hybrid_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(corr_su_t30, observed).
narrative_ontology:measurement(corr_su_t40, correct_latin_kernel__hybrid_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(corr_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__hybrid_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(correct_latin_kernel__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, correct_latin_kernel__discontinuity_reading).

% DUAL FORMULATION NOTE:
% The correct_latin_kernel constraint family decomposes into three distinct constraint stories, each instantiating a different reading of the contested kernel question 'What is the legitimate relationship between Classical and Medieval Latin?' The hybrid_reading (THIS story) bifurcates legitimacy: morphology continuous, syntax/lexicon requiring recovery. It influences (but does not foreclose) continuity_reading, which would show lower extractiveness because it accepts all medieval forms as evolved (no recovery needed, no expert gatekeeping required). It coexists with discontinuity_reading, which would show higher suppression because it treats recovery as symbolic reoccupation rather than error correction, making the recovery protocol more explicitly artificial and requiring more enforcement. The three stories are linked by network.affects_constraints to establish the kernel family structure; each story's cs_structure.reading_relations and cs_structure.axioms document the sibling relationships from that reading's perspective. Decomposition principle: ε is a property of the standing arrangement under contest (the hybrid reading of Latin transmission), assessed by the reading's own lights. The reading endorses: 'Medieval morphology is continuous, syntax/lexicon required recovery.' Its ε=0.62 is the extraction measured under THIS frame, not averaged across readings. A continuity reading would author ε≈0.20 (low extraction, because acceptance of all medieval forms reduces gatekeeping). A discontinuity reading would author ε≈0.75 (higher extraction, because recovery is framed as more artificial). Same kernel, different readings, different ε values—not because measurement changes, but because the standing arrangement the reading sees IS different (the reading's endorsed alternative shifts what counts as the 'baseline').

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(correct_latin_kernel__hybrid_reading, moderate, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
