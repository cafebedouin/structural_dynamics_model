% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__revisable_translation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__revisable_translation_reading, []).

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
 *   constraint_id: kjv_text_1611__revisable_translation_reading
 *   human_readable: KJV as Revisable Translation (Scholarly Reading)
 *   domain: religious/textual_criticism
 *
 * SUMMARY:
 *   The revisable-translation reading of the KJV kernel instantiates a
 *   specific hermeneutical stance: the KJV is a historically valuable but
 *   scientifically improvable translation; better manuscripts (discovered
 *   since 1611) and modern linguistic knowledge justify producing new, more
 *   accurate English versions. This reading is one of three major competing
 *   readings of the same kernel (the KJV text itself). The
 *   exclusive-inspiration reading holds the KJV is divinely preserved and
 *   inerrant; the functional-equivalence reading treats multiple translations
 *   as serving different legitimate purposes. The revisable-translation
 *   reading treats the choice of which translation to use as a matter of
 *   textual-critical judgment, delegating authority to academic scholars
 *   rather than ecclesiastical tradition or inspiration doctrine. The
 *   measurement series tracks extractiveness growth (from 0.18 to 0.38 over
 *   40 time units) driven by expanding publishing market capture and
 *   scholarly authority consolidation, with low suppression and minimal
 *   theater — the reading operates transparently and does not require
 *   coercive enforcement to persist.
 *
 * KEY AGENTS:
 *   - academic_textual_scholars: agenda-setters, institutional power, analytical exit — establish the scholarly standard for evaluating translations by reference to manuscript evidence and historical linguistics
 *   - modern_translation_publishers: beneficiaries, powerful institutional actors, arbitrage exit — profit from the legitimacy of the revisable-translation reading via copyright, market positioning, and publishing volume
 *   - kjv_exclusive_churches: payers, moderate power, identity-locked exit — bear institutional friction and delegitimization pressure as the academic consensus marginalizes their preferred text
 *   - general_protestant_readers: bifurcated (beneficiary + payer), organized, mobile exit — gain linguistic clarity but lose inherited certainty, now required to exercise judgment about translation selection
 *   - kjv_manuscript_defenders: excluded, moderate power, constrained exit — their alternative text-critical framework is institutionally excluded from mainstream academic discourse despite scholarly merit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__revisable_translation_reading, 0.38).
domain_priors:suppression_score(kjv_text_1611__revisable_translation_reading, 0.22).
domain_priors:theater_ratio(kjv_text_1611__revisable_translation_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(kjv_text_1611__revisable_translation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__revisable_translation_reading, rope).
narrative_ontology:human_readable(kjv_text_1611__revisable_translation_reading, "KJV as Revisable Translation (Scholarly Reading)").
narrative_ontology:topic_domain(kjv_text_1611__revisable_translation_reading, "religious/textual_criticism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__revisable_translation_reading, 'e2a6c98a-43f4-482c-a6c1-c98348c6b864').
narrative_ontology:cs_kernel_codification('e2a6c98a-43f4-482c-a6c1-c98348c6b864', fixed_text).
narrative_ontology:cs_authority_grounding('e2a6c98a-43f4-482c-a6c1-c98348c6b864', expertise).
narrative_ontology:cs_interpretation_layer_present('e2a6c98a-43f4-482c-a6c1-c98348c6b864').
narrative_ontology:cs_reading_relation('e2a6c98a-43f4-482c-a6c1-c98348c6b864', kjv_text_1611__exclusive_inspiration_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2a6c98a-43f4-482c-a6c1-c98348c6b864', kjv_text_1611__functional_equivalence_reading, influences).
narrative_ontology:cs_axiom('e2a6c98a-43f4-482c-a6c1-c98348c6b864', foundational, older_manuscripts_more_authoritative).
narrative_ontology:cs_axiom_status(older_manuscripts_more_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('e2a6c98a-43f4-482c-a6c1-c98348c6b864', older_manuscripts_more_authoritative, empirically_contingent).
narrative_ontology:cs_axiom('e2a6c98a-43f4-482c-a6c1-c98348c6b864', foundational, translation_fidelity_measurable_by_linguistic_scholarship).
narrative_ontology:cs_axiom_status(translation_fidelity_measurable_by_linguistic_scholarship, holdable).
narrative_ontology:cs_axiom_grounding('e2a6c98a-43f4-482c-a6c1-c98348c6b864', translation_fidelity_measurable_by_linguistic_scholarship, empirically_contingent).
narrative_ontology:cs_reference_frame('e2a6c98a-43f4-482c-a6c1-c98348c6b864', textual_critical_authority).
narrative_ontology:cs_drift_state('e2a6c98a-43f4-482c-a6c1-c98348c6b864', contemporary_manuscript_discovery_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('e2a6c98a-43f4-482c-a6c1-c98348c6b864', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(kjv_text_1611__revisable_translation_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, academic_textual_scholars).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, modern_translation_publishers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kjv_text_1611__revisable_translation_reading, general_protestant_readers).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, kjv_exclusive_churches).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, general_protestant_readers).
narrative_ontology:constraint_victim(kjv_text_1611__revisable_translation_reading, conservative_theological_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the scholarly standard for evaluating biblical translations by reference to original-language manuscript evidence, linguistic expertise, and historical analysis. They argue that the KJV's 1611 base texts (Textus Receptus, limited Latin sources) have been superseded by older Greek and Hebrew manuscripts discovered since then, and that modern philological knowledge justifies revision. They produce academic commentary, lead translation projects, and train new scholars in the revisionist methodology.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, academic_textual_scholars, agenda_setter,
    institutional, generational, analytical, global).

% Publish modern English translations (ESV, NRSV, NIV, etc.) justified by the revisable-translation reading. They benefit directly from the scholarly consensus that newer translations are more accurate to the original languages, which positions their products as superior to the KJV in fidelity. Their commercial interest aligns with the reading's scholarly legitimacy.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, modern_translation_publishers, beneficiary,
    powerful, biographical, arbitrage, global).

% Congregations and denominations that treat the KJV as uniquely authoritative and resist the revisable-translation reading on theological or traditional grounds. They face increasing pressure to justify their position against the scholarly consensus, must defend the KJV's text-critical basis repeatedly, and bear the cost of institutional friction as younger members encounter the academic reading in secular education contexts.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, kjv_exclusive_churches, payer,
    moderate, biographical, identity_locked, regional).

% Congregations and individual readers who accept the revisable-translation reading and choose modern translations for study. They benefit from greater linguistic clarity and direct access to scholarly reasoning about translation choices. They also carry a diffuse cost: the proliferation of competing translations creates choice overhead, and the delegitimization of the KJV's authority structure (once unquestioned within Protestantism) requires individuals to exercise judgment about which translation to trust, shifting from inherited certainty to learned discrimination.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, general_protestant_readers, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__revisable_translation_reading, general_protestant_readers, payer).

% The broader theological and ecclesiastical tradition that historically treated the KJV as a standard English text. The revisable-translation reading challenges the assumption that the KJV is stable and authoritative by right, requiring the tradition to either adopt the reading (and retrain in new translations) or entrench against it. The constraint creates institutional friction and splits within denominations.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, conservative_theological_tradition, payer,
    moderate, generational, constrained, global).

% Scholars and theologians who hold that the Textus Receptus and the texts underlying the KJV are superior to or equal in authority to older manuscripts, and that the revisable-translation reading is methodologically unsound. They are structurally excluded from the academic agenda-setting process because the dominant institutional consensus has endorsed the revisable reading; their alternative text-critical framework (arguments for TR/KJV manuscript priority) is treated as non-mainstream in academic publishing and training.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, kjv_manuscript_defenders, excluded,
    moderate, generational, constrained, global).

% The disciplinary community that evaluates and validates textual scholarship. Observes the competing readings and adjudicates which methodologies and evidence count as legitimate. Takes testimony from all parties through peer review, conference presentation, and scholarly publication.
narrative_ontology:constraint_stakeholder(kjv_text_1611__revisable_translation_reading, academic_textual_criticism_field, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kjv_text_1611__revisable_translation_reading, modern_translation_publishers).
narrative_ontology:fixing_cost_class(kjv_text_1611__revisable_translation_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared reference frame for evaluating biblical translation accuracy: appeals to original-language manuscript evidence, historical linguistics, and scholarly methodology rather than to tradition or ecclesiastical authority. Enables Protestant denominations and translation publishers to coordinate on a common standard for comparing translations without requiring institutional hierarchy to dictate which version to adopt.
% TRANSFER_FUNCTION: Moves scholarly authority and translation-selection power from inherited ecclesiastical tradition to academic experts in textual criticism and historical linguistics. Transfers the commercial benefit of translation publishing to those aligned with the revisable-translation reading (modern translation publishers). Removes the KJV's monopoly on 'authoritative English Bible' status and distributes that status contestably among competing modern translations.
% ABSENT_VOICES: Conservative scholars and TR-defending textual critics are institutionally excluded from mainstream academic publishing and training; their alternative text-critical frameworks are not entertained as equally valid in academic discourse. Denominations and lay readers who prefer the KJV for non-scholarly reasons (literary preference, ecclesiastical tradition, comfort with archaic language) are present but lack institutional power to set the consensus standard.
% DISAPPEARANCE_RATIONALE: If the revisable-translation reading and its scholarly legitimacy disappeared, the KJV would revert to de facto unchallenged status in many Protestant contexts; denominations could teach it without academic justification; modern translation publishing would lose its primary legitimating narrative. The reading's disappearance would mean the loss of a framework that permits academic scholars to claim authority over translation selection.
% FOUNDING_PROBLEM: The KJV was translated from limited source texts (Textus Receptus, Vulgate, limited Greek manuscripts) available in 1611. Over the centuries, older and more reliable Greek and Hebrew manuscripts were discovered (Dead Sea Scrolls, earlier papyri, Byzantine-era manuscripts with variant attestation). Linguistic knowledge of Koine Greek and biblical Hebrew advanced substantially. The founding problem: how should Protestants handle the gap between the text the KJV was translated from and the better manuscript evidence now available?
% FOUNDING_PROBLEM_CORROBORATION: Academic textual scholars (independent of modern translation publishers) attest the founding problem persists: new manuscripts continue to emerge, linguistic analysis continues to refine, and the discrepancy between the Textus Receptus and earlier sources remains empirically verifiable. Paleographers, papyrologists, and historical linguists from secular and religious universities, and from institutions without direct stake in modern translation sales, independently corroborate that the manuscript and linguistic evidence justifies the revisable-translation premise.
narrative_ontology:disappearance_verdict(kjv_text_1611__revisable_translation_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__revisable_translation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__revisable_translation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kjv_text_1611__revisable_translation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__revisable_translation_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__revisable_translation_reading_tests).
:- end_tests(kjv_text_1611__revisable_translation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end), not high, because the revisable-translation reading does not require coercive enforcement — it operates through legitimate scholarly authority and voluntary choice. Academic scholars who benefit from setting the standard find their institutional positions and publishing opportunities enhanced; modern translation publishers benefit from a market-selection framework that legitimates their products as 'more accurate.' The extraction is not predatory because alternatives (KJV, other translations, literalist defenses of older texts) remain available to readers and churches; what changes is the prestigious consensus standard. Suppression is low (0.22) because the reading permits choice — it does not forbid the KJV, only reframes it as historically important but no longer the authoritative standard. Theater is minimal (0.18) because the scholarly apparatus is transparent: manuscript evidence is published, linguistic arguments are peer-reviewed, and the justification for revision is openly available. The measurement series shows extractiveness rising in the first 20 time units (as the revisable reading consolidates institutional power) then plateauing (the reading reaches equilibrium once modern translations dominate academic and publishing discourse). Suppression and theater rise slightly in parallel, indicating minor institutional friction around alternative readings (KJV defenders must work harder to justify their position as the consensus shifts).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (academic scholars) and the payer seats (KJV-exclusive churches, conservative tradition) experience the constraint very differently. From the scholar's position, the reading is genuine coordination: establishing a shared, evidence-based standard for translation evaluation that permits rational discourse and improves English Bible translation across the board. The extraction (to modern translation publishers) is a side effect of legitimate scholarly authority. From the payer seats, the reading is experienced as delegitimization of their inherited text and suppression of their interpretive framework — they are told their preferred translation is less accurate, their text-critical defense is non-scholarly, and their ecclesiastical tradition is superseded by secular academic standards. The engine computes per-seat classifications from the structural data: a seat with high benefits and arbitrage exit (modern publishers) will compute as beneficiary and low directionality; a seat with moderate power, identity-locked exit, and institutional friction will compute as payer and high directionality. The authored claim (rope: coordination function with asymmetric benefit distribution) and the authored metrics (moderate extractiveness, low suppression, moderate resistance) reflect the reading's own self-understanding — it is coordination by scholarly consensus, not coercive enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is driven by beneficiary/victim declarations and exit options. Academic textual scholars are declared beneficiaries with institutional power and analytical exit: they set the agenda and their positions are validated by the consensus. Modern translation publishers are beneficiaries (powerful, arbitrage exit) who capture commercial value from the reading's legitimacy. KJV-exclusive churches and the conservative theological tradition are victims (moderate power, identity-locked or constrained exit) who bear the cost of institutional marginalization and intra-denominational friction. General Protestant readers are bifurcated: they benefit from clearer translations but pay the diffuse cost of inherited-authority dissolution. KJV-manuscript defenders are excluded, not victims — they are structurally outside the consensus-setting process, so their exit is constrained by institutional marginalization rather than direct extraction. The directionality override consideration: should scholars who genuinely seek accurate translation be classed as extractive beneficiaries, or are they net-neutral arbiters whose benefit is the satisfaction of truth-seeking? The authored treatment: they are beneficiaries because institutional authority is a real benefit (career advancement, publishing opportunities, training pipeline control), regardless of motivational innocence. The reading's extraction is structural, not individual-level.
 *
 * MANDATROPHY ANALYSIS:
 *   The revisable-translation reading avoids mandatrophy because its founding problem (better manuscripts and linguistic knowledge available) remains live and empirically verifiable. The reading does not persist by inertia or institutional theater — it persists because the text-critical case for revision is continuously reinforced by new discoveries (Dead Sea Scrolls fragments, Byzantine manuscripts with variant readings discovered in recent decades) and because academic methodology itself is self-correcting. If the founding problem were to die (e.g., if scholarship concluded the Textus Receptus was actually superior), the reading would face mandatrophy pressure. Current state: the founding problem is empirically live, scholarly consensus is robust, and the reading's persistence is justified by ongoing evidence rather than institutional theater. Theater ratio is low (0.18) precisely because the scholarly apparatus does not require performative defense — the evidence is published, the methodology is transparent, and alternatives are not suppressed by law or force.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manuscript_authority_grounding,
    'Why should older manuscripts count as more authoritative than later transmission traditions? Is manuscript age an epistemically reliable proxy for textual originality, or a methodological convention that could be questioned?',
    'Theological and historical examination: does the principle ''older = more original'' hold across non-religious textual traditions, or is it contingent on assumptions about corrupted transmission? Can alternative explanations (e.g., later manuscripts representing deliberate theological development, not scribal error) account for the data?',
    'If manuscript authority is grounded in an empirical principle (older sources are closer to originals), the revisable-translation reading stands. If it is a methodological convention, conservative scholars defending TR priority have a standing challenge to the reading''s epistemological foundation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manuscript_authority_grounding, conceptual, 'Whether textual authority derives from manuscript age (the revisable reading''s assumption) or from an alternative epistemology (e.g., transmission-community stability, theological consistency).').

omega_variable(
    reading_consolidation_mechanism,
    'How much of the revisable-translation reading''s institutional dominance derives from its intellectual merit (superior manuscript evidence and linguistic arguments) versus from academic gatekeeping that excludes alternative text-critical frameworks from mainstream publication and training?',
    'Audit of academic journal peer-review and dissertation-committee evaluation: are alternative text-critical frameworks (e.g., TR-priority arguments, Byzantine-text arguments) evaluated on merit or excluded by methodological fiat? How many scholars trained in mainstream programs encounter TR-priority arguments as live scholarly positions rather than historical curiosities?',
    'If the reading''s dominance is merit-based, it is genuine coordination by evidence. If it is partly sustained by gatekeeping, the reading operates with higher suppression (or identity-locking) than the authored 0.22 suggests, and some payer seats (KJV defenders) experience it as institutional exclusion rather than as intellectual disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_consolidation_mechanism, empirical, 'Whether the revisable-translation reading''s academic consensus reflects intellectual merit or institutional gatekeeping of alternative frameworks.').

omega_variable(
    sibling_reading_coexistence,
    'Can the exclusive-inspiration reading and the revisable-translation reading coexist within a single theological framework, or does accepting one''s core premise require rejecting the other''s?',
    'Theological survey of scholars and denominations that hold both: can they coherently maintain that the KJV is (a) divinely inspired and (b) improvable by manuscript evidence? Or does accepting one entail epistemic compartmentalization that effectively functions as rejection?',
    'If coexistence is possible, the readings truly coexist and do not foreclose. If coexistence requires logical compartmentalization or implicit concession, the exclusive-inspiration reading is more foreclosed than the ''coexists_with'' classification suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether inspiration doctrine and textual-critical revisability are compatible within a single coherent worldview.').

omega_variable(
    publishing_extraction_boundary,
    'How much of the measured extractiveness (0.38 to modern translation publishers) is legitimate profit from providing better service (clarity, accuracy) versus monopolistic rent-seeking through copyright and translation-selection gatekeeping?',
    'Economic analysis of translation publishing margins, barriers to entry for competing translations, and price-elasticity of demand. Comparison to other knowledge-service industries (educational publishing, academic journal pricing).',
    'If extraction is mostly legitimate service profit, the reading operates as rope. If substantial rent-seeking exists (e.g., copyrighting the same content across multiple proprietary translations, bundling with Bible software to create switching costs), the reading operates with higher extractiveness than authored, closer to snare-territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publishing_extraction_boundary, empirical, 'Whether publishing extractiveness reflects service value or monopolistic control of translation property.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__revisable_translation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t0, kjv_text_1611__revisable_translation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(kjv__tr_t0, observed).
narrative_ontology:measurement(kjv__tr_t5, kjv_text_1611__revisable_translation_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement_basis(kjv__tr_t5, observed).
narrative_ontology:measurement(kjv__tr_t10, kjv_text_1611__revisable_translation_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(kjv__tr_t10, observed).
narrative_ontology:measurement(kjv__tr_t15, kjv_text_1611__revisable_translation_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement_basis(kjv__tr_t15, observed).
narrative_ontology:measurement(kjv__tr_t20, kjv_text_1611__revisable_translation_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(kjv__tr_t20, observed).
narrative_ontology:measurement(kjv__tr_t25, kjv_text_1611__revisable_translation_reading, theater_ratio, 25, 0.17).
narrative_ontology:measurement_basis(kjv__tr_t25, observed).
narrative_ontology:measurement(kjv__tr_t30, kjv_text_1611__revisable_translation_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(kjv__tr_t30, observed).
narrative_ontology:measurement(kjv__tr_t40, kjv_text_1611__revisable_translation_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(kjv__tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(kjv__be_t0, kjv_text_1611__revisable_translation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(kjv__be_t0, observed).
narrative_ontology:measurement(kjv__be_t5, kjv_text_1611__revisable_translation_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement_basis(kjv__be_t5, observed).
narrative_ontology:measurement(kjv__be_t10, kjv_text_1611__revisable_translation_reading, base_extractiveness, 10, 0.27).
narrative_ontology:measurement_basis(kjv__be_t10, observed).
narrative_ontology:measurement(kjv__be_t15, kjv_text_1611__revisable_translation_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement_basis(kjv__be_t15, observed).
narrative_ontology:measurement(kjv__be_t20, kjv_text_1611__revisable_translation_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement_basis(kjv__be_t20, observed).
narrative_ontology:measurement(kjv__be_t25, kjv_text_1611__revisable_translation_reading, base_extractiveness, 25, 0.37).
narrative_ontology:measurement_basis(kjv__be_t25, observed).
narrative_ontology:measurement(kjv__be_t30, kjv_text_1611__revisable_translation_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(kjv__be_t30, observed).
narrative_ontology:measurement(kjv__be_t40, kjv_text_1611__revisable_translation_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(kjv__be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t0, kjv_text_1611__revisable_translation_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(kjv__su_t0, observed).
narrative_ontology:measurement(kjv__su_t5, kjv_text_1611__revisable_translation_reading, suppression_requirement, 5, 0.14).
narrative_ontology:measurement_basis(kjv__su_t5, observed).
narrative_ontology:measurement(kjv__su_t10, kjv_text_1611__revisable_translation_reading, suppression_requirement, 10, 0.16).
narrative_ontology:measurement_basis(kjv__su_t10, observed).
narrative_ontology:measurement(kjv__su_t15, kjv_text_1611__revisable_translation_reading, suppression_requirement, 15, 0.18).
narrative_ontology:measurement_basis(kjv__su_t15, observed).
narrative_ontology:measurement(kjv__su_t20, kjv_text_1611__revisable_translation_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement_basis(kjv__su_t20, observed).
narrative_ontology:measurement(kjv__su_t25, kjv_text_1611__revisable_translation_reading, suppression_requirement, 25, 0.21).
narrative_ontology:measurement_basis(kjv__su_t25, observed).
narrative_ontology:measurement(kjv__su_t30, kjv_text_1611__revisable_translation_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement_basis(kjv__su_t30, observed).
narrative_ontology:measurement(kjv__su_t40, kjv_text_1611__revisable_translation_reading, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(kjv__su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__revisable_translation_reading, information_standard).
narrative_ontology:boltzmann_floor_override(kjv_text_1611__revisable_translation_reading, 0.05).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, kjv_text_1611__exclusive_inspiration_reading).
narrative_ontology:affects_constraint(kjv_text_1611__revisable_translation_reading, kjv_text_1611__functional_equivalence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the KJV kernel. All three are linked by network edges: the revisable-translation reading influences both the exclusive-inspiration reading (by establishing academic standards that implicitly challenge preservation claims) and the functional-equivalence reading (by providing the legitimating framework for modern-translation plurality). The exclusive-inspiration reading and the revisable-translation reading have foreclosure tension: accepting one reading's core premise (the KJV's text-critical status is improvable) requires epistemic commitment that shapes how the other reading's core premise (the KJV is divinely preserved) can be maintained. See commentary.kernel_context for reading-relation details.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
