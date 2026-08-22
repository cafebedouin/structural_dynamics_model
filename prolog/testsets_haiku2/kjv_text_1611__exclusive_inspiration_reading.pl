% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__exclusive_inspiration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__exclusive_inspiration_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: kjv_text_1611__exclusive_inspiration_reading
 *   human_readable: KJV Exclusive Inspiration Doctrine and Textual Gate-Keeping
 *   domain: religious/theological
 *
 * SUMMARY:
 *   The KJV-exclusive-inspiration reading asserts that the 1611 King James
 *   Bible is the uniquely and verbally inspired English scripture, while all
 *   modern translations are corrupted, inferior, or Satanically compromised.
 *   This reading emerged as a crystallized doctrine in American
 *   fundamentalism (early 20th century) and persists in a subset of
 *   independent Baptist churches, Pentecostal congregations, and separatist
 *   Christian communities. The constraint operates by: (1) establishing the
 *   KJV as the sole legitimate textual authority, (2) delegitimizing modern
 *   translations and their scholarly proponents as false learning, (3)
 *   enforcing cognitive boundary-maintenance through narrative framing (e.g.,
 *   'preservation' narratives, conspiracy theories about manuscript
 *   suppression), and (4) extracting institutional loyalty and doctrinal
 *   control from lay believers who accept the framing. The core tension is
 *   that the founding problem—ensuring a reliable, authoritative English
 *   Bible—is presented as solved by freezing at 1611, while the empirical
 *   evidence (older Greek manuscripts, linguistic evolution, translation
 *   methodology) continuously undermines the exclusivity claim and requires
 *   ever-more-active enforcement to suppress.
 *
 * KEY AGENTS:
 *   - KJV-Only institutional leadership (agenda-setter, derives authority and membership control from exclusivity doctrine)
 *   - Modern translation scholars and committees (powerful but excluded from authority; bear reputational cost and market suppression)
 *   - Linguistic scholars and textual critics (moderate power; suppressed as purveyors of false learning within KJV-Only spaces)
 *   - Lay believers in KJV-Only congregations (powerless, identity-locked, suppressed access to comprehensible alternatives)
 *   - Mainstream Christian institutions and evangelical scholars (observers, testify to the constraint's effects from outside)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, 0.68).
domain_priors:suppression_score(kjv_text_1611__exclusive_inspiration_reading, 0.72).
domain_priors:theater_ratio(kjv_text_1611__exclusive_inspiration_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__exclusive_inspiration_reading, tangled_rope).
narrative_ontology:human_readable(kjv_text_1611__exclusive_inspiration_reading, "KJV Exclusive Inspiration Doctrine and Textual Gate-Keeping").
narrative_ontology:topic_domain(kjv_text_1611__exclusive_inspiration_reading, "religious/theological").

domain_priors:requires_active_enforcement(kjv_text_1611__exclusive_inspiration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__exclusive_inspiration_reading, '93325397-8b63-4cfc-92ba-079c6bf37b7f').
narrative_ontology:cs_kernel_codification('93325397-8b63-4cfc-92ba-079c6bf37b7f', fixed_text).
narrative_ontology:cs_authority_grounding('93325397-8b63-4cfc-92ba-079c6bf37b7f', extraction).
narrative_ontology:cs_interpretation_layer_present('93325397-8b63-4cfc-92ba-079c6bf37b7f').
narrative_ontology:cs_reading_relation('93325397-8b63-4cfc-92ba-079c6bf37b7f', kjv_text_1611__functional_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('93325397-8b63-4cfc-92ba-079c6bf37b7f', kjv_text_1611__revisable_translation_reading, coexists_with).
narrative_ontology:cs_axiom('93325397-8b63-4cfc-92ba-079c6bf37b7f', foundational, kjv_verbally_inspired_english_scripture).
narrative_ontology:cs_axiom_status(kjv_verbally_inspired_english_scripture, holdable).
narrative_ontology:cs_axiom_grounding('93325397-8b63-4cfc-92ba-079c6bf37b7f', kjv_verbally_inspired_english_scripture, theological).
narrative_ontology:cs_axiom('93325397-8b63-4cfc-92ba-079c6bf37b7f', foundational, modern_translations_corrupted_or_demonic).
narrative_ontology:cs_axiom_status(modern_translations_corrupted_or_demonic, holdable).
narrative_ontology:cs_axiom_grounding('93325397-8b63-4cfc-92ba-079c6bf37b7f', modern_translations_corrupted_or_demonic, theological).
narrative_ontology:cs_reference_frame('93325397-8b63-4cfc-92ba-079c6bf37b7f', providential_preservation_doctrine).
narrative_ontology:cs_drift_state('93325397-8b63-4cfc-92ba-079c6bf37b7f', contemporary_textual_criticism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('93325397-8b63-4cfc-92ba-079c6bf37b7f', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_institutional_leadership).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, modern_translation_advocates).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, linguistic_scholars).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, lay_believers_seeking_clarity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, lay_believers_seeking_clarity).
narrative_ontology:constraint_vindicates(kjv_text_1611__exclusive_inspiration_reading, textual_purity_doctrine).
narrative_ontology:constraint_vindicates(kjv_text_1611__exclusive_inspiration_reading, english_language_supremacy_in_gospel_transmission).
narrative_ontology:constraint_vindicates(kjv_text_1611__exclusive_inspiration_reading, verbal_inspiration_absolute_form).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the interpretive standard for which English texts count as scripture. Controls the narrative of textual authority within KJV-Only denominations and churches. Justifies the exclusivity claim through appeals to providential preservation, genealogical purity of manuscripts, and the authority of the 1611 translators. Derives institutional legitimacy, membership loyalty, and doctrinal control from the exclusive-inspiration framing. Exit would require abandoning the founding premise of their institutional identity.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_institutional_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Linguistic scholars, translation organizations (NIV, ESV, NRSV committees), and mainstream evangelical leaders who argue for using older manuscripts, applying modern linguistic knowledge, and producing translations optimized for contemporary comprehension. Their translations are declared corrupted or inferior by the KJV-Only reading, which delegitimizes their scholarly labor and excludes their products from institutional settings controlled by KJV-Only leadership. They bear reputational cost and market exclusion; exit requires ceasing translation work or accepting inferiority status.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, modern_translation_advocates, payer,
    powerful, biographical, constrained, global).

% Academic specialists in Greek, Hebrew, textual criticism, and historical linguistics whose work contradicts the exclusive-inspiration claim. They document manuscript variation, translation choices, and linguistic evolution. Within KJV-Only institutional contexts, their scholarship is suppressed as false learning or demonic deception. Their methods are excluded from authoritative biblical interpretation. Career advancement in KJV-Only institutions requires abandoning methodological standards or departing.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, linguistic_scholars, payer,
    moderate, biographical, constrained, national).

% Church members in KJV-Only congregations who find the 1611 language archaic, confusing, or limiting for personal study and comprehension. They experience cognitive dissonance: told the KJV is the only true scripture while finding it harder to understand than modern versions. Alternatives are presented as spiritually dangerous. Exit requires leaving the church community, which for identity-embedded believers means profound relational and spiritual rupture. They absorb the cost of constrained access to comprehensible scripture.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, lay_believers_seeking_clarity, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__exclusive_inspiration_reading, lay_believers_seeking_clarity, beneficiary).

% The Greek and Hebrew texts older than 1611, including the Dead Sea Scrolls and earlier uncial manuscripts, contain readings that differ from the KJV's source texts (the Textus Receptus and Byzantine manuscripts). These older sources are excluded from authority by the exclusive-inspiration reading, despite their historical priority and scholarly authentication. Non-agent entry kept for narrative completeness.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, early_manuscript_evidence, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kjv_text_1611__exclusive_inspiration_reading, early_manuscript_evidence).

% Catholic, Orthodox, mainline Protestant, and evangelical denominations that authorize multiple English translations and integrate modern textual scholarship into Scripture interpretation. They witness the exclusive-inspiration constraint's operation from outside KJV-Only institutions and can document its reputational and institutional effects.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, institutional_mainstream_christianity, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kjv_text_1611__exclusive_inspiration_reading, kjv_only_institutional_leadership).
narrative_ontology:fixing_cost_class(kjv_text_1611__exclusive_inspiration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable textual standard for English-language biblical worship, preaching, and teaching within KJV-Only churches: one version eliminates translation disputes, standardizes liturgical language, and creates interpretive coherence across congregations holding the 1611 text as authoritative.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual believers, scholars, and translation committees to KJV-Only institutional leadership, which alone adjudicates what counts as true scripture in English. Modern translations and their proponents are demoted from legitimate sources to corrupted alternatives. Believers experience a constrained choice set: use the KJV or use corrupted scripture. Leadership collects institutional loyalty, intellectual authority, and doctrinal control.
% ABSENT_VOICES: Textual scholars (including conservative evangelical scholars) who argue for using older manuscripts would be excluded from the conversation were they present; they attest the KJV-Only position contradicts the documentary evidence. Lay believers confused by archaic language would object that clarity is suppressed as satanic compromise. Sibling-reading proponents (functional-equivalence and revisable-translation advocates) are structurally excluded from having a say within KJV-Only institutional spaces.
% DISAPPEARANCE_RATIONALE: If the exclusive-inspiration constraint disappeared overnight, lay believers would immediately adopt modern translations for personal study; institutional leadership would lose doctrinal gatekeeping power; translation organizations would gain market access to KJV-Only congregations; and textual scholarship would re-enter the conversation as legitimate. The constraint's removal would reorganize how English-speaking believers access, study, and teach scripture.
% FOUNDING_PROBLEM: In the 16th and 17th centuries, Protestant reform movements and the growth of English-language Bible distribution created jurisdictional questions: which translation was authoritative? Which preserved the original meaning against medieval Catholic corruption narratives? The KJV translators positioned themselves as restoring Reformation principles; subsequent KJV-Only tradition extended this into a claim of exclusive inspiration.
% FOUNDING_PROBLEM_CORROBORATION: KJV-Only leadership attests the founding problem (distinguishing true from corrupted Scripture) is still live and requires exclusive adherence to 1611. Textual scholars and modern-translation committees attest the founding problem has been addressed through comparative manuscript study and no longer justifies the exclusive-inspiration claim. Mainstream Christian institutions and independent evangelical scholars testify that the original founding problem (securing a reliable English text) is solved through multiple competent, peer-reviewed translations. Documentary evidence—the discovery of earlier Greek manuscripts after 1611, and scholarly consensus that the KJV's source texts (the Byzantine/Textus Receptus line) are later recensions—corroborates the revisable-translation reading that the founding problem calls for ongoing improvement, not frozen exclusivity.
narrative_ontology:disappearance_verdict(kjv_text_1611__exclusive_inspiration_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__exclusive_inspiration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__exclusive_inspiration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kjv_text_1611__exclusive_inspiration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__exclusive_inspiration_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint directly transfers hermeneutical authority from believers and scholars to institutional gatekeepers, restricts textual choice, and demotes modern scholarship as illegitimate—extraction is the transfer of interpretive power coupled with delegitimation of competitors. Suppression is equally high (0.72) because the constraint requires active enforcement: believers must be convinced that other translations are corrupted, scholars must be excluded from platforms within KJV-Only spaces, and evidence contradicting the claim (manuscript priority, linguistic research) must be reframed as Satanic deception. The suppression series rises sharply from t=0 to t=24 (corresponding historically to the mid-20th century hardening of the doctrine and the emergence of modern translations as a coherent market threat) and plateaus at t=32+, indicating the constraint reached a stable enforcement equilibrium in communities where the doctrine took institutional root. Theater ratio is moderate (0.41 at interval end) because the constraint does execute a real coordination function (unified textual standard for worship) but increasingly that function is dwarfed by the enforcement machinery defending exclusivity against modern translation market pressure. The measurement series runs on one time grid; every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the KJV-Only leader's seat, the constraint is genuine coordination (providing textual stability, preserving 'pure' scripture, maintaining institutional continuity) and any resistance is satanic deception. From the modern-translation scholar's seat, the constraint is pure extraction: the coordination story is a cover narrative, and the actual enforcement machinery exists solely to exclude competitors and suppress scholarship. From the lay believer's seat in a KJV-Only church, the constraint oscillates: it provides the coordination benefit of unified liturgical language and community interpretive coherence, but over time the suppression cost (restricted access, cognitive dissonance) accumulates. The engine computes these perspectives from the structural data: leadership's high d favors a beneficiary classification, scholars' high d favors a target classification, and lay believers' symmetric-but-constrained d produces the mixed picture. The perspectival gap is not a flaw—it is the signal the framework exists to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   KJV-Only institutional leadership is the beneficiary: they collect institutional authority, doctrinal gatekeeping power, membership loyalty, and identity-fusion with the doctrine itself (career path, institutional status, and self-concept are bound to the KJV-Only framing). Their directionality d is near 0.0 (full beneficiary). Modern translation advocates and linguistic scholars are structural targets: they bear suppression, market exclusion, reputational costs, and exclusion from institutional spaces where their work would be valued. Their d is high (0.75+, near target-end). Lay believers are dual-positioned: they receive coordination benefit (a unified textual standard for church worship) but bear a high cost (constrained access to comprehensible scripture, cognitive pressure to believe the KJV-Only narrative despite experience). Their d is near 0.55 (symmetric, costs ≈ benefits, but skewed toward burden because the coordination benefit is passive while the suppressive cost is active). The constraint's persistence depends on identity-locking lay believers into the institutional frame: the suppression of alternative translations is structurally fused with the believer's sense of what it means to be faithful. Exiting the constraint means exiting the faith community for identity-embedded believers.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits the mandatrophy signature: the founding problem (securing a reliable, authoritative English Bible in the 16th-17th centuries) has been functionally solved through modern textual scholarship and multiple competent translations, yet the institutional arrangement persists and has hardened into doctrinal exclusivity. The measured theater_ratio plateau (0.41 from t=40 onward) indicates that active enforcement is increasingly theatrical—the constraint's persistence is maintained by narrative gatekeeping and identity-fusion rather than by solving the problem it was built to address. The extractiveness plateau (0.68 from t=32 onward) reflects the constraint reaching an asymptotic enforcement equilibrium: increasing suppression effort yields diminishing returns in delegitimating modern translations, because the empirical evidence gap keeps widening. This is a classic Piton or late-stage Tangled Rope signature: a function that has outlived its problem but persists through institutional and identity mechanisms. The constraint classification remains Tangled Rope rather than Piton because the coordination function is still materially valuable (unified textual standard, liturgical coherence) and the extraction is still active (not yet degraded into pure theater). But the mandatrophy signal is clear: institutional leadership continues defending exclusivity not primarily to solve the founding problem (which no longer requires it) but to preserve its gatekeeping authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of modern translations and linguistic scholarship structural (external barriers preventing access and distribution within KJV-Only institutions) or internalized (lay believers accept the KJV-Only narrative as spiritually true and resist alternatives cognitively)?',
    'Post-exit trajectory analysis: if lay believers who leave KJV-Only congregations continue to prefer the KJV after institutional pressure is removed, suppression is partly internalized; if they quickly adopt modern translations upon exit, suppression was primarily structural. Longitudinal studies of ex-fundamentalist belief trajectories provide evidence.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests—the target carries the suppression internally after institutional exit. Classification implications: stronger Snare signature. If structural, remedies focus on institutional reform. If mixed, both institutional change and identity-reconstruction support are needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the KJV-Only reading').

omega_variable(
    textual_authority_grounding_ambiguity,
    'Does the exclusive-inspiration reading''s claim to authority rest fundamentally on theological premises (God''s providential preservation of the KJV is a faith commitment independent of evidence) or empirical premises (the KJV''s source texts are actually older and more reliable than modern scholarship claims)?',
    'Comparative analysis of KJV-Only apologetic literature: if theological arguments predominate and empirical claims are secondary, the reading is primarily deontological (grounded in faith/duty). If empirical claims of manuscript priority are central to the authority argument, the reading is empirically_contingent. Documentary analysis of KJV-Only organizational teaching materials and scholarly critiques reveal the grounding structure.',
    'If theological, the reading is not falsifiable by empirical evidence (older manuscripts, linguistic research) and persists as a preference structure independent of data. If empirical, the reading is vulnerable to evidence and its persistence despite contrary evidence indicates suppression of falsifying data. Affects classification: a theological reading is more robust against Mandatrophy reclassification; an empirical reading whose grounds have been undermined is a Mandatrophy candidate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_authority_grounding_ambiguity, conceptual, 'Whether the KJV-exclusive-inspiration reading grounds its authority in faith or in empirical claims about manuscript evidence').

omega_variable(
    identity_lock_mechanism,
    'For lay believers in KJV-Only congregations, is the attachment to the exclusive-inspiration reading a professional-identity lock (career path tied to the doctrine, leadership role dependent on adherence), a relational-identity lock (self-concept constituted through the faith community and its interpretation), an ideological lock (the reading is woven into a coherent worldview that makes alternatives unthinkable), or some combination?',
    'Qualitative analysis of testimonies from people who exit KJV-Only communities, interviews examining identity rupture and reconstruction, and organizational ethnography documenting how the doctrine is taught to children and new members.',
    'Professional-identity locks can shift if career incentives change (institutional employment loses monopoly on credentials). Relational locks require community restructuring (finding new faith communities or secular groups with similar social functions). Ideological locks require worldview reconstruction (often the slowest transition). If the lock is primarily relational, institutional schism or alternative community formation can weaken the constraint; if primarily ideological, the constraint persists even across institutional changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'The mechanism binding lay believers'' identity to the KJV-exclusive-inspiration reading').

omega_variable(
    kernel_reading_contest_structure,
    'Are the three sibling readings of the kjv_text_1611 kernel genuinely coexisting positions held by different Christian constituencies, or does the exclusive-inspiration reading attempt to foreclose its siblings through institutional and narrative suppression?',
    'Documentary analysis: if KJV-Only leadership actively delegitimizes the functional-equivalence and revisable-translation readings as false (Satan-inspired alternatives), the relationships are not symmetric coexistence but asymmetric foreclosure attempts by the exclusive reading. If the siblings persist despite suppression (as they do in mainstream evangelical and academic communities), the coexistence is maintained despite the foreclosure attempt.',
    'If foreclosure, the exclusive-inspiration reading would be reclassified from Tangled Rope toward Snare (pure extraction under a coordination cover story). If coexistence, the classification remains Tangled Rope (asymmetric extraction riding on genuine coordination). The engine computes reading_relations from axiom_contradiction; empirical suppression dynamics feed the Mandatrophy and theater_ratio analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, empirical, 'Whether the exclusive-inspiration reading genuinely coexists with or actively forecloses its sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__exclusive_inspiration_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t0, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(kjv__tr_t0, observed).
narrative_ontology:measurement(kjv__tr_t8, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement_basis(kjv__tr_t8, observed).
narrative_ontology:measurement(kjv__tr_t16, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement_basis(kjv__tr_t16, observed).
narrative_ontology:measurement(kjv__tr_t24, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement_basis(kjv__tr_t24, observed).
narrative_ontology:measurement(kjv__tr_t32, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement_basis(kjv__tr_t32, observed).
narrative_ontology:measurement(kjv__tr_t40, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(kjv__tr_t40, observed).
narrative_ontology:measurement(kjv__tr_t50, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(kjv__tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(kjv__be_t0, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(kjv__be_t0, observed).
narrative_ontology:measurement(kjv__be_t8, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(kjv__be_t8, observed).
narrative_ontology:measurement(kjv__be_t16, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement_basis(kjv__be_t16, observed).
narrative_ontology:measurement(kjv__be_t24, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(kjv__be_t24, observed).
narrative_ontology:measurement(kjv__be_t32, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement_basis(kjv__be_t32, observed).
narrative_ontology:measurement(kjv__be_t40, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(kjv__be_t40, observed).
narrative_ontology:measurement(kjv__be_t50, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(kjv__be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t0, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(kjv__su_t0, observed).
narrative_ontology:measurement(kjv__su_t8, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement_basis(kjv__su_t8, observed).
narrative_ontology:measurement(kjv__su_t16, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement_basis(kjv__su_t16, observed).
narrative_ontology:measurement(kjv__su_t24, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement_basis(kjv__su_t24, observed).
narrative_ontology:measurement(kjv__su_t32, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 32, 0.69).
narrative_ontology:measurement_basis(kjv__su_t32, observed).
narrative_ontology:measurement(kjv__su_t40, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(kjv__su_t40, observed).
narrative_ontology:measurement(kjv__su_t50, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(kjv__su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__exclusive_inspiration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(kjv_text_1611__exclusive_inspiration_reading, 0.12).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__functional_equivalence_reading).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__revisable_translation_reading).

% DUAL FORMULATION NOTE:
% The kjv_text_1611 kernel decomposes into three structurally distinct constraints, each representing a different reading of the 1611 text's interpretive authority. The exclusive_inspiration_reading (this story) asserts the KJV is uniquely inspired and inerrant in English; the functional_equivalence_reading treats the KJV as one valuable translation among several complementary versions; the revisable_translation_reading treats the KJV as a historically important but improvable translation grounded in later manuscripts. Each reading has a distinct ε (extractiveness), victim/beneficiary structure, and classification. The exclusive_inspiration_reading shows high ε (0.68) because it transfers hermeneutical authority to institutional gatekeepers and suppresses alternatives as corrupted. The functional_equivalence and revisable_translation readings show lower ε (coordination-dominant or expertise-driven classification) because they distribute authority across multiple sources and integrate scholarly evidence. The three readings coexist as live positions held by different Christian communities but are linked through institutional and narrative suppression dynamics tracked in network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kjv_text_1611__exclusive_inspiration_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
