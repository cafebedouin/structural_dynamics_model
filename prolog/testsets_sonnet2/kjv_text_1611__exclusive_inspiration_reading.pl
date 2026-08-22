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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: KJV-Only Exclusive Inspiration Doctrine
 *   domain: religious/textual/theological
 *
 * SUMMARY:
 *   This story instantiates the exclusive_inspiration_reading of the KJV
 *   kernel: the claim that the 1611 King James translation is not merely
 *   historically significant but is the exclusively inspired, inerrant word
 *   of God in English, such that all other translations are corrupted or
 *   theologically compromised by definition. This reading emerged from
 *   mid-20th-century figures (building on earlier Reformation-era textual
 *   preferences) into an institutionalized movement with its own publishing
 *   infrastructure, seminaries, and pastoral networks. The doctrine functions
 *   as a genuine coordination device for in-group textual unity while
 *   simultaneously gate-keeping who counts as a legitimate believer, teacher,
 *   or translation, extracting social and financial rents from those who
 *   comply and imposing costs on those who use or produce alternative
 *   translations. This is ONE of three linked readings of the same kernel
 *   (kjv_text_1611); the functional_equivalence_reading and
 *   revisable_translation_reading are separate constraint files with their
 *   own ε values, beneficiary/victim structures, and classifications — they
 *   are not alternate measurements of this constraint but structurally
 *   distinct constraints sharing a textual-historical object.
 *
 * KEY AGENTS:
 *   - kjv_only_leadership: agenda_setter (institutional/arbitrage) — defines and enforces the doctrine, insulated from its costs
 *   - kjv_only_publishing_ministries: beneficiary (organized/arbitrage) — revenue depends on the doctrine's persistence
 *   - modern_translation_readers: payer (powerless/constrained) — bear reputational and fellowship costs
 *   - congregants_under_kjv_only_pastors: payer (powerless/trapped) — embedded through social and familial ties
 *   - textual_scholars_outside_movement: payer/excluded (moderate/mobile) — expertise delegitimized wholesale
 *   - manuscript_evidence: non-agent excluded seat — the evidentiary record is not treated as a live input
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, 0.71).
domain_priors:suppression_score(kjv_text_1611__exclusive_inspiration_reading, 0.78).
domain_priors:theater_ratio(kjv_text_1611__exclusive_inspiration_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__exclusive_inspiration_reading, tangled_rope).
narrative_ontology:human_readable(kjv_text_1611__exclusive_inspiration_reading, "KJV-Only Exclusive Inspiration Doctrine").
narrative_ontology:topic_domain(kjv_text_1611__exclusive_inspiration_reading, "religious/textual/theological").

domain_priors:requires_active_enforcement(kjv_text_1611__exclusive_inspiration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__exclusive_inspiration_reading, '6f7c0118-1a94-4b6e-b065-e835bcfd658f').
narrative_ontology:cs_kernel_codification('6f7c0118-1a94-4b6e-b065-e835bcfd658f', fixed_text).
narrative_ontology:cs_authority_grounding('6f7c0118-1a94-4b6e-b065-e835bcfd658f', lineage).
narrative_ontology:cs_interpretation_layer_present('6f7c0118-1a94-4b6e-b065-e835bcfd658f').
narrative_ontology:cs_reading_relation('6f7c0118-1a94-4b6e-b065-e835bcfd658f', kjv_text_1611__revisable_translation_reading, forecloses).
narrative_ontology:cs_reading_relation('6f7c0118-1a94-4b6e-b065-e835bcfd658f', kjv_text_1611__functional_equivalence_reading, forecloses).
narrative_ontology:cs_axiom('6f7c0118-1a94-4b6e-b065-e835bcfd658f', foundational, single_translation_exclusive_inspiration).
narrative_ontology:cs_axiom_status(single_translation_exclusive_inspiration, holdable).
narrative_ontology:cs_axiom_grounding('6f7c0118-1a94-4b6e-b065-e835bcfd658f', single_translation_exclusive_inspiration, theological).
narrative_ontology:cs_axiom('6f7c0118-1a94-4b6e-b065-e835bcfd658f', foundational, textus_receptus_manuscript_priority).
narrative_ontology:cs_axiom_status(textus_receptus_manuscript_priority, holdable).
narrative_ontology:cs_axiom_grounding('6f7c0118-1a94-4b6e-b065-e835bcfd658f', textus_receptus_manuscript_priority, empirically_contingent).
narrative_ontology:cs_axiom('6f7c0118-1a94-4b6e-b065-e835bcfd658f', secondary, modern_translations_theologically_corrupted).
narrative_ontology:cs_axiom_status(modern_translations_theologically_corrupted, holdable).
narrative_ontology:cs_axiom_grounding('6f7c0118-1a94-4b6e-b065-e835bcfd658f', modern_translations_theologically_corrupted, theological).
narrative_ontology:cs_reference_frame('6f7c0118-1a94-4b6e-b065-e835bcfd658f', verbal_plenary_preservation_in_english).
narrative_ontology:cs_drift_state('6f7c0118-1a94-4b6e-b065-e835bcfd658f', post_critical_text_scholarship_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6f7c0118-1a94-4b6e-b065-e835bcfd658f', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership).
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_publishing_ministries).
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_seminaries).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, modern_translation_readers).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, congregants_under_kjv_only_pastors).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, textual_scholars_outside_movement).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, non_english_speaking_converts).
narrative_ontology:constraint_vindicates(kjv_text_1611__exclusive_inspiration_reading, textus_receptus_priority_doctrine).
narrative_ontology:constraint_vindicates(kjv_text_1611__exclusive_inspiration_reading, verbal_plenary_preservation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pastors, denominational leaders, and movement founders who define and enforce the doctrine that only the KJV is inspired scripture in English. They train successors, control ordination and pulpit access within their networks, and adjudicate who counts as sound. They personally hold institutional positions insulated from the doctrine's costs and gain authority, donor loyalty, and institutional control from being the sole interpreters of what counts as legitimate scripture.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership, beneficiary).

% Publishing houses, bookstores, and conference circuits that sell KJV-only apologetics, study materials, and 'exposes' of modern translations. Revenue depends on maintaining the doctrine's plausibility and the ongoing sense of textual threat from modern versions.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_publishing_ministries, beneficiary,
    organized, generational, arbitrage, national).

% Bible colleges and training institutes whose curricula, faculty credentials, and graduate placement pipelines are built entirely around the exclusive-inspiration position. Their institutional survival is tied to producing pastors who will propagate the doctrine to new congregations.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_seminaries, beneficiary,
    organized, generational, constrained, national).

% Lay believers who use NIV, ESV, NASB, or other translations and are told by KJV-only teaching that they are reading a corrupted, Satan-influenced, or watered-down text. They bear reputational and spiritual-standing costs within KJV-only congregations and networks, sometimes facing exclusion from teaching roles, marriage within the community, or full fellowship.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, modern_translation_readers, payer,
    powerless, biographical, constrained, national).

% Members of churches where the pastor has made KJV-only doctrine a test of orthodoxy. Questioning the doctrine risks social exclusion, loss of standing, or being labeled compromised or apostate. Many are embedded through family, marriage, homeschooling networks, and social ties that make exit costly beyond the theological question itself.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, congregants_under_kjv_only_pastors, payer,
    powerless, biographical, trapped, local).

% Textual critics and biblical scholars whose manuscript-based work (documenting the Alexandrian, Byzantine, and other textual traditions) is dismissed wholesale by the movement as corrupted or demonically influenced, regardless of the scholar's own faith commitments. Their expertise is delegitimized rather than engaged, though they retain mobility outside the movement's institutions.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, textual_scholars_outside_movement, payer,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__exclusive_inspiration_reading, textual_scholars_outside_movement, excluded).

% Believers in missions contexts where KJV-only missionaries insist that translation work must derive from the King James text (via the Textus Receptus) rather than best available manuscripts and target-language linguistics, constraining Bible translation projects and creating theological dependency on an English-language artifact.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, non_english_speaking_converts, payer,
    powerless, biographical, trapped, global).

% The actual body of Greek and Hebrew manuscript witnesses (papyri, uncials, minuscules, versions, patristic citations) that textual criticism weighs. Its evidentiary claims are not addressed on the merits within the movement's framework; the manuscript record is treated as evidence for or against a prior doctrinal commitment rather than a live input.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, manuscript_evidence, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kjv_text_1611__exclusive_inspiration_reading, manuscript_evidence).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership).
narrative_ontology:fixing_cost_class(kjv_text_1611__exclusive_inspiration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, memorizable, stable textual reference that generations of English-speaking congregations can cite, quote, and cross-reference without translation disputes disrupting shared worship, memorization programs, and doctrinal continuity within a tradition.
% TRANSFER_FUNCTION: Moves authority to interpret and gatekeep scripture from textual scholarship and denominational plurality toward a closed leadership class; moves social and spiritual legitimacy away from believers who use other translations and toward those who comply; moves revenue toward KJV-only publishers and toward seminaries whose accreditation depends on the doctrine.
% ABSENT_VOICES: Textual critics within evangelicalism who accept manuscript-based scholarship while remaining committed believers are treated as compromised rather than engaged; non-English speakers whose translation needs are subordinated to an English-derived textual chain are rarely consulted; congregants who privately doubt the doctrine but fear social consequences do not speak in the rooms where the doctrine is set.
% DISAPPEARANCE_RATIONALE: If the exclusive-inspiration doctrine vanished overnight, KJV-only publishing houses would lose their core product line, seminaries built around the doctrine would need to reconstruct curricula, congregants using modern translations would no longer face social sanction, and mission translation projects constrained by Textus-Receptus-only requirements would gain access to broader manuscript scholarship. The doctrine's disappearance would visibly reorganize a real institutional ecosystem, not merely a belief.
% FOUNDING_PROBLEM: In the mid-20th century, the doctrine responded to genuine anxieties: a proliferation of new translations (RSV, later NIV and others) some produced by scholars with liberal theological commitments, textual-critical methods (especially reliance on Alexandrian-family manuscripts like Codex Sinaiticus and Vaticanus) that were unfamiliar to lay readers, and a felt loss of a single shared scriptural text across English-speaking Protestantism.
% FOUNDING_PROBLEM_CORROBORATION: KJV-only leadership attests the problem remains live, citing ongoing textual-critical disputes and theological drift in some modern translations. Textual scholars outside the movement — including conservative evangelical textual critics such as those affiliated with mainstream evangelical seminaries and the broader field of New Testament textual criticism — attest that the manuscript evidence has only strengthened since 1611 (older and more numerous witnesses now available) and that the doctrine's persistence reflects institutional lock-in rather than an unresolved textual crisis. No corroboration exists from outside the beneficiary set for the specific claim that the KJV alone is uniquely inspired; that claim is attested only within the movement itself.
narrative_ontology:disappearance_verdict(kjv_text_1611__exclusive_inspiration_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__exclusive_inspiration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__exclusive_inspiration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kjv_text_1611__exclusive_inspiration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__exclusive_inspiration_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored at 0.71 because the doctrine converts textual preference into a hard boundary condition for fellowship, ordination, and legitimacy, with real financial flows to publishers and seminaries whose product is the doctrine itself. Suppression is authored higher (0.78) because the doctrine's persistence depends on active social enforcement — shunning, disqualification from teaching, family and marital pressure — not merely on unforced preference. Theater ratio (0.42) reflects that some of the movement's apologetic activity (textual-critical argument, manuscript comparison) is a genuine attempt at scholarly engagement even though the underlying method inverts the evidentiary relationship (conclusion fixed, evidence selected to fit); a meaningful share of activity is performative boundary-maintenance rather than substantive inquiry. Accessibility collapse (0.58) and resistance (0.62) reflect that alternatives (other translations, textual-critical scholarship) are visibly available and used by billions of believers worldwide, and the doctrine faces active internal and external resistance — this is not a case of alternatives having vanished, but of alternatives being actively suppressed within a bounded institutional network.
 *
 * DIRECTIONALITY LOGIC:
 *   KJV-only leadership and the publishing/seminary infrastructure sit near the full-beneficiary end: they set the terms, collect authority and revenue, and are insulated from the doctrine's social costs (their own compliance is costless because they define compliance). Modern translation readers and congregants under KJV-only pastors sit near the full-target end: they bear real costs (exclusion, suspicion, loss of standing) for behavior (reading a modern translation) that carries no cost outside the movement's boundary, and their exit options range from constrained to trapped depending on how embedded they are in the movement's social fabric. Textual scholars outside the movement are targets of delegitimization but retain mobility since their institutional life is not dependent on the movement's approval — this differentiates them from congregants whose whole social world is inside it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (translation proliferation and unfamiliar textual-critical methods disorienting mid-century congregations) was arguably live in the 1950s-60s. Tracking the founding_problem_status as contested and disappearance_verdict as world_rearranges together flag a mandatrophy-adjacent pattern: the institutional infrastructure (seminaries, publishers, ordination gatekeeping) has outlived and outgrown the narrower problem it responded to, and now persists in significant part because dismantling it would cost the beneficiary institutions their reason for existing — not because the manuscript question remains genuinely unsettled among specialists. The tangled_rope classification (rather than pure snare) is warranted because the coordination function — a shared, stable, memorizable liturgical and catechetical text — is real and valuable to a religious community, distinct from the additional, non-necessary claim of exclusive divine inspiration and inerrancy layered on top of it that generates the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inspiration_claim_theological_vs_institutional,
    'Is the exclusive-inspiration claim a genuine theological conviction independently arrived at, or is it substantially a function of the institutional and financial infrastructure (seminaries, publishers, ordination networks) that depends on the claim''s continued plausibility?',
    'Compare doctrinal commitment and institutional financial dependency across KJV-preferring believers who are NOT embedded in KJV-only institutional networks (e.g., traditionalist individuals with no seminary or publishing ties) versus those whose livelihood depends on the doctrine. If preference-without-institutional-stake correlates with markedly lower exclusivity claims (KJV-preferred vs. KJV-only-exclusive), institutional capture is implicated.',
    'If substantially institutional, the tangled_rope classification is reinforced (genuine devotional/coordination preference exists, but the exclusivity layer is extraction-shaped). If substantially independent theological conviction held broadly regardless of institutional stake, the extraction reading weakens toward a more good-faith contested-belief structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inspiration_claim_theological_vs_institutional, empirical, 'Whether exclusivity claim tracks institutional incentive or independent conviction').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one of three readings of the kjv_text_1611 kernel (exclusive_inspiration_reading, functional_equivalence_reading, revisable_translation_reading). Where exactly does the disagreement among readings locate itself structurally?',
    'The disagreement is located at a single structural point: whether textual variation among manuscript traditions and translation choices is (a) theologically disqualifying (this reading), (b) irrelevant to functional adequacy (functional_equivalence_reading), or (c) grounds for ongoing scholarly revision (revisable_translation_reading). All three readings can examine the identical manuscript evidence and liturgical history; they diverge entirely on whether textual variation is compatible with a doctrine of verbal inspiration for a single translation.',
    'Because the readings diverge on doctrine rather than on contested facts about manuscripts, no amount of additional textual-critical evidence resolves the disagreement between readings — it is a live theological and institutional dispute, not an empirical one, even though this reading''s proponents frame it as settled by evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Locates the inter-reading disagreement at the doctrine-of-inspiration point, not at contested manuscript facts').

omega_variable(
    suppression_internalization_ambiguity,
    'For congregants raised inside KJV-only communities, is the suppression that keeps them from using or trusting modern translations structural (social sanction, exclusion risk) or internalized (a formed belief that modern translations are spiritually dangerous, persisting even absent social pressure)?',
    'Post-exit trajectory: track whether individuals who leave KJV-only congregations (removing structural sanction) continue to avoid modern translations out of genuine conviction, versus adopting them readily once social cost is removed.',
    'If suppression is substantially internalized, effective suppression persists well after structural exit, meaning the constraint''s true suppressive reach is larger than institutional-membership counts suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Structural vs internalized suppression mechanism for exiting congregants').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__exclusive_inspiration_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t0, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(kjv__tr_t12, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(kjv__tr_t24, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 24, 0.31).
narrative_ontology:measurement(kjv__tr_t36, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 36, 0.35).
narrative_ontology:measurement(kjv__tr_t48, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 48, 0.38).
narrative_ontology:measurement(kjv__tr_t60, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement(kjv__tr_t70, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 70, 0.42).

% Extraction over time
narrative_ontology:measurement(kjv__be_t0, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(kjv__be_t12, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(kjv__be_t24, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(kjv__be_t36, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 36, 0.62).
narrative_ontology:measurement(kjv__be_t48, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 48, 0.67).
narrative_ontology:measurement(kjv__be_t60, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(kjv__be_t70, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 70, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t0, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(kjv__su_t12, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(kjv__su_t24, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement(kjv__su_t36, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 36, 0.69).
narrative_ontology:measurement(kjv__su_t48, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 48, 0.73).
narrative_ontology:measurement(kjv__su_t60, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 60, 0.76).
narrative_ontology:measurement(kjv__su_t70, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 70, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__exclusive_inspiration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(kjv_text_1611__exclusive_inspiration_reading, 0.06).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, functional_equivalence_reading).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, revisable_translation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the kjv_text_1611 kernel. exclusive_inspiration_reading (this file) authors high extraction (0.71) driven by a named victim set (modern translation readers, congregants, outside scholars, non-English converts) and tangled_rope classification. functional_equivalence_reading is expected to author low extraction and no comparable victim set, since it treats translation plurality as unproblematic. revisable_translation_reading is expected to author moderate extraction directed primarily at revision-resistant institutions rather than lay readers, since it accepts the legitimacy of updating translations against better manuscript evidence. All three share the same textual-historical kernel object (the 1611 KJV and its manuscript basis) but diverge entirely on the doctrine-of-inspiration axis, which is why they are three constraints rather than one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
