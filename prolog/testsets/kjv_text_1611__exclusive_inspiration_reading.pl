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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kjv_text_1611__exclusive_inspiration_reading
 *   human_readable: KJV Exclusive Inspiration and Inerrancy Reading
 *   domain: religious/textual authority/theological
 *
 * SUMMARY:
 *   The KJV exclusive-inspiration reading is one reading of the contested
 *   kernel 'kjv_text_1611'. This reading claims the 1611 King James Version
 *   is the uniquely inspired, inerrant English Bible and that all other
 *   translations are corruptions or compromises. Under this reading, KJV-Only
 *   institutional leaders serve as gatekeepers of legitimate scriptural
 *   authority. Modern translation communities, textual scholars using earlier
 *   manuscripts, and lay believers seeking linguistic clarity are
 *   structurally positioned as compromising biblical purity. The constraint
 *   coordinates a doctrinal community around a single authoritative text
 *   while simultaneously extracting interpretive authority from alternative
 *   translation traditions and from the scholarly discipline of textual
 *   criticism. The expected structural delta from the kernel contest is
 *   present: modern translations enter the victim set (suppressed as
 *   illegitimate); KJV-Only leadership becomes the sole arbiter of textual
 *   authority; and extractiveness is high because gate-keeping of 'true'
 *   scripture blocks intellectual and linguistic alternatives.
 *
 * KEY AGENTS:
 *   - kjv_only_leadership: Organized institutional authority (churches, seminaries, publishing houses) that sets the KJV-exclusive doctrine and enforces it through doctrinal discipline. Their professional identity and authority rests entirely on the KJV's unchallenged status.
 *   - modern_translation_communities: Organized scholars and publishers (NIV, ESV, NRSV communities) producing translations based on earlier manuscripts. Structurally excluded and delegitimized rather than refuted on textual grounds.
 *   - lay_believers_seeking_clarity: Powerless, identity-locked church members who find the KJV's early modern English difficult but are taught that seeking clarity elsewhere is spiritual compromise. They pay in restricted textual access; many also benefit from community cohesion.
 *   - biblical_scholars_using_newer_manuscripts: Academic textual critics whose work with earlier Greek/Hebrew sources contradicts KJV-source-base primacy. Their findings are treated as attacks on scripture rather than incremental knowledge.
 *   - rival_translation_theological_communities: Mainline, Catholic, and Orthodox publishing traditions. Structurally excluded and treated as operating outside legitimate textual authority.
 *   - textual_criticism_as_discipline: Non-agent entry. The scholarly practice of manuscript comparison and historical-critical reconstruction is framed as epistemically corrupt when applied to scripture under this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, 0.68).
domain_priors:suppression_score(kjv_text_1611__exclusive_inspiration_reading, 0.76).
domain_priors:theater_ratio(kjv_text_1611__exclusive_inspiration_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__exclusive_inspiration_reading, tangled_rope).
narrative_ontology:human_readable(kjv_text_1611__exclusive_inspiration_reading, "KJV Exclusive Inspiration and Inerrancy Reading").
narrative_ontology:topic_domain(kjv_text_1611__exclusive_inspiration_reading, "religious/textual authority/theological").

domain_priors:requires_active_enforcement(kjv_text_1611__exclusive_inspiration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__exclusive_inspiration_reading, '648051b4-8fb7-48d3-8e05-94734029d174').
narrative_ontology:cs_kernel_codification('648051b4-8fb7-48d3-8e05-94734029d174', fixed_text).
narrative_ontology:cs_authority_grounding('648051b4-8fb7-48d3-8e05-94734029d174', extraction).
narrative_ontology:cs_interpretation_layer_present('648051b4-8fb7-48d3-8e05-94734029d174').
narrative_ontology:cs_reading_relation('648051b4-8fb7-48d3-8e05-94734029d174', kjv_text_1611__functional_equivalence_reading, forecloses).
narrative_ontology:cs_reading_relation('648051b4-8fb7-48d3-8e05-94734029d174', kjv_text_1611__revisable_translation_reading, forecloses).
narrative_ontology:cs_axiom('648051b4-8fb7-48d3-8e05-94734029d174', foundational, kjv_uniquely_inspired_in_english).
narrative_ontology:cs_axiom_status(kjv_uniquely_inspired_in_english, holdable).
narrative_ontology:cs_axiom_grounding('648051b4-8fb7-48d3-8e05-94734029d174', kjv_uniquely_inspired_in_english, theological).
narrative_ontology:cs_axiom('648051b4-8fb7-48d3-8e05-94734029d174', foundational, alternative_translations_corrupted_or_inferior).
narrative_ontology:cs_axiom_status(alternative_translations_corrupted_or_inferior, holdable).
narrative_ontology:cs_axiom_grounding('648051b4-8fb7-48d3-8e05-94734029d174', alternative_translations_corrupted_or_inferior, theological).
narrative_ontology:cs_axiom('648051b4-8fb7-48d3-8e05-94734029d174', secondary, textual_criticism_epistemically_corrupt_on_scripture).
narrative_ontology:cs_axiom_status(textual_criticism_epistemically_corrupt_on_scripture, holdable).
narrative_ontology:cs_axiom_grounding('648051b4-8fb7-48d3-8e05-94734029d174', textual_criticism_epistemically_corrupt_on_scripture, theological).
narrative_ontology:cs_reference_frame('648051b4-8fb7-48d3-8e05-94734029d174', kjv_singular_authority).
narrative_ontology:cs_drift_state('648051b4-8fb7-48d3-8e05-94734029d174', contemporary_archaeological_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('648051b4-8fb7-48d3-8e05-94734029d174', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership).
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_text_itself).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, modern_translation_communities).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, lay_believers_seeking_clarity).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, biblical_scholars_using_newer_manuscripts).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__exclusive_inspiration_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(kjv_text_1611__exclusive_inspiration_reading, 'none', 1).

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
 *   Extractiveness is 0.68 because the constraint transfers interpretive authority from individual believers and textual scholars to centralized KJV-Only leadership, restricting access to alternative translations and delegitimizing modern manuscript evidence. The transfer is substantial and decoupled from marginal service costs (the KJV text itself has not materially improved since 1769; the constraint's persistence is about institutional authority, not textual advancement). Suppression is 0.76 (high) because the constraint's persistence depends on active enforcement: denying that earlier manuscripts contradict the Textus Receptus, treating textual criticism as corrupt epistemology, and teaching lay believers that seeking clarity in modern translations is spiritually dangerous. Theater is 0.42 (moderate): authentic doctrinal concern about translation stability plays a real role, but a growing share of enforcement activity defends the exclusivity boundary against empirical challenge. The measurement series show rising extractiveness through t=25 (early phase of rising textual-archaeological challenges), then plateau at t=30 onwards (stabilization as KJV-Only institutions entrench their gatekeeping practices and reject new manuscript evidence as irrelevant), projecting flat at t=40. Suppression requirement rises similarly and plateaus, indicating institutional hardening without major new breakthrough. Theater ratio rises early (0.28→0.42) as the doctrinal case is increasingly supplemented by apologetic rhetoric and institutional boundary-marking, then stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (KJV-Only leadership) experiences the constraint as genuine textual protection and doctrinal coordination — they are defending scripture's integrity against corrupt alternatives. The payer seats (modern translation communities, textual scholars) experience the same structure as extractive gatekeeping that blocks legitimate scholarly work and linguistic access. The lay believer seat experiences it as dual: genuine community cohesion and doctrinal clarity (beneficiary function) coupled with restricted intellectual access and identity-lock (payer function). The engine computes these divergences from the structural data: the leadership's low directionality (beneficiary + institutional power + arbitrage-level exit options to defend the doctrine), the scholars' high directionality (victims + moderate power + constrained exit due to institutional retaliation), and the lay believers' mixed directionality (both beneficiary and payer, powerless, identity-locked). These seat-divergent classifications emerge from the structural declarations, not from any pre-adjudicated claim.
 *
 * DIRECTIONALITY LOGIC:
 *   KJV-Only leadership is the primary beneficiary (collects interpretive authority, controls the doctrine, lacks exit pressure because their identity IS the doctrine — arbitrage-level exit as institutional leaders in control of the rule-setting). Directionality is low (~0.1-0.2): they are substantially sheltered from extraction. Modern translation communities are victims (their textual work is delegitimized, their standing is blocked by institutional exclusion). Directionality is high (~0.8-0.9): the constraint extracts standing and suppresses their alternatives. Textual scholars are victims via the same mechanism (professional work treated as corrupt). Lay believers are mixed: they benefit from community boundaries and doctrinal clarity (beneficiary components, low-to-moderate d on the benefit side) but pay in restricted access and identity-lock (payer components, high d on the payer side). The constraint's directionality profile across seats is asymmetric by design: leadership captures the authority transfer while others bear the costs of access restriction and delegitimization.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Reformation fragmentation, competing English translations, doctrinal uncertainty) was a genuine 1611-era problem — KJV production was a real response. However, the founding problem status has shifted substantially over 400+ years. The Dead Sea Scrolls (1947+), Chester Beatty papyri, and Bodmer manuscripts now provide a much earlier textual base than anything available in 1611. Textual scholarship has converged on a reasonably stable manuscript genealogy. Modern translations have proliferated, yet major Christian traditions (Catholic, Orthodox, mainline Protestant, evangelical) have not fragmented into doctrinal chaos — they coordinate around different texts without losing unity-of-faith markers. The founding-problem mismatch is high: the problem the KJV was built to solve (unifying fragmented English translations in an era of textual uncertainty) has been substantially addressed by independent textual scholarship and ecumenical dialogue, yet the exclusive-inspiration constraint persists — not because the founding problem is live, but because institutional gatekeeping has become the agenda in itself. This is a textbook mandatrophy candidate: the constraint persists not to solve its founding problem but to preserve institutional authority. The theater_ratio measurements (starting 0.28, rising to 0.42) capture this drift: an increasing share of the constraint's enforcement activity is devoted to defending its own exclusivity boundary (against textual evidence and alternative traditions) rather than to solving the original coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manuscript_evidence_vs_inspiration_claim,
    'Does the availability of earlier Greek and Hebrew manuscripts (Dead Sea Scrolls, Chester Beatty, Bodmer papyri) that differ from the Textus Receptus (the KJV''s Greek source base) constitute evidence that the KJV''s exclusivity claim is historically contingent, or can textual inspiration be claimed independently of manuscript primacy?',
    'Empirical: detailed comparison of the KJV''s source manuscripts against earlier attested texts, coupled with theological analysis of whether inspiration-claims can be decoupled from manuscript history. Conceptual: clarification of what ''inspiration'' means in the presence of documented textual variation across attested sources.',
    'If earlier manuscripts represent a prior, more authentic textual state, the exclusivity claim loses its historical grounding and becomes a claim about selective preservation rather than inerrancy. If inspiration is asserted as orthogonal to manuscript history, the constraint''s empirical anchor dissolves and becomes purely doctrinal/aesthetic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(manuscript_evidence_vs_inspiration_claim, empirical, 'Whether the exclusivity claim can survive archaeological and textual-historical challenges to its manuscript basis.').

omega_variable(
    identity_lock_vs_authentic_access,
    'Is the lay believer''s ''identity-locked'' exit (trapped between KJV loyalty and spiritual guilt over seeking clarity) a structural feature of the constraint''s persistence, or a contingent effect of institutional messaging that could be decoupled?',
    'Observational: survey or ethnographic data on believers who transition from KJV-Only communities to communities accepting modern translations, documenting whether the identity-lock dissolves post-exit or persists. Conceptual: analysis of whether the constraint could be maintained without identity-fusion messaging.',
    'If identity-lock is structural and decoupling it undermines the constraint''s persistence, the constraint is more extractive than its coordination function alone justifies. If identity-lock dissolves cleanly on exit, the constraint''s persistence relies more on institutional authority than on internalized identity fusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_authentic_access, empirical, 'Whether suppression is internalized (identity) or structural (institutional).').

omega_variable(
    theological_gatekeeping_as_coordination,
    'Is the KJV-Only gatekeeping of ''true scripture'' a necessary feature of the coordination problem (fragmentation across translations), or an extractive use of coordination authority to restrict interpretive pluralism?',
    'Comparative: examine whether coordinated, single-text systems (Catholic Vulgate, Orthodox traditions with primary Septuagint/Slavonic use) achieve the same coordination function with different institutional gate-keepers and whether doctrinal unity is demonstrably worse in translation-pluralist contexts (mainline, evangelical, academic).',
    'If coordination is achievable under plural-translation frames, then gatekeeping is not necessary to solve the founding problem and is better classified as extractive authority-accumulation. If doctrinal fragmentation is empirically worse in plural contexts, gatekeeping''s coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_gatekeeping_as_coordination, empirical, 'Whether gatekeeping is a necessary coordination mechanism or an extraction mechanism riding on coordination.').

omega_variable(
    textual_revision_paradox,
    'The 1611 KJV was itself a revision of earlier English translations (Tyndale, Coverdale, Bishops'' Bible) and was subsequently revised in 1769. Can the exclusivity claim rationally apply to a text that was itself a contingent revisionary act, and if so, does that principle invalidate the exclusivity frame by admitting that later scholarly work might improve on 1611?',
    'Theological/logical: analysis of KJV-Only doctrine on the status of the 1611 vs. 1769 revision, and systematic comparison with arguments for excluding post-1611 scholarship on identical grounds.',
    'If the logic admits 1769 revision but excludes later scholarship, the boundary is arbitrary and the constraint is defensible only by appeal to authority, not principle. If the logic excludes later scholarship on principle, it must explain why 1611-to-1769 revision was legitimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_revision_paradox, conceptual, 'Whether the exclusivity claim is logically consistent given the KJV''s own status as a revision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__exclusive_inspiration_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t0, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(kjv__tr_t5, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(kjv__tr_t10, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(kjv__tr_t15, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(kjv__tr_t20, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(kjv__tr_t25, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(kjv__tr_t30, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(kjv__tr_t35, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(kjv__tr_t40, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(kjv__be_t0, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(kjv__be_t5, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(kjv__be_t10, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(kjv__be_t15, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(kjv__be_t20, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(kjv__be_t25, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(kjv__be_t30, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(kjv__be_t35, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(kjv__be_t40, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t0, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(kjv__su_t5, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(kjv__su_t10, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement(kjv__su_t15, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(kjv__su_t20, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(kjv__su_t25, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 25, 0.76).
narrative_ontology:measurement(kjv__su_t30, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(kjv__su_t35, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 35, 0.76).
narrative_ontology:measurement(kjv__su_t40, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 40, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__exclusive_inspiration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(kjv_text_1611__exclusive_inspiration_reading, 0.12).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__functional_equivalence_reading).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__revisable_translation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel kjv_text_1611. The sibling readings (functional_equivalence_reading, revisable_translation_reading) are separate constraint stories, each with its own epsilon, stakeholder structure, and type. All three stories are linked via network.affects_constraints. The exclusive_inspiration_reading forecloses both sibling readings within a single commitment framework (if KJV is uniquely inspired, functional equivalence and revisability are logically excluded). The sibling readings coexist with each other across different institutional parties. The constraint family decomposes the kernel contest into three structurally distinct claims, each with different extraction profiles: this reading (exclusive inspiration) has high extractiveness; the functional_equivalence_reading has lower extractiveness (pluralism admitted); the revisable_translation_reading has minimal extractiveness (scholarly process admitted). See network.affects_constraints for the linkage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
