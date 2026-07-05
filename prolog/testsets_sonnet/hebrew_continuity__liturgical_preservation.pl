% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__liturgical_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__liturgical_preservation, []).

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
 *   constraint_id: hebrew_continuity__liturgical_preservation
 *   human_readable: Hebrew Continuity Through Liturgical Preservation (Kernel Reading)
 *   domain: sociolinguistics/religious institutions/commitment systems
 *
 * SUMMARY:
 *   This story occupies the 'liturgical_preservation' reading of the
 *   contested hebrew_continuity kernel: the claim that Hebrew lives through
 *   preserved ritual recitation and textual transmission, requiring zero
 *   native speakers, with the primary victim class being anyone or anything
 *   pressing toward vernacularization or secularization of the register. This
 *   is deliberately NOT the native_generative reading (which requires living
 *   generative fluency and would have a near-zero ε since Israeli Hebrew's
 *   revival is substantially uncontested as living-language coordination) nor
 *   the bridge_pidginized reading (contact-language function across diaspora
 *   communities, a different beneficiary/victim structure again). Under this
 *   reading, Hebrew's 'life' is defined entirely by fidelity of recitation
 *   and textual transmission across a diaspora that does not need to speak
 *   the language generatively — a genuinely different empirical claim with a
 *   different ε than the other two readings, which is why it is authored as
 *   its own story rather than as a measurement variant of one story.
 *
 * KEY AGENTS:
 *   - rabbinic_authorities: Primary agenda-setter (institutional/arbitrage) — administers credentialing and defines authentic recitation
 *   - liturgical_publishers: Beneficiary (organized/mobile) — revenue from continued specialist mediation demand
 *   - synagogue_institutions: Beneficiary/agenda-setter (institutional/constrained) — communal identity structured around recitation competence
 *   - diaspora_congregants: Payer/beneficiary (moderate/constrained) — bear study cost, gain belonging
 *   - secularizing_diaspora_youth: Primary target (powerless/mobile) — bear stigma cost of exit from recitation-without-comprehension model
 *   - hebrew_language_reformers: Excluded (powerless/trapped) — structurally shut out of credentialing institutions
 *   - linguistic_anthropologists: Analytical observer — documents the register's sociolinguistic function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, 0.28).
domain_priors:suppression_score(hebrew_continuity__liturgical_preservation, 0.34).
domain_priors:theater_ratio(hebrew_continuity__liturgical_preservation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, extractiveness, 0.28).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, resistance, 0.46).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__liturgical_preservation, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__liturgical_preservation, "Hebrew Continuity Through Liturgical Preservation (Kernel Reading)").
narrative_ontology:topic_domain(hebrew_continuity__liturgical_preservation, "sociolinguistics/religious institutions/commitment systems").

domain_priors:requires_active_enforcement(hebrew_continuity__liturgical_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__liturgical_preservation, 'b0e7386b-ced6-453b-804b-a87c05e17c5d').
narrative_ontology:cs_kernel_codification('b0e7386b-ced6-453b-804b-a87c05e17c5d', fixed_text).
narrative_ontology:cs_authority_grounding('b0e7386b-ced6-453b-804b-a87c05e17c5d', lineage).
narrative_ontology:cs_interpretation_layer_present('b0e7386b-ced6-453b-804b-a87c05e17c5d').
narrative_ontology:cs_reading_relation('b0e7386b-ced6-453b-804b-a87c05e17c5d', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_reading_relation('b0e7386b-ced6-453b-804b-a87c05e17c5d', hebrew_continuity__bridge_pidginized, influences).
narrative_ontology:cs_axiom('b0e7386b-ced6-453b-804b-a87c05e17c5d', foundational, recitation_fidelity_constitutes_transmission).
narrative_ontology:cs_axiom_status(recitation_fidelity_constitutes_transmission, holdable).
narrative_ontology:cs_axiom_grounding('b0e7386b-ced6-453b-804b-a87c05e17c5d', recitation_fidelity_constitutes_transmission, conventional).
narrative_ontology:cs_axiom('b0e7386b-ced6-453b-804b-a87c05e17c5d', foundational, native_fluency_unnecessary_for_language_survival).
narrative_ontology:cs_axiom_status(native_fluency_unnecessary_for_language_survival, holdable).
narrative_ontology:cs_axiom_grounding('b0e7386b-ced6-453b-804b-a87c05e17c5d', native_fluency_unnecessary_for_language_survival, empirically_contingent).
narrative_ontology:cs_reference_frame('b0e7386b-ced6-453b-804b-a87c05e17c5d', diaspora_dispersion_without_national_vernacular_anchor).
narrative_ontology:cs_drift_state('b0e7386b-ced6-453b-804b-a87c05e17c5d', post_1948_israeli_revival_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b0e7386b-ced6-453b-804b-a87c05e17c5d', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__liturgical_preservation, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, liturgical_publishers).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, synagogue_institutions).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, secularizing_diaspora_youth).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, non_orthodox_reform_congregants).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, hebrew_language_reformers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, diaspora_congregants).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, diaspora_congregants).
narrative_ontology:constraint_vindicates(hebrew_continuity__liturgical_preservation, textual_fixity_doctrine).
narrative_ontology:constraint_vindicates(hebrew_continuity__liturgical_preservation, unbroken_transmission_chain_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determine which recitation traditions, pronunciations, and textual variants count as authentic transmission. Administer the training pipeline (yeshiva, cantorial schools) that credentials who may lead prayer and teach the liturgy. Their institutional standing depends on Hebrew remaining a fixed, ritually-bound register rather than a living vernacular that could drift beyond their adjudication.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, rabbinic_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Produce and sell prayer books, cantillation guides, and textual commentaries that depend on continued demand for precise liturgical Hebrew literacy. Revenue scales with the perceived necessity of specialist mediation between congregants and the fixed text.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, liturgical_publishers, beneficiary,
    organized, generational, mobile, global).

% Organize communal life around the recitation calendar; membership, dues, and communal identity are structured by participation in liturgical Hebrew practice. Institutional continuity depends on congregants continuing to treat correct recitation as the marker of belonging.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, synagogue_institutions, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, synagogue_institutions, agenda_setter).

% Learn to recite texts they frequently do not fluently understand, investing years of study (bar/bat mitzvah preparation, Hebrew school) to meet a competence bar defined by others. They gain communal belonging and continuity with ancestors, but bear the cost of memorization without functional generative fluency, and face social judgment for imperfect recitation.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, diaspora_congregants, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, diaspora_congregants, beneficiary).

% Increasingly opt out of liturgical Hebrew literacy entirely, finding the recitation-without-comprehension model alienating relative to a living language they could actually speak. Their disengagement is treated by the kernel's authorities as a threat to be countered rather than a legitimate structural critique; they bear reputational cost (accusations of assimilation) for exiting.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, secularizing_diaspora_youth, payer,
    powerless, biographical, mobile, national).

% Participate in liturgical traditions that have vernacularized portions of the service (English transliteration, translated prayer) but remain measured against the Orthodox liturgical-preservation standard as less authentic. They pay a legitimacy tax even while partially exiting the full recitation requirement.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, non_orthodox_reform_congregants, payer,
    moderate, biographical, constrained, national).

% Advocate for treating Hebrew primarily as a living, generative language (the native_generative reading) rather than a fixed liturgical register. Their proposals to prioritize spoken fluency over recitation accuracy are structurally excluded from the liturgical institutions that control credentialing and communal legitimacy.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, hebrew_language_reformers, excluded,
    powerless, generational, trapped, national).

% Study the sociolinguistic function of liturgical Hebrew as a preserved ritual register, documenting how recitation-based transmission maintains textual continuity independent of vernacular fluency, and how this compares to the other kernel readings.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, linguistic_anthropologists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a stable, mutually intelligible ritual register across a geographically dispersed diaspora spanning many vernacular languages and centuries — anyone trained in the recitation tradition anywhere in the world can participate in the same liturgical service and access the same textual corpus without translation drift.
% TRANSFER_FUNCTION: Moves years of study time, tuition to Hebrew schools and cantorial training, and publisher revenue from congregant families toward rabbinic institutions, liturgical publishers, and credentialed instructors, in exchange for communal belonging and access to sanctioned ritual participation.
% ABSENT_VOICES: Hebrew language reformers who want the kernel occupied by native generative fluency are excluded from the credentialing and communal-legitimacy structures that liturgical institutions control; secularizing youth who exit are treated as attrition rather than as a structural verdict on the recitation-without-comprehension model.
% DISAPPEARANCE_RATIONALE: Rabbinic authorities and synagogue institutions would say the world rearranges catastrophically — the unbroken chain of transmission breaks, communal cohesion around shared text dissolves. Secularizing youth and reformers would say comprehension-based engagement with Jewish text and identity would likely increase, not decrease, if freed from the recitation-accuracy gate; the underlying textual corpus (Torah, siddur) would persist in translation and study regardless of ritual recitation continuing unchanged.
% FOUNDING_PROBLEM: A diaspora scattered across many vernaculars needed a stable, non-drifting register for shared textual and liturgical continuity, since no single geographic community could anchor a living spoken standard the way a nation-state language does.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities and liturgical publishers attest the founding problem is fully live — dispersion continues, textual fidelity remains at risk from assimilation. Independent linguistic anthropologists and Hebrew language reformers note that since 1948 a living generative Hebrew vernacular exists in Israel, which arguably resolves the original diaspora-continuity problem differently than recitation preservation does, making the liturgical-preservation kernel occupation increasingly a communal-identity function rather than a linguistic-survival function. No party fully outside the liturgical institutions' orbit attests that recitation preservation remains linguistically necessary rather than institutionally self-perpetuating.
narrative_ontology:disappearance_verdict(hebrew_continuity__liturgical_preservation, contested).
narrative_ontology:founding_problem_status(hebrew_continuity__liturgical_preservation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__liturgical_preservation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_continuity__liturgical_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__liturgical_preservation, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__liturgical_preservation_tests).
:- end_tests(hebrew_continuity__liturgical_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.28) because the coordination function is real — a fixed liturgical register genuinely does solve a diaspora-wide legibility problem — but a meaningful share of the credentialing apparatus (cantorial certification, publisher-dependent literacy gates) extracts study time and fees beyond what preservation strictly requires. Theater ratio is elevated and rising (0.20→0.42) because an increasing share of recitation-accuracy enforcement (precise trope, pronunciation policing) functions as in-group signaling and institutional self-justification rather than transmission necessity — the underlying text is preserved in written form regardless of oral recitation precision, so much of the accuracy enforcement is performative maintenance of authority rather than functionally necessary for continuity. Suppression is moderate (0.34) and structural: it operates through communal belonging gates and legitimacy stigma rather than coercive force, which is why accessibility_collapse (0.58) is mid-range rather than near-total — reform and secular alternatives visibly exist and are chosen by real numbers of people, they are just penalized.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities and synagogue institutions sit near the beneficiary end: they administer the kernel occupation and their institutional standing depends on it persisting exactly as recitation-based. Liturgical publishers are a clear beneficiary with mobile exit (their business model could pivot to translation/study materials but currently profits from the recitation-literacy gate). Secularizing youth and reformers sit near the target end: they bear the cost of a fidelity standard they did not choose and are structurally excluded from redefining it. Diaspora congregants and Reform congregants are genuinely mixed — real coordination benefit (communal continuity, felt connection to ancestors) alongside real cost (comprehension-free labor, legitimacy taxation for partial exit) — captured by dual roles rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora-wide register stability absent a national vernacular anchor) was substantially resolved differently by the 1948 Israeli Hebrew revival, which produced a living generative standard the native_generative reading tracks. Under the liturgical_preservation reading specifically, the founding problem has been partially superseded by that alternative resolution but the institutional apparatus (credentialing, publishing, communal-belonging gates) persists at increasing scale — classic mandatrophy signature: the mandate (register stability for isolated diaspora) is less live than it was, while the administering institutions' theater ratio rises. This is exactly the kind of divergence the tangled_rope classification is built to hold: real coordination function (shared liturgical intelligibility) plus asymmetric extraction (accuracy-policing that outruns functional necessity) under active enforcement (credentialing gates, communal legitimacy stigma) — not a pure snare, because genuine coordination value remains for participating congregants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recitation_fidelity_necessity,
    'Is precise recitation/cantillation accuracy functionally necessary for textual transmission continuity, or does the written textual corpus alone suffice, making oral accuracy enforcement primarily a social/institutional signaling function?',
    'Comparative study of communities that have relaxed recitation-accuracy standards (some Reform and Reconstructionist congregations) versus those maintaining strict standards (Orthodox): does textual transmission integrity measurably differ, or only communal-identity markers?',
    'If accuracy enforcement is not functionally necessary for transmission, a larger share of the measured extraction and suppression is pure institutional rent rather than coordination cost, pushing the classification toward snare; if genuinely necessary (e.g., oral tradition carries information the written text alone loses), more of it is legitimate coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recitation_fidelity_necessity, empirical, 'Whether oral recitation accuracy is functionally load-bearing for transmission or primarily performative.').

omega_variable(
    kernel_reading_supersession,
    'Does the existence of a living generative Hebrew standard (Israeli Hebrew, satisfying the native_generative reading) partially or fully discharge the founding problem this liturgical_preservation reading was built to solve, making continued strict liturgical-preservation enforcement institutionally self-perpetuating rather than functionally necessary?',
    'Track whether diaspora communities with high exposure to living Israeli Hebrew (through travel, media, Hebrew immersion programs) show reduced reliance on liturgical-only Hebrew literacy for felt continuity, versus communities isolated from the living standard.',
    'If liturgical preservation is substantially superseded as a continuity mechanism by the living-language alternative, the founding_problem_status moves from contested toward dead, strengthening a mandatrophy reading; if diaspora communities remain functionally isolated from the living standard, the founding problem_status remains genuinely live for this reading specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_supersession, conceptual, 'Whether the native_generative reading''s success discharges this reading''s founding justification.').

omega_variable(
    kernel_framing_underdetermination,
    'Is ''Hebrew lives'' best decomposed into three parallel readings (liturgical, native_generative, bridge_pidginized) as done here, or is liturgical_preservation better understood as a subordinate special case nested within bridge_pidginized (since diaspora liturgical use is itself a form of bridge/contact function)?',
    'Sociolinguistic taxonomy work distinguishing register function (liturgical/ritual-only use with no interactional generativity) from contact-language function (used for actual interpersonal communication across vernacular boundaries) — do communities that recite Hebrew liturgically also use it as an interactional bridge, or are these empirically disjoint populations?',
    'If the two functions are empirically disjoint (most liturgical-only communities never use Hebrew interactionally), the three-way decomposition used in this story family is correct and should remain three separate constraints; if they substantially overlap, liturgical_preservation may need remerging into bridge_pidginized as a single constraint with adjusted ε, changing this story''s beneficiary/victim structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the liturgical/bridge distinction in the kernel decomposition is empirically well-founded or an artifact of framing choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__liturgical_preservation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__liturgical_preservation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hebr_tr_t20, hebrew_continuity__liturgical_preservation, theater_ratio, 20, 0.25).
narrative_ontology:measurement(hebr_tr_t40, hebrew_continuity__liturgical_preservation, theater_ratio, 40, 0.3).
narrative_ontology:measurement(hebr_tr_t60, hebrew_continuity__liturgical_preservation, theater_ratio, 60, 0.34).
narrative_ontology:measurement(hebr_tr_t80, hebrew_continuity__liturgical_preservation, theater_ratio, 80, 0.38).
narrative_ontology:measurement(hebr_tr_t100, hebrew_continuity__liturgical_preservation, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__liturgical_preservation, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(hebr_be_t20, hebrew_continuity__liturgical_preservation, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(hebr_be_t40, hebrew_continuity__liturgical_preservation, base_extractiveness, 40, 0.22).
narrative_ontology:measurement(hebr_be_t60, hebrew_continuity__liturgical_preservation, base_extractiveness, 60, 0.24).
narrative_ontology:measurement(hebr_be_t80, hebrew_continuity__liturgical_preservation, base_extractiveness, 80, 0.26).
narrative_ontology:measurement(hebr_be_t100, hebrew_continuity__liturgical_preservation, base_extractiveness, 100, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__liturgical_preservation, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(hebr_su_t20, hebrew_continuity__liturgical_preservation, suppression_requirement, 20, 0.25).
narrative_ontology:measurement(hebr_su_t40, hebrew_continuity__liturgical_preservation, suppression_requirement, 40, 0.28).
narrative_ontology:measurement(hebr_su_t60, hebrew_continuity__liturgical_preservation, suppression_requirement, 60, 0.3).
narrative_ontology:measurement(hebr_su_t80, hebrew_continuity__liturgical_preservation, suppression_requirement, 80, 0.32).
narrative_ontology:measurement(hebr_su_t100, hebrew_continuity__liturgical_preservation, suppression_requirement, 100, 0.34).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__liturgical_preservation, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__liturgical_preservation, 0.1).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, bridge_pidginized).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings occupying the contested hebrew_continuity kernel. liturgical_preservation (this story) claims Hebrew lives through preserved ritual recitation independent of native fluency — moderate ε, tangled_rope, institutional beneficiaries (rabbinic authorities, publishers, synagogues) versus secularizing/reform victims. native_generative claims Hebrew lives only through living generative daily use — substantially satisfied by the Israeli revival, expected low ε, rope/mountain-adjacent. bridge_pidginized claims Hebrew lives as a diaspora contact language, an intermediate functional claim with its own beneficiary/victim set (interpreters and community bridge-figures versus linguistic purists on both flanks). Each carries a distinct, stable ε and distinct stakeholder structure; they are linked here via affects_constraints rather than merged, per the ε-invariance principle — the successful entrenchment of native_generative Hebrew in Israel exerts real downstream pressure on liturgical_preservation's founding-problem status (see omega kernel_reading_supersession), which is the concrete content of the network edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
