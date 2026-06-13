% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__functional_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__functional_equivalence_reading, []).

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
 *   constraint_id: kjv_text_1611__functional_equivalence_reading
 *   human_readable: KJV as Functional-Equivalence Reading: Complementary Translation Authority
 *   domain: religious_studies/textual_criticism/theology
 *
 * SUMMARY:
 *   The King James Version (1611) stands as a canonical English Bible after
 *   400 years of use, valued for its literary cadence, theological weight,
 *   and continuity with the Protestant tradition. This constraint story
 *   models ONE reading of a contested kernel: the functional-equivalence
 *   reading, which asserts that multiple translations serve complementary
 *   purposes—the KJV valuable for historical/literary reasons, modern
 *   translations for clarity and contemporary accessibility. This reading
 *   distributes translational authority across a portfolio of versions rather
 *   than concentrating it in a single text. The constraint's stability
 *   depends on accepting that no single translation holds gate-keeping power
 *   over Scripture or doctrine, which is actively contested by
 *   exclusive-inspiration advocates (who claim the KJV is uniquely inerrant)
 *   and by revisionist critics (who claim the KJV should be displaced by
 *   better scholarship). The claim/metric gap is deliberate: this constraint
 *   is CLAIMED as a rope (genuine coordination of complementary purposes)
 *   while the extracted authority shifts measured at 0.38—moderate and
 *   declining as the reading stabilizes over the interval.
 *
 * KEY AGENTS:
 *   - kjv_custodians: Institutional agenda-setters who promote and preserve the KJV's recognized place without claiming exclusivity. They navigate between exclusive-inspiration advocates (who want KJV supremacy) and revisionist critics (who want KJV replacement), holding the middle ground.
 *   - liturgical_communities: Primary beneficiaries using the KJV in worship for its beauty and continuity while maintaining exit options (they are not prevented from using other translations). They have organized power and mobile exit—genuine choice, not coercion.
 *   - modern_translation_publishers: Payers who face diffuse competition from the KJV's public-domain availability and cultural prestige, yet remain engaged because market demand for clarity and accessibility is real. They have powerful institutional resources and complete exit (they can stop publishing).
 *   - literary_scholars and historical_researchers: Secondary beneficiaries studying the KJV as a canonical English text and historical artifact. No one prevents them from consulting other translations; they benefit from the KJV's availability without restriction.
 *   - exclusive_inspiration_advocates: Excluded from this reading's decision structure because it denies their core claim. They would argue for KJV-only authority but are not in the room where functional equivalence is operative.
 *   - revisionist_textual_critics: Excluded because the reading treats the KJV as a reference standard, not a candidate for replacement. They have exit (they publish their own translations) but are excluded from the authority structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__functional_equivalence_reading, 0.38).
domain_priors:suppression_score(kjv_text_1611__functional_equivalence_reading, 0.22).
domain_priors:theater_ratio(kjv_text_1611__functional_equivalence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__functional_equivalence_reading, rope).
narrative_ontology:human_readable(kjv_text_1611__functional_equivalence_reading, "KJV as Functional-Equivalence Reading: Complementary Translation Authority").
narrative_ontology:topic_domain(kjv_text_1611__functional_equivalence_reading, "religious_studies/textual_criticism/theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__functional_equivalence_reading, 'ca663783-14a4-48f3-8e08-67114bb158e4').
narrative_ontology:cs_kernel_codification('ca663783-14a4-48f3-8e08-67114bb158e4', fixed_text).
narrative_ontology:cs_authority_grounding('ca663783-14a4-48f3-8e08-67114bb158e4', lineage).
narrative_ontology:cs_interpretation_layer_present('ca663783-14a4-48f3-8e08-67114bb158e4').
narrative_ontology:cs_reading_relation('ca663783-14a4-48f3-8e08-67114bb158e4', kjv_text_1611__exclusive_inspiration_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca663783-14a4-48f3-8e08-67114bb158e4', kjv_text_1611__revisable_translation_reading, coexists_with).
narrative_ontology:cs_axiom('ca663783-14a4-48f3-8e08-67114bb158e4', foundational, multiple_legitimate_translations).
narrative_ontology:cs_axiom_status(multiple_legitimate_translations, holdable).
narrative_ontology:cs_axiom_grounding('ca663783-14a4-48f3-8e08-67114bb158e4', multiple_legitimate_translations, conventional).
narrative_ontology:cs_axiom('ca663783-14a4-48f3-8e08-67114bb158e4', foundational, authority_distributed_without_hierarchy).
narrative_ontology:cs_axiom_status(authority_distributed_without_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('ca663783-14a4-48f3-8e08-67114bb158e4', authority_distributed_without_hierarchy, instrumental).
narrative_ontology:cs_reference_frame('ca663783-14a4-48f3-8e08-67114bb158e4', distributed_translation_authority).
narrative_ontology:cs_drift_state('ca663783-14a4-48f3-8e08-67114bb158e4', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ca663783-14a4-48f3-8e08-67114bb158e4', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__functional_equivalence_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, liturgical_communities).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, literary_scholars).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, historical_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, modern_translation_publishers).
narrative_ontology:constraint_victim(kjv_text_1611__functional_equivalence_reading, modern_translation_publishers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and promote the KJV as a legitimate translation choice for worship, scholarship, and cultural transmission. Argue that the KJV's literary beauty, historical continuity, and extensive commentary tradition make it valuable alongside modern translations. Their agenda is preserving the KJV's recognized place in the Protestant canon without claiming exclusivity.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, kjv_custodians, agenda_setter,
    institutional, generational, arbitrage, global).

% Use the KJV in worship for its rhythmic beauty, theological weight, and intergenerational continuity. They value the KJV as a familiar anchor to tradition but are not prevented from using other translations; they choose the KJV for specific liturgical purposes. The coordination function is the KJV's availability as one recognized option among several.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, liturgical_communities, beneficiary,
    organized, generational, mobile, regional).

% Market alternative translations (NASB, NIV, ESV, NRSV, etc.) for different reading purposes: clarity, study, literary accessibility. They face low but real competition from the KJV's cultural prestige and cost advantage (public domain). Their exit is complete—they can stop producing alternatives or shift focus—but market demand keeps them engaged.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, modern_translation_publishers, payer,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__functional_equivalence_reading, modern_translation_publishers, beneficiary).

% Study the KJV as a canonical English text shaping literature, philosophy, and cultural reference. They benefit from the KJV's recognized status without restriction; they can and do use other translations for comparison. The coordination function is the KJV's availability as a studied historical artifact.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, literary_scholars, beneficiary,
    moderate, biographical, mobile, global).

% Use the KJV as primary source material for understanding early modern English, religious history, and cultural impact. They benefit from the KJV's recognized textual stability and availability. No one prevents them from consulting other translations or original languages.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, historical_researchers, beneficiary,
    moderate, biographical, mobile, global).

% Believe the KJV is the exclusively inspired, inerrant English text and view the functional-equivalence reading as a capitulation to modernism. They are structurally excluded from this reading's decision-making because it explicitly denies their central claim; they would argue for KJV-only doctrine but are not in the room where functional equivalence is the working premise.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, exclusive_inspiration_advocates, excluded,
    organized, generational, constrained, regional).

% Argue the KJV reflects inferior manuscript sources and outdated translation philosophy; they advocate for continuous revision as scholarship improves. They are structurally excluded from the functional-equivalence framework because it treats the KJV as a stable reference point, not a candidate for replacement. They have exit (they publish their own translations) but are excluded from this reading's authority structure.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, revisionist_textual_critics, excluded,
    moderate, biographical, mobile, global).

% Navigate the functional-equivalence reading as institutional policy: permitting multiple translations in worship while acknowledging the KJV's historical and literary significance. They observe the contestation without being targets of it; they can shift their policy independently.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, mainline_protestant_denominations, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes multiple legitimate translation options by assigning each a specific scholarly and liturgical purpose: the KJV for historical continuity and literary study, modern translations for clarity and accessibility. This solves the coordination problem of how to honor textual tradition while meeting contemporary readers' needs without declaring any single translation the exclusive standard.
% TRANSFER_FUNCTION: Moves scholarly authority and cultural prestige toward a distributed model: no single translation gate-keeps access to Scripture or theological authority. Modern publishers gain market legitimacy by positioning themselves as complementary to the KJV; the KJV retains cultural weight while ceding exclusive authority claims.
% ABSENT_VOICES: Exclusive inspiration advocates are structurally excluded because the reading denies their core premise—that the KJV is uniquely inspired. Revisionist textual critics are excluded because the reading treats the KJV as a reference standard rather than a candidate for replacement. Both groups would contest the functional-equivalence framing from outside the decision structure.
% DISAPPEARANCE_RATIONALE: If the functional-equivalence reading vanished, Protestant denominations would split into two poles: exclusive-inspiration communities retreating to KJV-only doctrine with reduced denominational scale, and modern-translation communities consolidating authority in newer versions. The reading's disappearance would eliminate the negotiated middle ground, forcing institutional polarization.
% FOUNDING_PROBLEM: Early modern English Protestantism needed a vernacular Scripture accessible to lay readers while maintaining connection to the learned translation tradition. The KJV solved this by balancing scholarly Hebrew/Greek fidelity with readable English cadence, becoming the standard reference for 400 years.
% FOUNDING_PROBLEM_CORROBORATION: KJV custodians and liturgical communities attest the founding problem is live: lay readers still value a readable, dignified translation. Modern translation publishers and textual scholars attest the problem is substantially solved by superior scholarship: newer translations better represent the original languages for contemporary readers. Academic testimony from historical linguistics and manuscript studies supports the latter reading.
narrative_ontology:disappearance_verdict(kjv_text_1611__functional_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__functional_equivalence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__functional_equivalence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(kjv_text_1611__functional_equivalence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__functional_equivalence_reading_tests).
:- end_tests(kjv_text_1611__functional_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38, declining from 0.28 over the interval) because the functional-equivalence reading distributes authority rather than concentrating it. The KJV retains cultural weight and scholarly attention, but this is not extraction in the snare sense—it is the legitimate outcome of 400 years of canonical status and literary beauty. Modern publishers face diffuse competition but are not prevented from market entry; liturgical communities have genuine choice among translations. The constraint succeeds precisely by NOT extracting from its beneficiaries—the KJV is available without gate-keeping modern translations out. Suppression is low (0.22, declining to 0.22 over 25 time points) because the reading does not require active coercion to hold its shape. Exclusive-inspiration advocates and revisionist critics are excluded from the decision structure, but that exclusion is argumentative (they disagree with the premise) rather than coercive (they are not prevented from promoting their own readings). Theater is low and stable (0.18) because the functional-equivalence reading's coordination function is genuine—it solves a real problem of how to honor tradition while meeting contemporary needs—and does not rely on performative maintenance. The measurements show the constraint stabilizing over its interval: extractiveness plateaus near 0.38, suppression and theater both decline slightly, suggesting the functional-equivalence reading is consolidating as a sustainable equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   The KJV custodians' seat and the exclusive-inspiration advocates' excluded seat experience this constraint radically differently. From the custodian position, the constraint is a genuine coordination solution preserving the KJV's place while accommodating modern scholarship. From the excluded position, the constraint looks like a betrayal—a denial of the KJV's unique divine inspiration. Revisionist critics occupy yet another perspective: for them, the functional-equivalence reading is a holding action that slows necessary textual improvement. The engine computes each seat's type from the structural data (power, exit, beneficiary/victim status, suppression). The custodians should compute as beneficiaries with low directionality (they benefit from the KJV's preserved status); modern publishers as symmetric or slight-target (they face competition but have resources and exit); liturgical communities as beneficiaries with mobile exit (they benefit without coercion); excluded advocates should NOT appear in the per-seat classification (excluded is a narrative role, not a classification seat) but their existence and argument structure inform the omega variables about kernel contestation.
 *
 * DIRECTIONALITY LOGIC:
 *   The KJV custodians derive d near the beneficiary end (0.1–0.2): they benefit from the functional-equivalence reading's official recognition of the KJV's legitimacy, and they have institutional exit (they can promote alternative readings). Modern translation publishers derive d near symmetric or slight-target (0.45–0.55): they face diffuse competition from the KJV's prestige and public-domain availability, but they have powerful resources, complete market exit, and real demand for clarity-focused translations. Their exit is arbitrage-grade despite the KJV competition. Liturgical communities derive d near beneficiary (0.2–0.3): they benefit from having multiple legitimate options, they have genuinely mobile exit (nothing prevents them from switching translations), and they are not coerced into KJV use. The exclusion of both sibling readings' advocates does not directly affect directionality—exclusion is a narrative fact (these seats are not in the decision structure), while directionality is a structural property (how much this constraint extracts from or subsidizes a given agent). The excluded advocates would have high d (1.0) if they were seated as targets, because the functional-equivalence reading denies the premise each sibling reading is built on. But they are excluded, not seated as targets, so this remains a narrative fact captured in the omega and commentary rather than a directionality computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The functional-equivalence reading avoids mandatrophy because its founding problem—how to honor textual tradition while serving contemporary readers—remains genuinely live. Liturgical communities still value the KJV's beauty and continuity; modern readers still need clarity; scholars still study the KJV as a historical artifact. The constraint solves a real coordination problem (distributing authority without concentrating it in a single text) rather than serving a defunct mandate. The exclusive-inspiration reading would face mandatrophy if the founding problem (the need for an exclusively authoritative English Bible) were dead—but within that reading's own tradition, the problem remains live as a theological premise. The revisable-translation reading faces potential mandatrophy if scholarship advances to the point that the KJV's manuscript basis and translation philosophy are so clearly superseded that keeping it as a reference standard becomes purely theatrical—but that is a future state, not the current one. This reading (functional-equivalence) stabilizes the constraint precisely by distributing the burden: no single text bears the entire weight of being the authoritative English Scripture, so no single text faces mandatrophy-via-obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the KJV a uniquely inspired English text (exclusive_inspiration_reading), a complementary translation among many (functional_equivalence_reading), or a historically important but improvable translation (revisable_translation_reading)?',
    'The three readings coexist as live theological positions held by different denominational and scholarly communities. No single resolution will settle all parties because each reading encodes a different epistemic claim about authority sources (divine inspiration, functional utility, scholarly improvement) that cannot be simultaneously true in the same framework.',
    'This reading (functional_equivalence) asserts that multiple valid translations serve different purposes without hierarchical ranking. The sibling readings reject this: exclusive inspiration claims the KJV is uniquely authoritative; revisable translation claims the KJV is a stepping-stone to better versions. Each reading''s classification (extraction, authority structure, suppression) follows from which reading''s premise is operative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel contest: which reading of the KJV''s status is operative').

omega_variable(
    authority_decentralization_feasibility,
    'Can Protestant textual authority remain stable when distributed across multiple translations, or does decentralization inevitably erode any single translation''s authoritativeness?',
    'Historical observation of Protestant communities that adopt functional-equivalence policy: do they maintain doctrinal coherence despite translation diversity, or do differences in phrasing/emphasis generate theological drift? Do communities eventually select a preferred translation de facto despite de jure pluralism?',
    'If decentralization erodes authority, the functional-equivalence reading collapses as a stable arrangement and the constraint reclassifies toward snare (pluralism as cover for authority erosion). If authority remains coherent across translations, the reading sustains as a genuine coordination solution with distributed authority architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_decentralization_feasibility, empirical, 'Whether decentralized translation authority can remain functionally stable').

omega_variable(
    extraction_via_translation_dominance,
    'Does the KJV''s cultural prestige and public-domain availability extract an unmeasured cost from modern translation publishers by restricting their pricing and market share?',
    'Economic analysis of translation market share and publisher profitability before/after competing modern translations. Do publishers recoup development costs for new translations, or does KJV competition prevent market consolidation that would justify investment?',
    'If extraction is real, the measured extractiveness (0.38) underestimates the constraint''s cost to a substantial payer class (modern publishers), and the classification could shift toward tangled_rope. If extraction is negligible (publishers profit adequately despite competition), the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_via_translation_dominance, empirical, 'Whether KJV dominance extracts hidden costs from modern translation publishers').

omega_variable(
    identity_fusion_kjv_heritage,
    'For communities that use the KJV, is the attachment to the KJV primarily functional (it works for our purposes), identity-fused (the KJV is constitutive of who we are), or ideologically identity-locked (exiting the KJV would dissolve the community''s self-understanding)?',
    'Ethnographic observation of communities that shift translation: do they experience the shift as a practical adjustment, an identity loss, or an ideological betrayal? How quickly do new cohorts adopt the KJV if raised on modern translations?',
    'If identity fusion is high, the measured suppression and resistance underestimate the constraint''s internalized hold on affected parties. If functional, the rope classification is robust. If identity-locked, the constraint carries interpersonal suppression omegas and may require recalibration of exit_options for affected stakeholders.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_fusion_kjv_heritage, empirical, 'Degree of identity fusion with the KJV among liturgical communities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__functional_equivalence_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t0, kjv_text_1611__functional_equivalence_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(kjv__tr_t0, observed).
narrative_ontology:measurement(kjv__tr_t5, kjv_text_1611__functional_equivalence_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(kjv__tr_t5, observed).
narrative_ontology:measurement(kjv__tr_t10, kjv_text_1611__functional_equivalence_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(kjv__tr_t10, observed).
narrative_ontology:measurement(kjv__tr_t15, kjv_text_1611__functional_equivalence_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement_basis(kjv__tr_t15, observed).
narrative_ontology:measurement(kjv__tr_t20, kjv_text_1611__functional_equivalence_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(kjv__tr_t20, observed).
narrative_ontology:measurement(kjv__tr_t25, kjv_text_1611__functional_equivalence_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(kjv__tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(kjv__be_t0, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(kjv__be_t0, observed).
narrative_ontology:measurement(kjv__be_t5, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(kjv__be_t5, observed).
narrative_ontology:measurement(kjv__be_t10, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement_basis(kjv__be_t10, observed).
narrative_ontology:measurement(kjv__be_t15, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement_basis(kjv__be_t15, observed).
narrative_ontology:measurement(kjv__be_t20, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(kjv__be_t20, observed).
narrative_ontology:measurement(kjv__be_t25, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(kjv__be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t0, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(kjv__su_t0, observed).
narrative_ontology:measurement(kjv__su_t5, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 5, 0.26).
narrative_ontology:measurement_basis(kjv__su_t5, observed).
narrative_ontology:measurement(kjv__su_t10, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 10, 0.24).
narrative_ontology:measurement_basis(kjv__su_t10, observed).
narrative_ontology:measurement(kjv__su_t15, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 15, 0.23).
narrative_ontology:measurement_basis(kjv__su_t15, observed).
narrative_ontology:measurement(kjv__su_t20, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement_basis(kjv__su_t20, observed).
narrative_ontology:measurement(kjv__su_t25, kjv_text_1611__functional_equivalence_reading, suppression_requirement, 25, 0.22).
narrative_ontology:measurement_basis(kjv__su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__functional_equivalence_reading, information_standard).
narrative_ontology:boltzmann_floor_override(kjv_text_1611__functional_equivalence_reading, 0.12).
narrative_ontology:affects_constraint(kjv_text_1611__functional_equivalence_reading, kjv_text_1611__exclusive_inspiration_reading).
narrative_ontology:affects_constraint(kjv_text_1611__functional_equivalence_reading, kjv_text_1611__revisable_translation_reading).

% DUAL FORMULATION NOTE:
% The constraint family kjv_text_1611 consists of three structurally distinct constraints, each a different reading of the same kernel (the historical KJV and its claimed authority). The exclusive_inspiration_reading asserts the KJV is uniquely inerrant—high extraction, high suppression, snare-flavored. The revisable_translation_reading asserts the KJV should be displaced by better scholarship—lower extraction on the KJV itself but higher pressure on custodians to justify keeping it as a reference. The functional_equivalence_reading (this constraint) distributes authority without ranking—moderate extraction, low suppression, rope-flavored. Each reading has its own ε, its own beneficiary/victim structure, its own type. The family structure reflects the kernel contest: these are not different measurements of one constraint, but three separate constraints arising from three irreconcilable premises about what the KJV's status should be. Network links enable contamination propagation analysis: changes in one reading's institutional support affect the others' operating environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
