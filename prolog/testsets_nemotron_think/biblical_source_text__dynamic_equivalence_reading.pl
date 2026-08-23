% ============================================================================
% CONSTRAINT STORY: biblical_source_text__dynamic_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: biblical_source_text__dynamic_equivalence_reading
 *   human_readable: Dynamic Equivalence Translation Philosophy
 *   domain: religious/linguistic/authority
 *
 * SUMMARY:
 *   The dynamic equivalence reading of the biblical source text kernel
 *   subordinates formal correspondence to communicative effectiveness,
 *   explicitly treating the source text as a communicative act to be
 *   re-expressed in the receptor language's natural idiom rather than a
 *   formal structure to be replicated. Originating in Eugene Nida's
 *   missionary linguistics (1960s), it became the dominant paradigm for
 *   global Bible translation through the United Bible Societies and major
 *   evangelical publishers (NIV, NLT, CEV, GNT). The constraint operates
 *   through translation committees, publishing contracts, and denominational
 *   endorsement — active enforcement maintains the paradigm against formal
 *   equivalence alternatives. Its coordination function is genuine: millions
 *   access Scripture who could not navigate formal equivalence. Its
 *   extraction is also real: scholars lose morphological precision essential
 *   for exegesis, and the constraint's persistence depends on suppressing
 *   formal equivalence as 'pastorally inferior.' The claimed_type
 *   (tangled_rope) reflects this dual structure; the metrics capture moderate
 *   extractiveness rising over six decades as the paradigm hardened into
 *   institutional default.
 *
 * KEY AGENTS:
 *   - translation_committees: Primary agenda_setter (institutional/constrained) — sets and enforces translation policy
 *   - publishing_houses: Secondary agenda_setter / beneficiary (institutional/mobile) — controls distribution and profits
 *   - lay_readers: Primary beneficiary (organized/constrained) — receives intelligible text, limited alternatives
 *   - missionary_organizations: Primary beneficiary (organized/mobile) — drives and funds the paradigm for mission effectiveness
 *   - scholarly_community: Primary victim (organized/constrained) — bears precision loss, constrained by dominant texts
 *   - formal_equivalence_translators: Excluded (moderate/constrained) — competing philosophy marginalized
 *   - theological_educators: Observer (moderate/mobile) — navigates between paradigms pedagogically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__dynamic_equivalence_reading, 0.42).
domain_priors:suppression_score(biblical_source_text__dynamic_equivalence_reading, 0.38).
domain_priors:theater_ratio(biblical_source_text__dynamic_equivalence_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(biblical_source_text__dynamic_equivalence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__dynamic_equivalence_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__dynamic_equivalence_reading, "Dynamic Equivalence Translation Philosophy").
narrative_ontology:topic_domain(biblical_source_text__dynamic_equivalence_reading, "religious/linguistic/authority").

domain_priors:requires_active_enforcement(biblical_source_text__dynamic_equivalence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__dynamic_equivalence_reading, '5354ae85-d552-4031-8dea-17f4f36a80ca').
narrative_ontology:cs_kernel_codification('5354ae85-d552-4031-8dea-17f4f36a80ca', fixed_text).
narrative_ontology:cs_authority_grounding('5354ae85-d552-4031-8dea-17f4f36a80ca', practice).
narrative_ontology:cs_interpretation_layer_present('5354ae85-d552-4031-8dea-17f4f36a80ca').
narrative_ontology:cs_reading_relation('5354ae85-d552-4031-8dea-17f4f36a80ca', biblical_source_text__formal_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('5354ae85-d552-4031-8dea-17f4f36a80ca', biblical_source_text__critical_reconstructive_reading, coexists_with).
narrative_ontology:cs_axiom('5354ae85-d552-4031-8dea-17f4f36a80ca', foundational, communicative_effectiveness_primacy).
narrative_ontology:cs_axiom_status(communicative_effectiveness_primacy, holdable).
narrative_ontology:cs_axiom_grounding('5354ae85-d552-4031-8dea-17f4f36a80ca', communicative_effectiveness_primacy, instrumental).
narrative_ontology:cs_axiom('5354ae85-d552-4031-8dea-17f4f36a80ca', foundational, structural_fidelity_subordination).
narrative_ontology:cs_axiom_status(structural_fidelity_subordination, holdable).
narrative_ontology:cs_axiom_grounding('5354ae85-d552-4031-8dea-17f4f36a80ca', structural_fidelity_subordination, instrumental).
narrative_ontology:cs_reference_frame('5354ae85-d552-4031-8dea-17f4f36a80ca', communicative_act_model).
narrative_ontology:cs_drift_state('5354ae85-d552-4031-8dea-17f4f36a80ca', contemporary_translation_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5354ae85-d552-4031-8dea-17f4f36a80ca', '').
narrative_ontology:cs_kernel_id(biblical_source_text__dynamic_equivalence_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, lay_readers).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, missionary_organizations).
narrative_ontology:constraint_victim(biblical_source_text__dynamic_equivalence_reading, scholarly_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_source_text__dynamic_equivalence_reading, publishing_houses).
narrative_ontology:constraint_vindicates(biblical_source_text__dynamic_equivalence_reading, functional_equivalence_theory).
narrative_ontology:constraint_vindicates(biblical_source_text__dynamic_equivalence_reading, communicative_priority_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commission and oversee Bible translations using dynamic equivalence principles. They set translation policy, approve renderings, and control publication. Their authority derives from denominational or parachurch mandates. Exit means leaving institutional translation work entirely.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, translation_committees, agenda_setter,
    institutional, generational, constrained, global).

% Publish and distribute dynamic equivalence translations (NIV, NLT, CEV, GNT). They benefit commercially from accessible translations that reach mass markets. They enforce the constraint through copyright control and contractual translation standards. Can pivot to other translation philosophies if market demands shift.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, publishing_houses, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__dynamic_equivalence_reading, publishing_houses, beneficiary).

% Ordinary believers who read Scripture in vernacular translations. They receive intelligible text that requires no specialized training to understand. Their access to alternatives is constrained by what their church provides and what they can afford; switching translations means losing familiarity and community shared text.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, lay_readers, beneficiary,
    organized, biographical, constrained, global).

% Cross-cultural mission agencies that prioritize rapid, intelligible Scripture access for unreached groups. They fund and drive dynamic equivalence translations because they accelerate evangelism and church planting. They can adopt other translation philosophies but treat dynamic equivalence as operationally optimal for their mission.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, missionary_organizations, beneficiary,
    organized, generational, mobile, global).

% Biblical scholars, linguists, and exegetes who require morphological and syntactic precision for word studies, textual criticism, and historical reconstruction. Dynamic equivalence obscures structural features they depend on (verb forms, case endings, discourse markers). They bear the cost of lost precision; their exit is constrained because the dominant published translations they must engage with are dynamic equivalence.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, scholarly_community, payer,
    organized, biographical, constrained, global).

% Translators and publishers committed to formal equivalence (ESV, NASB, NKJV). They are structurally excluded from the dynamic equivalence ecosystem — their philosophy is treated as pastorally inferior by the dominant missionary-publishing complex. They would argue for structural fidelity but are kept at the margins of major translation funding and distribution channels.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, formal_equivalence_translators, excluded,
    moderate, biographical, constrained, global).

% Seminary professors and church educators who navigate between translations. They see the full structure: dynamic equivalence serves pastoral accessibility but creates pedagogical friction when students encounter original languages. They can choose which translations to teach but must engage the dominant dynamic equivalence texts their students bring.
narrative_ontology:constraint_stakeholder(biblical_source_text__dynamic_equivalence_reading, theological_educators, observer,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides intelligible Scripture access across language barriers for faith communities, enabling shared worship, discipleship, and evangelism without requiring original-language literacy.
% TRANSFER_FUNCTION: Moves morphological and syntactic precision from the translation output to the scholarly apparatus (footnotes, commentaries, original-language tools), concentrating structural fidelity in specialist resources while the main text carries communicative force.
% ABSENT_VOICES: Oral cultures and pre-literate communities for whom dynamic equivalence was originally theorized — they rarely participate in translation committees or scholarly debates. Also excluded: minority-language communities where dynamic equivalence is imposed by majority-language mission strategies without local structural fidelity needs assessment.
% DISAPPEARANCE_RATIONALE: If dynamic equivalence translations vanished overnight, global mission strategy would fracture — missionary organizations would lose their primary evangelism tool, publishing houses would lose their bestselling products, and churches would face immediate liturgical disruption. The scholarly community would gain unmediated structural access but lose the common vernacular text that enables cross-community discourse.
% FOUNDING_PROBLEM: Mid-20th century missionary linguistics (Nida) identified that formal equivalence translations were unintelligible to receptor-language communities, creating a barrier to Gospel communication that structural fidelity alone could not solve.
% FOUNDING_PROBLEM_CORROBORATION: Eugene Nida's own later writings acknowledge dynamic equivalence created new problems (loss of exegetical precision, theological flattening). Formal equivalence advocates (e.g., Leland Ryken, ESV translation committee) attest the founding problem is overstated — they argue teaching can bridge the gap. Independent linguistic anthropologists (e.g., work on Bible translation reception in oral cultures) corroborate both accessibility gains and structural losses.
narrative_ontology:disappearance_verdict(biblical_source_text__dynamic_equivalence_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_source_text__dynamic_equivalence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__dynamic_equivalence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_source_text__dynamic_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__dynamic_equivalence_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__dynamic_equivalence_reading_tests).
:- end_tests(biblical_source_text__dynamic_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the constraint transfers structural precision from main text to apparatus — a real loss for scholars but not total (original languages remain accessible). Suppression (0.38) reflects active enforcement: translation committees reject formal equivalence renderings, publishers prioritize dynamic equivalence, denominations endorse it. Theater_ratio (0.25) is low-moderate: the pastoral mission rationale is genuine, but a growing share of enforcement serves institutional inertia (copyright control, market dominance). Accessibility_collapse (0.45) is partial: formal equivalence alternatives exist (ESV, NASB) but are structurally marginalized in global distribution. Resistance (0.55) is significant: scholarly pushback, formal equivalence revival, and receptor-community feedback create friction. The measurement series on a shared grid (1960-2024, six points) shows extractiveness and suppression rising together as the paradigm institutionalized, theater creeping up as coordination rationale becomes ritualized.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seats (committees, publishers) experience this as genuine coordination they built and maintain — the constraint solves the real problem of unintelligible Scripture. The payer seat (scholarly_community) experiences the same structure as enforced extraction — their necessary precision is systematically removed and relocated to footnotes they must separately purchase. The beneficiary seats (lay_readers, missionaries) experience coordination with diffuse indirect costs. The engine computes this divergence from the structural data; the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Translation_committees and publishing_houses are structural beneficiaries (d near 0.0-0.2): they control the constraint, collect revenue and authority. Missionary_organizations are beneficiaries with mobile exit (d ~0.15): they gain operational effectiveness but can switch philosophies. Lay_readers are beneficiaries with constrained exit (d ~0.3): genuine coordination benefit, but indirect cost via theological flattening, limited switching. Scholarly_community are payers with constrained exit (d ~0.8): they bear precision loss, must engage dominant texts, cannot easily opt out. Formal_equivalence_translators are excluded (d ~0.9): their philosophy is the enforcement target. Theological_educators sit near symmetric (d ~0.5): they see both sides, can choose texts but must teach the dominant ones.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unintelligible formal equivalence) was real in 1960s missionary contexts. By 2024, receptor-language literacy and linguistic resources have expanded dramatically — the original problem is substantially mitigated. Yet the constraint persists and hardens (rising extractiveness, suppression, theater). This is mandatrophy: the mandate (make Scripture intelligible) has outlived its founding conditions, but the arrangement persists because it now serves institutional interests (publishing revenue, mission bureaucracy, denominational brand). The founding_problem_status = contested captures this: missionary organizations claim the problem is live; scholars and formal equivalence advocates claim it is dead; independent linguists see both sides.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the dynamic equivalence reading a distinct constraint instantiated from the biblical_source_text kernel, or merely an interpretive variant within a single constraint?',
    'Test ε-invariance: if evaluating ''dynamic equivalence'' via pastoral effectiveness yields low extractiveness but evaluating via scholarly precision yields high extractiveness, these are two constraints (per DP-001). The ε-invariance principle requires decomposition.',
    'If one constraint, the engine computes a single χ; if two, each reading gets its own ε, stakeholders, and classification — the formal_equivalence_reading and critical_reconstructive_reading would be separate constraint stories linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether kernel readings instantiate distinct constraints per ε-invariance.').

omega_variable(
    missionary_extraction_boundary,
    'Does the missionary organization''s benefit from dynamic equivalence constitute coordination (genuine Gospel access) or extraction (institutional growth at the expense of textual integrity)?',
    'Compare translation outcomes in contexts where missionary organizations control the process versus contexts where receptor communities control it. If receptor-controlled processes converge on dynamic equivalence, it''s coordination; if they diverge, missionary control extracts.',
    'If extraction, the constraint shifts toward snare for the scholarly_community seat; if coordination, it remains tangled_rope with genuine dual function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(missionary_extraction_boundary, empirical, 'Whether missionary benefit is genuine coordination or institutional extraction.').

omega_variable(
    scholarly_resistance_nature,
    'Is the scholarly community''s resistance to dynamic equivalence professional self-protection (gatekeeping) or a structurally necessary check on semantic drift?',
    'Track doctrinal divergence rates in communities using exclusively dynamic equivalence translations versus those with formal equivalence access, controlling for theological tradition.',
    'If resistance is gatekeeping, the victim designation is inflated; if semantic drift is measurably harmful, the victim designation is structurally warranted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scholarly_resistance_nature, preference, 'Nature of scholarly resistance: gatekeeping vs. necessary fidelity check.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__dynamic_equivalence_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t1960, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(bibl_tr_t1978, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement(bibl_tr_t1990, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(bibl_tr_t2001, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 2001, 0.22).
narrative_ontology:measurement(bibl_tr_t2011, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 2011, 0.24).
narrative_ontology:measurement(bibl_tr_t2024, biblical_source_text__dynamic_equivalence_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(bibl_be_t1960, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 1960, 0.28).
narrative_ontology:measurement(bibl_be_t1978, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 1978, 0.35).
narrative_ontology:measurement(bibl_be_t1990, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(bibl_be_t2001, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 2001, 0.4).
narrative_ontology:measurement(bibl_be_t2011, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 2011, 0.41).
narrative_ontology:measurement(bibl_be_t2024, biblical_source_text__dynamic_equivalence_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t1960, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 1960, 0.25).
narrative_ontology:measurement(bibl_su_t1978, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 1978, 0.32).
narrative_ontology:measurement(bibl_su_t1990, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(bibl_su_t2001, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 2001, 0.37).
narrative_ontology:measurement(bibl_su_t2011, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 2011, 0.38).
narrative_ontology:measurement(bibl_su_t2024, biblical_source_text__dynamic_equivalence_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__dynamic_equivalence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_source_text__dynamic_equivalence_reading, 0.08).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__dynamic_equivalence_reading, biblical_source_text__critical_reconstructive_reading).

% DUAL FORMULATION NOTE:
% The biblical_source_text kernel decomposes into three constraint stories: this dynamic_equivalence_reading (tangled_rope, moderate ε, beneficiaries=lay_readers/missionaries, victims=scholars), formal_equivalence_reading (likely rope or mountain, low ε, beneficiaries=scholars/traditionalists, victims=lay_readers in low-literacy contexts), and critical_reconstructive_reading (likely scaffold or rope, ε varies by textual certainty, beneficiaries=textual_critics, victims=communities needing stable text). They are linked because each reading cites the kernel's authority while structurally contradicting the others' operational priorities. The upstream critical_reconstructive_reading influences both downstream translation readings (textual basis constrains translation options).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_source_text__dynamic_equivalence_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
