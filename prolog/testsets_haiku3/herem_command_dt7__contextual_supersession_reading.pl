% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__contextual_supersession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__contextual_supersession_reading, []).

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
 *   constraint_id: herem_command_dt7__contextual_supersession_reading
 *   human_readable: Herem Command (DT 7) — Contextual Supersession Reading
 *   domain: religious/ethical/hermeneutical
 *
 * SUMMARY:
 *   This constraint instantiates the contextual-supersession reading of the
 *   herem command (Deuteronomy 7:1–2), which prescribes the complete
 *   destruction and assimilation prohibition of Canaanite populations during
 *   Israel's settlement. Under this reading, herem is a historically
 *   contingent directive issued to a particular people at a particular
 *   moment—the Iron Age settlement consolidation—and is morally superseded by
 *   later textual layers emphasizing universal covenant membership (prophetic
 *   universalism, Christian new covenant theology). The reading does NOT deny
 *   the text's canonical status; it RELOCATES moral authority by staging a
 *   developmental trajectory within the tradition itself: from ethnic
 *   boundary-enforcement to ethical universalism. Contemporary enforcement of
 *   herem-based separation (whether ethnic or religious fundamentalism) is
 *   treated as a misapplication of a time-bounded directive, making it a
 *   constraint on those who enforce it (they pay the cost of moral
 *   anachronism and legal consequence) rather than a coordination mechanism
 *   for the tradition. The claimed type is Scaffold because the reading
 *   explicitly stages herem as transitional infrastructure—necessary at t0,
 *   delegitimated at t1+ by the emergence of competing authority (prophetic
 *   and Christian readings). The kernel contest itself (three sibling
 *   readings with incompatible foundational claims) is the defining feature
 *   of this constraint story.
 *
 * KEY AGENTS:
 *   - ancient_israel_settlement_cohort — beneficiary of the original directive's coordination function at t0
 *   - later_jewish_communities — inheritors and reinterpreters, managing the tradition's internal tensions
 *   - early_christian_communities — agenda-setters who formalized the supersession reading and codified it as doctrine
 *   - fundamentalist_enforcement_practitioners — contemporary payers who bear the cost of treating time-bound directive as durable
 *   - textual_scholarship_community — observers who provide empirical grounding for dating, composition history, and comparative context
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__contextual_supersession_reading, 0.31).
domain_priors:suppression_score(herem_command_dt7__contextual_supersession_reading, 0.28).
domain_priors:theater_ratio(herem_command_dt7__contextual_supersession_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(herem_command_dt7__contextual_supersession_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__contextual_supersession_reading, scaffold).
narrative_ontology:human_readable(herem_command_dt7__contextual_supersession_reading, "Herem Command (DT 7) — Contextual Supersession Reading").
narrative_ontology:topic_domain(herem_command_dt7__contextual_supersession_reading, "religious/ethical/hermeneutical").

narrative_ontology:has_sunset_clause(herem_command_dt7__contextual_supersession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__contextual_supersession_reading, '74bd9634-229c-4940-b87b-5e93d4f20e78').
narrative_ontology:cs_kernel_codification('74bd9634-229c-4940-b87b-5e93d4f20e78', fixed_text).
narrative_ontology:cs_authority_grounding('74bd9634-229c-4940-b87b-5e93d4f20e78', lineage).
narrative_ontology:cs_interpretation_layer_present('74bd9634-229c-4940-b87b-5e93d4f20e78').
narrative_ontology:cs_reading_relation('74bd9634-229c-4940-b87b-5e93d4f20e78', herem_command_dt7__durable_separation_reading, coexists_with).
narrative_ontology:cs_reading_relation('74bd9634-229c-4940-b87b-5e93d4f20e78', herem_command_dt7__allegorical_displacement_reading, influences).
narrative_ontology:cs_axiom('74bd9634-229c-4940-b87b-5e93d4f20e78', foundational, herem_historically_bounded_directive).
narrative_ontology:cs_axiom_status(herem_historically_bounded_directive, holdable).
narrative_ontology:cs_axiom_grounding('74bd9634-229c-4940-b87b-5e93d4f20e78', herem_historically_bounded_directive, empirically_contingent).
narrative_ontology:cs_axiom('74bd9634-229c-4940-b87b-5e93d4f20e78', foundational, ethical_universalism_supersedes_ethnic_particularism).
narrative_ontology:cs_axiom_status(ethical_universalism_supersedes_ethnic_particularism, holdable).
narrative_ontology:cs_axiom_grounding('74bd9634-229c-4940-b87b-5e93d4f20e78', ethical_universalism_supersedes_ethnic_particularism, deontological).
narrative_ontology:cs_reference_frame('74bd9634-229c-4940-b87b-5e93d4f20e78', iron_age_settlement_coordination).
narrative_ontology:cs_drift_state('74bd9634-229c-4940-b87b-5e93d4f20e78', contemporary_interfaith_pluralism, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('74bd9634-229c-4940-b87b-5e93d4f20e78', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__contextual_supersession_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, ancient_israel_settlement_cohort).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, fundamentalist_enforcement_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(herem_command_dt7__contextual_supersession_reading, later_jewish_communities).
narrative_ontology:constraint_victim(herem_command_dt7__contextual_supersession_reading, later_jewish_communities).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, prophetic_universalism_doctrine).
narrative_ontology:constraint_vindicates(herem_command_dt7__contextual_supersession_reading, christian_covenant_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The historical addressees of the herem command during the Iron Age settlement period (roughly 1200–1000 BCE). They faced existential threats from surrounding populations and operated under survival constraints. The herem framing provided both military justification and theological coherence for tribal consolidation. From this reading's perspective, they were recipients of a time-bound directive appropriate to their specific historical moment.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, ancient_israel_settlement_cohort, beneficiary,
    institutional, generational, analytical, local).

% Post-exilic and later communities that inherited the herem texts but interpreted them through prophetic frameworks that shifted emphasis from ethnic boundary maintenance to ethical behavior and covenant relation. They carried both the original texts and the interpretive overlay that superseded the original command's literal application. They benefited from access to the full textual tradition while paying the cost of managing its internal tensions.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, later_jewish_communities, beneficiary,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__contextual_supersession_reading, later_jewish_communities, payer).

% Developed and codified the contextual supersession reading explicitly: herem as a historically contingent directive replaced by a new covenant (new testament) that universalizes membership criteria from ethnic/territorial to faith-based. They authored the interpretive framework that relocated moral authority from the original command to the new ethical standard.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, early_christian_communities, agenda_setter,
    organized, generational, mobile, regional).

% Modern interpreters and communities that treat the herem command as durable, literal, and binding across time—reading it as a mandate for contemporary ethnic or religious boundaries and justifying exclusion or violence toward designated outsiders. From the supersession reading's perspective, they are the constraint's victims: they bear the cost of misapplying a time-bound directive to contemporary contexts, and face moral and legal consequences for acting on that misapplication.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, fundamentalist_enforcement_practitioners, payer,
    moderate, biographical, identity_locked, local).

% Individuals and groups targeted by fundamentalist enforcement of herem-based separation rules (prohibitions on intermarriage, cultural mixing, or relationship across ethnic/religious lines). They are excluded from the supersession reading's direct authority conversation but would be the primary beneficiaries of its adoption: they would transition from being targets of separation mandates to being treated as full covenant members under universalizing frameworks.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, intermarriage_and_assimilation_communities, excluded,
    powerless, biographical, trapped, local).

% Biblical scholars, historians, and comparative religionists who examine the herem command's historical context, authorship, redaction history, and relationship to surrounding ancient Near Eastern conquest narratives. They provide empirical evidence about dating, authorial intent, and the textual strata that complicate the durable-mandate reading.
narrative_ontology:constraint_stakeholder(herem_command_dt7__contextual_supersession_reading, textual_scholarship_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__contextual_supersession_reading, diffuse).
narrative_ontology:fixing_cost_class(herem_command_dt7__contextual_supersession_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided theological and operational coherence for a tribal society under existential threat during a particular historical moment (Iron Age settlement consolidation). The herem framing coordinated military action, boundary maintenance, and tribal identity formation into a unified cosmological narrative.
% TRANSFER_FUNCTION: Moves interpretive authority from a literal ethnic-separation mandate to a time-contextualized, ethically-superseded framework. Under the contextual supersession reading, moral legitimacy transfers FROM the original command TO the new universal covenant; obedience transfers FROM literal herem enforcement TO belief-based inclusion criteria. Contemporary fundamentalist enforcement transfers authority BACK toward the original command, incurring moral and legal cost.
% ABSENT_VOICES: Voices of those historically targeted by herem-based exclusion who did not participate in the redefinition (conquered Canaanite populations, diaspora Jews assimilated into surrounding cultures, interfaith couples under contemporary fundamentalist enforcement). They are structurally excluded from the authority structures (ancient monarchic, later rabbinical, Christian ecclesiastical) that authored the supersession reading.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and the durable-separation reading became canonical, contemporaneous enforcement of ethnic or religious boundaries justified by herem would expand, affecting diaspora communities and interfaith families. If the allegorical reading became canonical instead, the constraint would persist textually but lose performative force (the text would be spiritualized away from literal application). The reading itself does not disappear from textual tradition—it is an irreversible interpretive layer—but its authority as the governing framework could be displaced by competing readings.
% FOUNDING_PROBLEM: How to interpret a biblical command (Deuteronomy 7) that prescribes genocide and ethnic separation in light of later textual strata that emphasize universal divine compassion, covenant inclusion beyond ethnic boundaries, and prophetic critique of ethnic chosenness.
% FOUNDING_PROBLEM_CORROBORATION: Textual scholars (both Jewish and Christian) document the compositional history of Deuteronomy and track the development of universalizing themes in later prophetic books (Jonah, Isaiah 40–55) and New Testament writings. Interfaith advocacy communities and victims of contemporary herem-based enforcement attest to the live tension between literal and superseded readings. Conservative scholars and fundamentalist communities attest the founding problem persists because the original command remains canonical text, not superseded by later layers.
narrative_ontology:disappearance_verdict(herem_command_dt7__contextual_supersession_reading, contested).
narrative_ontology:founding_problem_status(herem_command_dt7__contextual_supersession_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__contextual_supersession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(herem_command_dt7__contextual_supersession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__contextual_supersession_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__contextual_supersession_reading_tests).
:- end_tests(herem_command_dt7__contextual_supersession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics encode the reading's narrative arc. At t0 (settlement period), extractiveness is near-maximal (0.85) because the directive's full literal force is applied—genocide and categorical separation. Suppression is high (0.72) because enforcing such a program requires active coercion. By t625 (exilic period), extractiveness drops sharply (0.68) as the Babylonian exile itself disrupts territorial control and subsequent prophetic reinterpretation begins. The exilic and post-exilic periods witness the composition of prophetic universalism (Isaiah 40–55, the book of Jonah) within the canon itself—a delegitimation-from-within. Theater ratio rises (0.18–0.35) as the constraint transitions from enforced literal practice to interpretive performance: communities maintain textual attachment while reorienting its meaning. By t2500 (contemporary), extractiveness has stabilized at 0.31 because only fundamentalist enforcement practitioners treat herem as binding, and they are a minority seat; the mainstream reading (Jewish and Christian) has shifted the moral authority elsewhere. Theater ratio (0.42) reflects the constraint's persistence as interpretive theater: the text remains canonically authoritative but its application is contested and mostly redirected to allegorical or superseded status. Suppression (0.28) tracks residual enforcement by fundamentalists against interfaith communities, but lacks the institutional backing it once commanded. The measurement series uses one shared time grid; each metric is authored at every examined epoch.
 *
 * DIRECTIONALITY LOGIC:
 *   The ancient_israel_settlement_cohort occupies the structural beneficiary seat: they received a directive that solved their existential coordination problem. But this is a retrospective classification—the reading itself does not assert they should benefit today, only that they did at t0. Later communities (Jewish and Christian) occupy a complex seat: they benefit from the full canonical tradition (access to the text, the interpretive heritage, the community coherence around shared scripture) while paying the cost of managing the internal contradiction between literal herem and universalizing layers. The early_christian_communities are agenda-setters in formalizing the supersession. The fundamental_enforcement_practitioners are the constraint's contemporary targets: they pay through moral delegitimation, legal jeopardy, and social cost for treating time-bound directive as durable. Intermarriage and assimilation communities are excluded from the direct authority conversation but would transition from being excluded targets (under durable-separation reading) to included members (under supersession reading). The directionality derives from structural role, not from whether the reading is 'true': even if the reading is correct, beneficiaries and payers have different structural relationships to it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to interpret herem in light of later universalizing layers) is live and contested: the three sibling readings represent live, incompatible positions held by different communities. The disappearance verdict is contested because the three readings are incommensurable—if the supersession reading vanished, the durable-separation and allegorical readings would compete for authority, reshaping the constraint entirely. The mandatrophy question is whether the founding problem still demands the herem text's inclusion in the canon at all, or whether the universalizing layers constitute a sufficient answer that permits herem to be stored as historical text without moral authority. The supersession reading sidesteps full mandatrophy by keeping herem in the canon but relocating its moral force; it does not resolve the mandatrophy, but it defers it through interpretive layering. This is the characteristic move of Scaffold constraints: they stage a transition but do NOT guarantee the underlying contradiction disappears.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_composition_ambiguity,
    'What is the chronological and literary relationship between the herem command (D source, likely 7th century BCE or later) and the universalizing prophetic texts (exilic and post-exilic, 6th century BCE onward)? Are they reflections of real diachronic theological development, or editorial layering imposed retrospectively?',
    'Redaction-critical and source-critical analysis: linguistic markers, theological vocabulary evolution, comparative Near Eastern composition history, and archaeological context of religious practice change.',
    'If the universalizing layers are demonstrably later and respond polemically to herem-style enforcement, the supersession reading gains empirical grounding. If composition is more complex (interleaving, nested redaction), the reading''s narrative arc becomes less clear and the conceptual-supersession frame weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_composition_ambiguity, empirical, 'Whether later universalizing texts consciously supersede earlier herem directives or represent independent traditions.').

omega_variable(
    identity_lock_mechanism_in_fundamentalism,
    'For fundamentalist enforcement practitioners, is the durable-separation reading constitutive of their identity (religious, ethnic, communal)—i.e., would abandoning the supersession reading and accepting the contextual reading require a reconstruction of core identity? Or is it a contingent belief they hold but could revise without identity rupture?',
    'Phenomenological interviews with practitioners who have moved between readings; analysis of what identity elements they report as needing reconstruction during such moves; observation of whether exit from the durable-separation reading requires institutional or communal departure.',
    'If the reading is identity-constitutive (identity_locked exit), the constraint on fundamentalist practitioners is structurally deeper; suppression becomes partly internalized. If contingent, exit is mobile and the reading can change without identity cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_fundamentalism, empirical, 'Whether durable-separation reading is identity-fused for practitioners or revisable belief.').

omega_variable(
    prophetic_universalism_grounding_contested,
    'Is prophetic universalism (Jonah, Isaiah 40–55, etc.) internally motivated by genuine theological development within the tradition, or is it imposed by exilic communities seeking to reconcile defeat with divine justice (theodicy argument masquerading as universalism)?',
    'Comparative theology: examine whether universal divine compassion appears in ancient Near Eastern religious parallels (Egyptian, Mesopotamian, Hittite) that exilic Jewish communities encountered; trace theological concepts (divine justice, covenant inclusion, forgiveness) to their sources and motivations.',
    'If universalism is internally motivated, the contextual supersession reading gains moral-not-just-political authority. If it is theodicy cover-story, the reading becomes tactically adaptive rather than ethically motivated—which affects whether contemporary adoption of the reading is an endorsement or an adoption of cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prophetic_universalism_grounding_contested, conceptual, 'Whether prophetic universalism represents genuine ethical development or crisis-driven reinterpretation.').

omega_variable(
    christian_covenant_displacement_vs_coexistence,
    'Does Christian new covenant theology genuinely REPLACE the old covenant''s ethnic-boundary framework (forecloses durable-separation reading), or does it merely layer a new interpretive lens while keeping the old covenant''s literal text in canonical tension?',
    'Examination of Christian practice: How do Christian communities handle Jewish ethnic identity, intermarriage, and covenant membership? Do they enforce new-covenant-only boundaries, or permit dual-covenant theology? Survey of major Christian traditions'' formal teaching on old covenant status.',
    'If replacement is genuine foreclosure, the durable-separation and contextual-supersession readings are logically incompatible within a Christian framework. If layering permits coexistence, both readings remain live and the constraint persists as unresolved kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(christian_covenant_displacement_vs_coexistence, conceptual, 'Whether new covenant theology forecloses or merely recontextualizes old covenant boundaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__contextual_supersession_reading, 0, 2500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(herem_theater_t0_settlement_period, herem_command_dt7__contextual_supersession_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(herem_theater_t0_settlement_period, projected).
narrative_ontology:measurement(herem_theater_t625_exilic_period, herem_command_dt7__contextual_supersession_reading, theater_ratio, 625, 0.18).
narrative_ontology:measurement_basis(herem_theater_t625_exilic_period, observed).
narrative_ontology:measurement(herem_theater_t1250_rabbinic_period, herem_command_dt7__contextual_supersession_reading, theater_ratio, 1250, 0.35).
narrative_ontology:measurement_basis(herem_theater_t1250_rabbinic_period, observed).
narrative_ontology:measurement(herem_theater_t1750_medieval_interpretation, herem_command_dt7__contextual_supersession_reading, theater_ratio, 1750, 0.38).
narrative_ontology:measurement_basis(herem_theater_t1750_medieval_interpretation, observed).
narrative_ontology:measurement(herem_theater_t2250_contemporary_resurgence, herem_command_dt7__contextual_supersession_reading, theater_ratio, 2250, 0.41).
narrative_ontology:measurement_basis(herem_theater_t2250_contemporary_resurgence, observed).
narrative_ontology:measurement(herem_theater_t2500_current, herem_command_dt7__contextual_supersession_reading, theater_ratio, 2500, 0.42).
narrative_ontology:measurement_basis(herem_theater_t2500_current, observed).

% Extraction over time
narrative_ontology:measurement(herem_extractiveness_t0_settlement_period, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement_basis(herem_extractiveness_t0_settlement_period, projected).
narrative_ontology:measurement(herem_extractiveness_t625_exilic_period, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 625, 0.68).
narrative_ontology:measurement_basis(herem_extractiveness_t625_exilic_period, observed).
narrative_ontology:measurement(herem_extractiveness_t1250_rabbinic_period, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 1250, 0.42).
narrative_ontology:measurement_basis(herem_extractiveness_t1250_rabbinic_period, observed).
narrative_ontology:measurement(herem_extractiveness_t1750_medieval_interpretation, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 1750, 0.28).
narrative_ontology:measurement_basis(herem_extractiveness_t1750_medieval_interpretation, observed).
narrative_ontology:measurement(herem_extractiveness_t2250_contemporary_resurgence, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 2250, 0.31).
narrative_ontology:measurement_basis(herem_extractiveness_t2250_contemporary_resurgence, observed).
narrative_ontology:measurement(herem_extractiveness_t2500_current, herem_command_dt7__contextual_supersession_reading, base_extractiveness, 2500, 0.31).
narrative_ontology:measurement_basis(herem_extractiveness_t2500_current, observed).

% Suppression requirement over time
narrative_ontology:measurement(herem_suppression_t0_settlement_period, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(herem_suppression_t0_settlement_period, projected).
narrative_ontology:measurement(herem_suppression_t625_exilic_period, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 625, 0.38).
narrative_ontology:measurement_basis(herem_suppression_t625_exilic_period, observed).
narrative_ontology:measurement(herem_suppression_t1250_rabbinic_period, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 1250, 0.22).
narrative_ontology:measurement_basis(herem_suppression_t1250_rabbinic_period, observed).
narrative_ontology:measurement(herem_suppression_t1750_medieval_interpretation, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 1750, 0.18).
narrative_ontology:measurement_basis(herem_suppression_t1750_medieval_interpretation, observed).
narrative_ontology:measurement(herem_suppression_t2250_contemporary_resurgence, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 2250, 0.28).
narrative_ontology:measurement_basis(herem_suppression_t2250_contemporary_resurgence, observed).
narrative_ontology:measurement(herem_suppression_t2500_current, herem_command_dt7__contextual_supersession_reading, suppression_requirement, 2500, 0.28).
narrative_ontology:measurement_basis(herem_suppression_t2500_current, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__contextual_supersession_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(herem_command_dt7__contextual_supersession_reading, 0.12).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__contextual_supersession_reading, herem_command_dt7__allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% The herem command (Deuteronomy 7) is a contested kernel with three structurally distinct constraint instantiations. The contextual_supersession_reading stages herem as historically-bounded scaffold, morally superseded by prophetic universalism and Christian covenant theology; it authorizes universal membership and low contemporary extractiveness. The durable_separation_reading treats herem as timeless divine mandate for identity preservation and categorical separation; it authorizes eternal ethnic/religious boundaries and high extractiveness for those coerced by enforcement. The allegorical_displacement_reading spiritualizes herem into internal moral warfare against sin rather than external conquest; it permits allegorical reuse while denying literal application. All three are live readings held by different communities. They share a referent (the Deuteronomy 7 text) but instantiate incompatible ε values because they disagree on the text's moral authority and temporal scope. Each reading is a separate constraint story with separate structural data; they are linked through the kernel contest (network.affects_constraints) and through omega variables documenting the interpretive disagreement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(herem_command_dt7__contextual_supersession_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
