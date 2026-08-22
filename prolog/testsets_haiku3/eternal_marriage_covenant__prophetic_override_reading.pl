% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__prophetic_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__prophetic_override_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: eternal_marriage_covenant__prophetic_override_reading
 *   human_readable: Prophetic Override Authority for Doctrine Revision
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   The Church of Jesus Christ of Latter-day Saints (LDS) doctrine Doctrine
 *   and Covenants section 132 established polygamy as an eternal covenant
 *   required for the highest exaltation. Between 1880 and 1890, federal
 *   pressure—statute, prosecution of leaders, and seizure of church
 *   assets—created an institutional survival crisis. In 1890, church
 *   president Wilford Woodruff issued the Manifesto, claiming a new
 *   revelation that suspended the practice of polygamy while asserting the
 *   eternal doctrine remained valid. This constraint story instantiates the
 *   *prophetic override reading*: the authority of a living prophet to
 *   receive continuing revelation that supersedes prior revelation when
 *   circumstances (here: federal legal pressure and institutional dissolution
 *   threat) require it. The reading is one of three coherent readings of the
 *   eternal marriage covenant kernel; the others are the
 *   immutable_commandment_reading (D&C 132 is eternally binding,
 *   unchangeable) and the temporal_accommodation_reading (the Manifesto is
 *   obedience to civil law without doctrinal change, not a new revelation).
 *   This constraint operationalizes the override mechanism itself: who has
 *   authority to change doctrine, under what pressure, and at what cost to
 *   those bound by the prior doctrine.
 *
 * KEY AGENTS:
 *   - institutional_church_leadership: agenda-setter, holds prophetic authority to declare new revelation (power: institutional, exit: arbitrage)
 *   - practicing_polygamists: payers, spiritually mandated to live the covenant under prior doctrine, now commanded to cease (power: organized, exit: identity_locked)
 *   - fundamentalist_dissidents: payers and excluded, reject the override as false revelation, excommunicated and persecuted (power: powerless, exit: trapped)
 *   - federal_government: secondary agenda-setter, applies coercive pressure that activates the override mechanism (power: institutional, exit: analytical)
 *   - church_membership_base: beneficiaries, receive institutional stability and legal recognition without bearing the direct cost (power: moderate, exit: constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, 0.68).
domain_priors:suppression_score(eternal_marriage_covenant__prophetic_override_reading, 0.71).
domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__prophetic_override_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__prophetic_override_reading, "Prophetic Override Authority for Doctrine Revision").
narrative_ontology:topic_domain(eternal_marriage_covenant__prophetic_override_reading, "religious_law/political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__prophetic_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__prophetic_override_reading, '7e4e35f3-8168-436d-871f-c32b74e2e817').
narrative_ontology:cs_kernel_codification('7e4e35f3-8168-436d-871f-c32b74e2e817', formalized).
narrative_ontology:cs_authority_grounding('7e4e35f3-8168-436d-871f-c32b74e2e817', lineage).
narrative_ontology:cs_interpretation_layer_present('7e4e35f3-8168-436d-871f-c32b74e2e817').
narrative_ontology:cs_reading_relation('7e4e35f3-8168-436d-871f-c32b74e2e817', eternal_marriage_covenant__immutable_commandment_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e4e35f3-8168-436d-871f-c32b74e2e817', eternal_marriage_covenant__temporal_accommodation_reading, influences).
narrative_ontology:cs_axiom('7e4e35f3-8168-436d-871f-c32b74e2e817', foundational, continuing_revelation_doctrine).
narrative_ontology:cs_axiom_status(continuing_revelation_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('7e4e35f3-8168-436d-871f-c32b74e2e817', continuing_revelation_doctrine, theological).
narrative_ontology:cs_axiom('7e4e35f3-8168-436d-871f-c32b74e2e817', foundational, prophetic_authority_supersedes_prior_mandate).
narrative_ontology:cs_axiom_status(prophetic_authority_supersedes_prior_mandate, holdable).
narrative_ontology:cs_axiom_grounding('7e4e35f3-8168-436d-871f-c32b74e2e817', prophetic_authority_supersedes_prior_mandate, deontological).
narrative_ontology:cs_reference_frame('7e4e35f3-8168-436d-871f-c32b74e2e817', eternal_polygamy_covenant_mandatory).
narrative_ontology:cs_drift_state('7e4e35f3-8168-436d-871f-c32b74e2e817', post_manifesto_institutional_survival, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7e4e35f3-8168-436d-871f-c32b74e2e817', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, institutional_church_leadership).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, practicing_polygamists).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, fundamentalist_dissidents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_membership_base).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims authority to receive continuing revelation from deity that supersedes prior doctrine when institutional survival or legal compliance requires it. In 1890, leadership invoked this authority to suspend the practice of polygamy (while maintaining the doctrine as eternally valid) in response to federal prosecution, land seizure, and existential institutional threat. Controls the doctrinal interpretation apparatus and the definition of what constitutes a legitimate revelation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, institutional_church_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Were required to live the polygamous covenant as essential to eternal salvation under prior doctrine. The prophetic override constraint requires them to cease the practice and accept celibacy or monogamy as obedient response to the new revelation. Their religious identity and salvation narrative are constituted through the practice; exit means renouncing the faith or accepting spiritual demotion. Many did not accept the override and split into fundamentalist sects.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, practicing_polygamists, payer,
    organized, generational, identity_locked, global).

% Rejected the prophetic override as false revelation, holding that D&C 132 remains eternally binding. They are excommunicated and persecuted by the institutional church, yet remain bound to the same theological tradition that produces the override authority. They have no institutional mechanism to contest the override from within; their only recourse is separation into fundamentalist movements that claim true continuity with prior doctrine.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, fundamentalist_dissidents, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__prophetic_override_reading, fundamentalist_dissidents, observer).

% Applies legal pressure against polygamy through statute, prosecution of leaders, and land seizure. Does not directly benefit from the prophetic override, but creates the existential institutional pressure that activates the church's override authority. The constraint's functioning requires federal coercion to make the override cost-bearing for the institutional leadership.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, federal_government, agenda_setter,
    institutional, biographical, analytical, national).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__prophetic_override_reading, federal_government, beneficiary).

% Receives institutional stability, legal recognition, and social normalization that the prophetic override enables. The constraint allows the church to comply with U.S. law while retaining theological coherence and institutional continuity. They benefit from the church's survival without bearing the direct cost of doctrine revision; the cost falls on those whose practice the override suspends.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_membership_base, beneficiary,
    moderate, biographical, constrained, global).

% Assess whether the prophetic override represents genuine doctrine change or performative compliance. Their judgment affects the legitimacy of the constraint and the likelihood of renewed persecution. They represent the external authority whose pressure the override mechanism operationalizes.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, legal_and_political_authorities, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__prophetic_override_reading, institutional_church_leadership).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__prophetic_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% FOUNDING_PROBLEM: The church faced an existential choice in 1890: either renounce the polygamy doctrine established in D&C 132 (requiring doctrinal authority to supersede prior revelation) or accept institutional dissolution under federal law. The prophetic override mechanism provided a way to do both simultaneously—suspend practice while maintaining doctrinal validity, allowing the church to survive federal pressure without admitting doctrinal error.
% FOUNDING_PROBLEM_CORROBORATION: The institutional church attests the founding problem as ongoing: federal pressure persisted through 1920, and the override mechanism remained necessary to hold doctrine and practice in tension. Federal historical records and legal testimony establish the pressure. Fundamentalist dissidents and historians outside the benefiting parties attest differently: the override was motivated by institutional self-preservation, not divine guidance, and it has become a permanent mechanism for doctrinal revision by fiat rather than a response to an emergency.
narrative_ontology:founding_problem_status(eternal_marriage_covenant__prophetic_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__prophetic_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eternal_marriage_covenant__prophetic_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__prophetic_override_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 at interval end because the constraint's primary function is to transfer the burden of doctrine revision onto practicing polygamists and dissidents while preserving institutional leadership authority and theological coherence for the general membership. The constraint extracts from those whose practice was spiritually mandated; it does not extract from those who never practiced or whose faith rests on other doctrines. Suppression is higher (0.71) because the constraint's persistence depends on actively silencing dissent—excommunicating those who reject the override, suppressing fundamentalist movements, and maintaining doctrinal authority over who can legitimately claim prophetic standing. Theater is moderate (0.42): the Manifesto invokes genuine theological mechanism (continuing revelation), but increasingly the enforcement activity (persecution of fundamentalists, doctrinal policing) operates to suppress alternatives rather than to administer real revelation. The measurement trajectory shows a sharp rise in extractiveness and suppression from 1880 to 1890 (when the crisis peaked and the override was issued), then stabilization—the constraint reaches its equilibrium by 1900 and remains there as fundamentalists separate and dissent is institutionally managed. Accessibility collapse is moderate (0.62) because the override authority (the prophet's claim to receive new revelation) is presented as natural law (divine communication), but alternatives remain intellectually coherent—one can hold that the override is false revelation and remain within the theological tradition (the fundamentalist path). Resistance is moderate-high (0.58) because the constraint meets real resistance from practicing polygamists and dissidents, but the institutional church's control of the authority apparatus and the federal government's backing make resistance costly and ultimately ineffectual at changing the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The institutional leadership seat and the practicing-polygamist seat should compute to different constraint types. From the leadership perspective, the override is genuine coordination—it solves the impossible problem of maintaining both eternal doctrine and institutional survival, a real collective-action problem. From the practicing-polygamist perspective, the same mechanism operates as enforced extraction: they lose the spiritual mandate they were taught was essential to exaltation, with no alternative path within the tradition they were socialized into. The engine should compute a rope-or-coordination type from the leadership seat (beneficiary, arbitrage exit, institutional power) and a snare-or-extraction type from the polygamist seat (victim, identity-locked exit, organized power). The asymmetry in exit options (arbitrage for leadership, identity_locked for polygamists) drives directionality divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership sits near d=0.0 (full beneficiary): they exercise the override authority, retain institutional control and legitimacy, and face no direct cost—they have arbitrage exit (they could step down, but institutional leadership is the only seat they occupy). Practicing polygamists sit near d=1.0 (full target): they bear the direct cost (loss of spiritual mandate, social stigma, family dissolution), have identity-locked exit (their entire faith identity is constituted through the doctrine and community), and cannot exit without spiritual self-annihilation. Fundamentalist dissidents sit at d=0.95 (near-full target): they are actively suppressed through excommunication and persecution, have trapped exit (they cannot participate in the institutional structure that produces authority, yet remain bound to the tradition), and powerless (they lack institutional standing to contest the override). The federal government sits at d=0.3 (mild target): it bears enforcement costs (maintaining pressure, managing the legal regime), but benefits indirectly from the override (institutional compliance with law, social stability). Church membership sits near d=0.45 (symmetric): genuine coordination benefit (institutional survival, legal recognition, theological coherence) balanced against indirect cost (they benefit from the extraction that falls on others, and they accept a doctrine-practice gap without the explicating authority the leadership reserves to itself).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy in the strict sense (a mandate that has outlived its original function and persists by institutional inertia). The founding problem—federal pressure and institutional survival crisis—is authoritatively declared as 'live' by the institutional leadership: they continue to assert that the override was divinely mandated because the federal threat remained present through 1920 and because polygamy continues as a doctrinal ideal (even if suspended in practice). However, the constraint does exhibit a form of functional drift: the override mechanism was originally invoked as a one-time response to an emergency; it has become a permanent feature of the doctrinal apparatus, used subsequently to justify other revisions (e.g., removal of racial restrictions on priesthood in 1978, changes to temple ceremonies) that were not under federal pressure. The constraint thus transitions from emergency coordination (solving the incompatibility of two non-negotiable mandates) to a structural mechanism for indefinite doctrinal revision by institutional fiat. The commentary on mandatrophy should note this drift: the founding problem (federal survival crisis) remains declared as live, but the constraint's actual operation extends far beyond that problem, suggesting that the override authority is now weaponized for institutional control rather than genuine emergency resolution. An omega variable should capture the ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    override_authority_legitimacy,
    'What constitutes a legitimate prophetic override? Does federal legal pressure (non-theological pressure) justify a prophet in declaring new revelation?',
    'Doctrinal analysis of the prophetic override mechanism: does the LDS tradition establish criteria for when a prophet can supersede prior revelation? Are those criteria met in 1890? Alternatively, historical analysis of whether the Manifesto was presented as divine revelation or as pragmatic accommodation.',
    'If the override requires theological grounds and federal pressure is insufficient, the Manifesto may be reclassified as a policy change rather than a genuine revelation, which undermines the prophetic_override_reading and strengthens the temporal_accommodation_reading. If federal pressure is sufficient, the override authority becomes conditional on institutional survival rather than on doctrinal necessity, suggesting extraction rather than genuine revelation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(override_authority_legitimacy, conceptual, 'The criterion for legitimate prophetic override and whether the Manifesto meets it.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.71) structural (external barriers: legal consequences, institutional exclusion) or internalized (the target believes they deserve the suppression because it is divinely mandated)?',
    'Post-exit trajectory analysis: do practicing polygamists and fundamentalist dissidents who physically leave the institutional structure continue to experience identity shame, guilt, and self-blame? Do second-generation children of separated fundamentalists retain faith in the overridden doctrine despite social distance from the institutional church?',
    'If suppression is predominantly structural, the constraint''s effective suppression is 0.71 and exits become more mobile once institutional barriers are removed. If internalized, the target carries the suppression identity-forward and may re-affiliate with fundamentalist movements even decades later, suggesting the constraint''s effective suppression exceeds the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression in the override mechanism.').

omega_variable(
    doctrine_practice_sustainability,
    'Can the doctrine-practice distinction—eternal polygamy doctrine suspended in practice—remain coherent indefinitely, or does the gap itself require periodic re-assertion?',
    'Longitudinal analysis of LDS doctrinal discourse: how often does the leadership re-assert the eternal validity of polygamy doctrine? Does the frequency of re-assertion increase or decrease over time? Does the gap create pressure for either doctrinal abandonment or practice restoration?',
    'If the gap requires periodic re-assertion, the constraint is partially theatrical—it persists because the authority structure must continuously perform the override. If the gap stabilizes and the doctrine becomes dormant, the constraint transitions to a piton (the doctrine is intact but functionless, maintained only for historical continuity). This affects the theater_ratio trajectory and the long-term classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_sustainability, empirical, 'The sustainability of the doctrine-practice gap and its relationship to institutional performance.').

omega_variable(
    federal_pressure_conditionality,
    'Is the constraint''s persistence conditional on ongoing federal pressure, or has it become independent of external pressure?',
    'Historical analysis of the constraint''s enforcement intensity relative to federal pressure timeline: does suppression of fundamentalism correlate with federal activity, or is it decoupled? Does the institutional church relax the override if federal pressure disappears?',
    'If the constraint is conditional on federal pressure, it is technically a scaffold (intended as transitional response to emergency) that has persisted beyond its founding problem. If it is independent, it is self-perpetuating and should be classified differently. This directly affects mandatrophy analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_pressure_conditionality, empirical, 'Whether the override constraint is conditional on external federal pressure or self-perpetuating.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the prophetic_override_reading logically foreclose the immutable_commandment_reading, or can both coexist within the same faith tradition?',
    'Theological analysis: if a prophet can receive new revelation superseding prior revelation, does this logically eliminate the possibility that prior revelation was immutably binding? Or can one hold both that D&C 132 was binding at the time it was given AND that the prophet could legitimately supersede it later?',
    'If foreclosure obtains, the two readings are incompatible; the institutional church''s adoption of the prophetic_override_reading should have eliminated the immutable_commandment reading from within the tradition. If coexistence obtains, both readings remain live options, and the fundamentalist dissidents are maintaining a logically coherent alternative within the tradition. This affects the reading_relations classification (forecloses vs. coexists_with).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the prophetic override logically forecloses immutable-commandment doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__prophetic_override_reading, 1880, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1880, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1880, 0.15).
narrative_ontology:measurement(eter_tr_t1887, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1887, 0.28).
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1890, 0.38).
narrative_ontology:measurement(eter_tr_t1900, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1900, 0.42).
narrative_ontology:measurement(eter_tr_t1910, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1910, 0.42).
narrative_ontology:measurement(eter_tr_t1920, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1920, 0.42).

% Extraction over time
narrative_ontology:measurement(eter_be_t1880, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1880, 0.25).
narrative_ontology:measurement(eter_be_t1887, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1887, 0.48).
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1890, 0.62).
narrative_ontology:measurement(eter_be_t1900, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1900, 0.68).
narrative_ontology:measurement(eter_be_t1910, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1910, 0.68).
narrative_ontology:measurement(eter_be_t1920, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1920, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1880, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1880, 0.35).
narrative_ontology:measurement(eter_su_t1887, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1887, 0.58).
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1890, 0.68).
narrative_ontology:measurement(eter_su_t1900, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1900, 0.71).
narrative_ontology:measurement(eter_su_t1910, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1910, 0.71).
narrative_ontology:measurement(eter_su_t1920, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1920, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__prophetic_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eternal_marriage_covenant__prophetic_override_reading, 0.12).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__temporal_accommodation_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, federal_anti_polygamy_statute__enforcement_regime).

% DUAL FORMULATION NOTE:
% The eternal_marriage_covenant kernel has three structurally distinct readings, each with different ε values and beneficiary/victim structures. This constraint (prophetic_override_reading) models the override mechanism as a coordination function (solving the incompatibility of eternal doctrine and federal law) that operates as asymmetric extraction (cost borne by practicing polygamists and dissidents). The immutable_commandment_reading models the same doctrine as natural law (ε ≈ 0.1, mountain) and rejects the override's legitimacy. The temporal_accommodation_reading models the Manifesto as pragmatic obedience to civil law without doctrinal change (lower ε, rope or tangled_rope depending on whether dissent is suppressed). All three are readings of the same kernel; they are not three perspectives on one constraint, but three different constraints sharing a kernel identity. The network links them as competing interpretations of the same religious text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eternal_marriage_covenant__prophetic_override_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
