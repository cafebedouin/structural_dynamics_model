% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__reformist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__reformist_reading, []).

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
 *   constraint_id: constitutional_secularism__reformist_reading
 *   human_readable: Reformist Reading: State Duty to Eliminate Oppressive Religious Practice
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This story instantiates the reformist reading of the constitutional
 *   secularism kernel: the state carries an affirmative constitutional duty
 *   to eliminate religious practices that oppress marginalized groups
 *   (scheduled castes, women), and this duty supersedes claims of religious
 *   autonomy where the two conflict. Unlike the strict-neutrality reading
 *   (equal distance, no interference) or the principled-intervention reading
 *   (calibrated intervention balanced against communal self-governance), the
 *   reformist reading treats religious-autonomy claims as categorically
 *   subordinate once a practice is characterized as oppressive — it does not
 *   ask whether intervention is proportionate or minimally invasive, only
 *   whether the practice in question falls within the disfavored category.
 *   This is deliberately authored as the most extractive reading of the
 *   kernel: its ε is high because the doctrine's operative logic removes
 *   negotiation and proportionality review as live constraints on state power
 *   once the oppressive-practice finding is made.
 *
 * KEY AGENTS:
 *   - scheduled_caste_worshippers: primary beneficiary (powerless/trapped) — gains entry and standing previously denied
 *   - women_excluded_from_religious_sites: primary beneficiary (powerless/trapped) — gains ritual access previously denied
 *   - state_reform_apparatus: agenda-setter (institutional/analytical) — defines and enforces the oppressive-practice category
 *   - religious_conservative_congregations and temple_trust_boards: primary payers (moderate-organized/constrained) — lose managerial and doctrinal authority
 *   - orthodox_clergy: identity-locked payer — vocation constituted by the overridden practice
 *   - minority_religious_denominational_institutions: precedent-anxious payer — fears doctrinal spillover
 *   - constitutional_courts: agenda-setter/observer — adjudicate and thereby constitute the doctrine's actual content
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, 0.68).
domain_priors:suppression_score(constitutional_secularism__reformist_reading, 0.71).
domain_priors:theater_ratio(constitutional_secularism__reformist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__reformist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__reformist_reading, "Reformist Reading: State Duty to Eliminate Oppressive Religious Practice").
narrative_ontology:topic_domain(constitutional_secularism__reformist_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__reformist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__reformist_reading, '7b67bc80-bd6a-41f6-b805-db70ef759f27').
narrative_ontology:cs_kernel_codification('7b67bc80-bd6a-41f6-b805-db70ef759f27', formalized).
narrative_ontology:cs_authority_grounding('7b67bc80-bd6a-41f6-b805-db70ef759f27', lineage).
narrative_ontology:cs_interpretation_layer_present('7b67bc80-bd6a-41f6-b805-db70ef759f27').
narrative_ontology:cs_reading_relation('7b67bc80-bd6a-41f6-b805-db70ef759f27', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('7b67bc80-bd6a-41f6-b805-db70ef759f27', constitutional_secularism__principled_intervention_reading, coexists_with).
narrative_ontology:cs_axiom('7b67bc80-bd6a-41f6-b805-db70ef759f27', foundational, oppression_finding_categorically_supersedes_autonomy).
narrative_ontology:cs_axiom_status(oppression_finding_categorically_supersedes_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('7b67bc80-bd6a-41f6-b805-db70ef759f27', oppression_finding_categorically_supersedes_autonomy, deontological).
narrative_ontology:cs_axiom('7b67bc80-bd6a-41f6-b805-db70ef759f27', secondary, state_duty_to_intervene_is_affirmative_not_discretionary).
narrative_ontology:cs_axiom_status(state_duty_to_intervene_is_affirmative_not_discretionary, holdable).
narrative_ontology:cs_axiom_grounding('7b67bc80-bd6a-41f6-b805-db70ef759f27', state_duty_to_intervene_is_affirmative_not_discretionary, conventional).
narrative_ontology:cs_reference_frame('7b67bc80-bd6a-41f6-b805-db70ef759f27', anti_untouchability_founding_mandate).
narrative_ontology:cs_drift_state('7b67bc80-bd6a-41f6-b805-db70ef759f27', contemporary_gender_ritual_disputes, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7b67bc80-bd6a-41f6-b805-db70ef759f27', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__reformist_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_caste_worshippers).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, women_excluded_from_religious_sites).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, reform_movement_litigants).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, state_reform_apparatus).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_conservative_congregations).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, temple_trust_boards).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, orthodox_clergy).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, minority_religious_denominational_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically barred from entry to temples and denied priesthood roles on caste grounds. Under this reading, courts and legislatures strike down exclusionary custom as untouchability-adjacent practice, opening entry and office by judicial order rather than by negotiated communal reform. Their access now depends on continued state willingness to enforce against local resistance.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, scheduled_caste_worshippers, beneficiary,
    powerless, generational, trapped, national).

% Excluded from specific shrines or rites on menstruation-related or purity grounds. This reading treats such exclusion as constitutionally intolerable regardless of the community's own theological account of the practice, and directs courts to override customary managing-committee authority to compel entry.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, women_excluded_from_religious_sites, beneficiary,
    powerless, generational, trapped, national).

% Legislatures, courts, and administrative boards that define which religious practices count as oppressive and issue orders — entry mandates, board takeovers, criminal prohibitions — overriding denominational management. Justifies its authority as a duty flowing directly from constitutional equality and dignity guarantees, independent of whether the affected community consents to the characterization of its practice as oppressive.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, state_reform_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Communities whose contested practices are reclassified by the state as oppression rather than legitimate religious observance. Face loss of managerial control, court-ordered rule changes to core ritual, and criminal or civil liability for continuing customary practice. Their theological objection that the practice is a matter of essential religious character, not caste or gender subordination, is treated as legally irrelevant once the state's characterization prevails.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_conservative_congregations, payer,
    moderate, biographical, constrained, regional).

% Statutory or customary bodies that administer temple property and ritual. Under this reading their autonomy is explicitly subordinate to state reform authority: boards can be superseded, membership rules struck down, and administration placed under government-appointed receivers when the site's practice is found oppressive. They retain organizational resources to litigate but cannot exit state jurisdiction.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, temple_trust_boards, payer,
    organized, biographical, constrained, regional).

% Priests and religious authorities whose professional standing and theological worldview are constituted by the very practices being overridden. Their objection is not merely economic but identity-constitutive: conceding the state's characterization of their tradition as oppressive would require repudiating the framework their vocation rests on, making genuine accommodation-seeking exit functionally unavailable to them even though formal exit (leaving the priesthood) exists.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, orthodox_clergy, payer,
    moderate, biographical, identity_locked, regional).

% Non-majority religious institutions (certain denominational sects, minority-community trusts) fear that a doctrine empowering the state to override religious autonomy whenever it finds internal practice oppressive will, once established against a majority-community practice, be turned against minority practice with less political cost to the state — their objection is partly about precedent, not only about the immediate practice at issue.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, minority_religious_denominational_institutions, payer,
    organized, generational, constrained, national).

% Adjudicate essential-practices doctrine disputes and decide whether a given custom is core religious practice (protected) or oppressive social practice masquerading as religion (unprotected). Their rulings both implement and constitute this reading; dissenting judges within the same courts sometimes favor the strict-neutrality or principled-intervention readings instead, so the doctrine's content shifts with panel composition.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, constitutional_courts, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__reformist_reading, constitutional_courts, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__reformist_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_secularism__reformist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state power to dismantle practices that operate, within a religious frame, to exclude or subordinate identifiable groups (scheduled castes, women) from religious and social goods — solving a collective-action problem where no internal reform mechanism within the affected community would otherwise displace an entrenched exclusionary custom.
% TRANSFER_FUNCTION: Moves control over ritual access, temple administration, and doctrinal authority from incumbent religious managing bodies and clergy to state courts and legislatures, and moves standing/access/dignity from previously excluded groups' perspective to those groups, at the cost of managerial and interpretive authority previously held by community institutions.
% ABSENT_VOICES: The internal reformist wings of the affected religious communities themselves are frequently absent from the litigation record, which is often framed as state-versus-orthodoxy; their voices, and any independent theological reform argument distinct from the state's constitutional argument, tend to be flattened into either the reform or orthodox camp by the binary structure of the litigation.
% DISAPPEARANCE_RATIONALE: If this doctrine's affirmative-duty reading were withdrawn, the specific court orders and legislative reforms premised on it (temple-entry mandates for excluded castes, shrine-access rulings for women, denominational board supersession orders) would lose their constitutional basis, and communities currently under state-imposed reform would regain the ability to resist changes as a matter of religious autonomy — access previously compelled by court order would again depend on community-internal negotiation.
% FOUNDING_PROBLEM: Post-independence constitution-framers confronted entrenched caste-based exclusion from religious life (untouchability) that religious tradition itself sanctioned in the practice of many communities, and needed a constitutional mechanism for the state to override customary religious authority to dismantle that exclusion.
% FOUNDING_PROBLEM_CORROBORATION: Scheduled-caste rights organizations and several constitutional scholars attest the founding problem (caste exclusion enforced through religious sanction) remains substantially live in specific communities and specific rites, supporting continued affirmative intervention. Religious conservative associations and some minority-rights scholars attest that the doctrine, as currently applied, has migrated from remedying caste exclusion specifically toward a broader judicially-defined category of 'oppressive practice' that increasingly reaches gender-based ritual distinctions unconnected to caste, and argue the founding problem's scope has been extended beyond what independent corroboration from outside reform-movement litigants supports.
narrative_ontology:disappearance_verdict(constitutional_secularism__reformist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__reformist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__reformist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_secularism__reformist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__reformist_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__reformist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__reformist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.68 at interval end) because the reformist reading, by its own internal logic, treats the finding of 'oppression' as dispositive rather than as one factor in a proportionality balance — once a practice is classified, autonomy claims yield categorically. This differs sharply from what a principled-intervention reading would score, where balancing tests would keep extraction lower and more calibrated. Suppression is high (0.71) because enforcing entry orders, board supersessions, and criminal prohibitions against a resistant community requires ongoing state coercive capacity, not voluntary compliance. Theater ratio stays comparatively low (0.22) because the intervention mechanism is substantively operative — courts and legislatures are not merely performing reform, they are materially reallocating access and authority — though a modest and rising theatrical component reflects symbolic enforcement actions (highly publicized entry escorts, for example) that outrun the doctrine's actual reach into less visible local practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Scheduled-caste worshippers and excluded women are the clearest structural beneficiaries: the doctrine exists specifically to grant them access and standing they were denied, and their own exit options (trapped, powerless) make external remedy — rather than negotiated internal reform — their only realistic path, which is precisely what the reformist reading offers. Religious conservative congregations, temple trust boards, and minority denominational institutions are the targets: their autonomy is what the doctrine's affirmative duty explicitly overrides. Orthodox clergy receive an identity-lock override consideration distinct from the organized institutions: their exit option is formally available (leave the priesthood) but practically foreclosed by identity fusion with the tradition being overridden, which the raw exit_options atom (identity_locked) captures without needing a numeric override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — caste exclusion sanctioned by religious custom — was real and constitutionally salient at framing. The mandatrophy risk in this reading specifically is scope creep: the doctrine's authority was forged against caste-based untouchability, a settled constitutional wrong, but the reformist reading's categorical (non-balancing) structure makes it structurally easy to extend the same 'oppressive practice, therefore autonomy yields' logic to contested territory (gender-based ritual distinctions with less consensus than caste exclusion) without re-litigating whether the extension is warranted. Naming founding_problem_status as contested (rather than simply live) is the mechanism that prevents this story from silently assuming the doctrine's current scope is co-extensive with its founding justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reformist_vs_principled_intervention_boundary,
    'Is the categorical (non-balancing) supersession of religious autonomy in the reformist reading empirically distinguishable from the calibrated balancing the principled-intervention reading claims to perform, or do courts applying either label reach materially the same outcomes through different rhetoric?',
    'Comparative case analysis across a sample of temple-entry, essential-practices, and denominational-autonomy rulings, coding whether the court''s reasoning contains an actual proportionality step (evidence for principled-intervention) or treats the oppression finding as dispositive (evidence for reformist).',
    'If the two readings collapse into the same practice under different labels, the kernel''s reading-space effectively has two live positions, not three, and network edges between the reformist and principled-intervention constraint stories should be strengthened from influences toward something closer to functional identity in outcome (though the schema does not permit collapsing constraint stories after the fact — this would instead be documented as convergence-in-practice-divergence-in-doctrine).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_vs_principled_intervention_boundary, empirical, 'Whether the reformist and principled-intervention readings are doctrinally distinct or converge in applied outcome.').

omega_variable(
    essential_practices_doctrine_manipulability,
    'Is the essential-practices test (which distinguishes protected core religious practice from unprotected oppressive custom) a principled theological/historical inquiry, or is it functionally a vehicle through which courts encode contemporary constitutional values and then attribute the result to the religion''s own doctrine?',
    'Track instances where courts'' essential-practices findings diverge from the self-understanding of the community''s own religious authorities and from independent historical/theological scholarship, and assess whether divergence correlates with the outcome the court appears to favor on other grounds.',
    'If the test is substantially manipulable, the reformist reading''s legitimacy claim (that it is enforcing constitutional values against practices that are not truly essential to the religion) is weaker than claimed, and the doctrine functions closer to unconstrained state override of religious practice than to a principled essential/non-essential distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(essential_practices_doctrine_manipulability, conceptual, 'Whether the essential-practices doctrine is a principled inquiry or a results-oriented instrument.').

omega_variable(
    minority_spillover_risk,
    'Does establishing categorical state override authority against majority-community practices create a genuine precedential risk of the same doctrine being applied more readily against minority religious communities, given differential political costs?',
    'Track the doctrine''s actual application pattern across majority and minority religious communities over the interval, and compare enforcement intensity, judicial deference, and political controversy across community type for comparably ''oppressive'' practices.',
    'If minority communities face systematically more aggressive application for comparable practices, the reformist reading''s stated universal principle (oppression triggers duty regardless of community) is not being applied even-handedly, which would itself be a form of the extraction the framework is designed to detect — differential political cost of intervention becomes a hidden additional axis of who bears the doctrine''s force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_spillover_risk, empirical, 'Whether categorical override authority is applied even-handedly across majority and minority religious communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__reformist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__reformist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t8, constitutional_secularism__reformist_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(cons_tr_t16, constitutional_secularism__reformist_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(cons_tr_t24, constitutional_secularism__reformist_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(cons_tr_t32, constitutional_secularism__reformist_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(cons_tr_t40, constitutional_secularism__reformist_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__reformist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cons_be_t8, constitutional_secularism__reformist_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(cons_be_t16, constitutional_secularism__reformist_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement(cons_be_t24, constitutional_secularism__reformist_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(cons_be_t32, constitutional_secularism__reformist_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(cons_be_t40, constitutional_secularism__reformist_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__reformist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cons_su_t8, constitutional_secularism__reformist_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(cons_su_t16, constitutional_secularism__reformist_reading, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(cons_su_t24, constitutional_secularism__reformist_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(cons_su_t32, constitutional_secularism__reformist_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(cons_su_t40, constitutional_secularism__reformist_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, constitutional_secularism__principled_intervention_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the constitutional_secularism kernel. constitutional_secularism__strict_neutrality_reading authors near-zero extraction from the premise that the state should not characterize or intervene in internal religious practice at all. constitutional_secularism__principled_intervention_reading authors moderate, calibrated extraction from a balancing-test premise. This story (reformist_reading) authors the highest extraction of the three because its categorical supersession logic removes proportionality review once an oppression finding is made. All three share the same underlying kernel (the constitutional text and doctrine governing state-religion relations) but instantiate structurally distinct constraints with distinct ε, distinct victim sets, and distinct beneficiary sets — per the ε-invariance principle they are authored as separate stories rather than as one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
