% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__principled_intervention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__principled_intervention_reading, []).

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
 *   constraint_id: constitutional_secularism__principled_intervention_reading
 *   human_readable: Constitutional Secularism: Principled Intervention Reading
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint instantiates the 'principled intervention' reading of
 *   constitutional secularism: the doctrine that the state may legitimately
 *   intervene in religious affairs when doing so advances social reform and
 *   protects weaker sections within communities from discriminatory or
 *   oppressive practices. The reading claims that such intervention is not a
 *   violation of secularism but rather its fulfillment — the state's duty to
 *   protect equality overrides religious autonomy when the two conflict. The
 *   constraint is a READING of the contested kernel 'constitutional
 *   secularism,' which admits at least three structurally distinct
 *   interpretations: strict neutrality (no state interference), principled
 *   intervention (this one), and reformist (state affirmative duty to
 *   eliminate oppressive practices). This story describes only the principled
 *   intervention reading's structure, metrics, and beneficiary/victim
 *   asymmetry. The claim (tangled_rope) and metrics reflect substantive
 *   extraction and active enforcement — the reading's own logic acknowledges
 *   that it privileges state authority over religious autonomy, which creates
 *   the asymmetry.
 *
 * KEY AGENTS:
 *   - Reform-oriented state: institutional agenda-setter with generational time horizon and analytical exit; determines which interventions count as principled reform
 *   - Marginalized religious minorities: powerless beneficiaries trapped within their communities; gain leverage and protection through state intervention
 *   - Religious majorities: organized payers with constrained exit; experience intervention as loss of governance autonomy
 *   - Autonomous religious communities: institutions experiencing loss of discretionary authority over internal governance
 *   - Reformist judges and administrators: institutional agenda-setters with mobile exit who gain professional prestige and institutional power from enforcing the doctrine
 *   - Strict neutrality advocates: excluded moderate-power seat; structurally erased from legitimacy framing
 *   - Constitutional court: institutional observer whose interpretation of the constitutional mandate shapes doctrine boundaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, 0.68).
domain_priors:suppression_score(constitutional_secularism__principled_intervention_reading, 0.71).
domain_priors:theater_ratio(constitutional_secularism__principled_intervention_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__principled_intervention_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__principled_intervention_reading, "Constitutional Secularism: Principled Intervention Reading").
narrative_ontology:topic_domain(constitutional_secularism__principled_intervention_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__principled_intervention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__principled_intervention_reading, '386bc09d-15d6-4c2a-82df-24b918f3389f').
narrative_ontology:cs_kernel_codification('386bc09d-15d6-4c2a-82df-24b918f3389f', fixed_text).
narrative_ontology:cs_authority_grounding('386bc09d-15d6-4c2a-82df-24b918f3389f', extraction).
narrative_ontology:cs_interpretation_layer_present('386bc09d-15d6-4c2a-82df-24b918f3389f').
narrative_ontology:cs_reading_relation('386bc09d-15d6-4c2a-82df-24b918f3389f', constitutional_secularism__strict_neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('386bc09d-15d6-4c2a-82df-24b918f3389f', constitutional_secularism__reformist_reading, influences).
narrative_ontology:cs_axiom('386bc09d-15d6-4c2a-82df-24b918f3389f', foundational, state_capacity_for_benevolent_intervention).
narrative_ontology:cs_axiom_status(state_capacity_for_benevolent_intervention, holdable).
narrative_ontology:cs_axiom_grounding('386bc09d-15d6-4c2a-82df-24b918f3389f', state_capacity_for_benevolent_intervention, instrumental).
narrative_ontology:cs_axiom('386bc09d-15d6-4c2a-82df-24b918f3389f', foundational, equality_overrides_religious_autonomy_when_conflict).
narrative_ontology:cs_axiom_status(equality_overrides_religious_autonomy_when_conflict, holdable).
narrative_ontology:cs_axiom_grounding('386bc09d-15d6-4c2a-82df-24b918f3389f', equality_overrides_religious_autonomy_when_conflict, deontological).
narrative_ontology:cs_reference_frame('386bc09d-15d6-4c2a-82df-24b918f3389f', state_benevolent_protector_of_constitutional_equality).
narrative_ontology:cs_drift_state('386bc09d-15d6-4c2a-82df-24b918f3389f', contemporary_administrative_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('386bc09d-15d6-4c2a-82df-24b918f3389f', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__principled_intervention_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, reform_oriented_state).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, marginalized_religious_minorities).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, religious_majorities).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, autonomous_religious_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, reformist_judges_and_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the scope and legitimacy of interventions in religious affairs. Justifies each intervention as advancing social reform: eliminating caste discrimination in temples, outlawing child marriage, requiring equal access to religious spaces, regulating funds used for practices deemed harmful. Wields constitutional authority to adjudicate which practices align with constitutional values of dignity and equality. The constraint's persistence depends on the state's willingness and capacity to determine what counts as reformist intervention versus overreach.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, reform_oriented_state, agenda_setter,
    institutional, generational, analytical, national).

% Within their own religious communities (often lower castes within Hinduism, Dalits, Muslim women, Christian tribes with oppressive patriarch practices), gain access to spaces, resources, and exit options from discriminatory practices through state-backed interventions. Cannot exit the religious community without severe social cost; the state intervention provides leverage within-community for reform. The constraint benefits them by redirecting religious authority toward their inclusion.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, marginalized_religious_minorities, beneficiary,
    powerless, biographical, trapped, national).

% Experience state intervention in their internal religious governance — rules about temple access, ritual practice regulation, financial controls — as external coercion disguised as reform. The constraint's enforcement prevents them from governing their own institutions according to traditional authority structures. They bear the cost of diluted institutional autonomy and the threat of judicial reversal of religious decisions.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, religious_majorities, payer,
    organized, generational, constrained, national).

% Religious institutions and their leaders lose discretionary authority over their own governance. Internally contested religious questions (who may lead prayers, how funds are used, what marriage rules apply) become subject to state adjudication under the rubric of 'reform.' The communities cannot exit or withdraw consent without losing legal recognition and property rights. They would argue that state neutrality — not intervention — is the proper constitutional baseline.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, autonomous_religious_communities, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__principled_intervention_reading, autonomous_religious_communities, excluded).

% Judicial and bureaucratic actors who enforce and expand the doctrine of principled intervention. They gain institutional authority and the power to redefine religious practices as aligned or misaligned with constitutional values. Career advancement and professional prestige attach to successful reform litigation and policy implementation. The constraint legitimizes their expansion into new domains of religious regulation.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, reformist_judges_and_administrators, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__principled_intervention_reading, reformist_judges_and_administrators, beneficiary).

% Constitutional scholars, religious autonomy defenders, and civil liberties advocates who would argue for equal distance and non-interference. They are structurally locked out of the decision-making process because the constraint reframes their position as indifference to oppression. Their voice appears in dissents and academic critique but does not shape implementation doctrine.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, strict_neutrality_advocates, excluded,
    moderate, generational, trapped, national).

% Members of the beneficiary group who disagree with state-mandated reforms on the grounds that they dilute or distort their own religious tradition. They want inclusion and respect within their religion on their tradition's terms, not on terms dictated by external state actors. Cannot speak back to the state machinery without being seen as defenders of oppression; cannot fully embrace the reformist direction without betraying their tradition. Their voices are erased by the beneficiary/payer binary.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, marginalized_religious_minorities_internal_dissidents, excluded,
    powerless, biographical, identity_locked, national).

% Adjudicate the boundaries of permissible intervention and the weight of competing constitutional values (religious freedom, equality, dignity). Over time, doctrine from this seat either reins in intervention or expands it. The constraint's classification and persistence depend partly on how the court interprets its own mandate.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, constitutional_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_secularism__principled_intervention_reading, reform_oriented_state).
narrative_ontology:fixing_cost_class(constitutional_secularism__principled_intervention_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for advancing social reform and protecting marginalized individuals within religious communities by authorizing state oversight of religious practices. Solves the coordination problem of how to honor both religious autonomy and constitutional equality: the reading answers that principled intervention — where the state acts to protect fundamental rights — is not a violation of secularism but its fulfillment.
% TRANSFER_FUNCTION: Moves effective decision-making authority from autonomous religious institutions and majorities to state agents (courts, administrators, reform-oriented officials) on the question of which religious practices align with constitutional values. Religious minorities gain leverage within their communities; majorities and autonomous institutions lose discretionary control. Prestige and institutional power flow to judges and reformist administrators who successfully expand and enforce the doctrine.
% ABSENT_VOICES: Strict neutrality advocates and defenders of religious autonomy are structurally excluded from the reading's legitimacy frame — they appear as defenders of oppression. Marginalized religious minorities who object to externally-mandated reform on grounds of respect for their own tradition are erased by the beneficiary/payer binary; their dissent is re-labeled as complicity. Alternative readings of what constitutes 'reform' within religious traditions are suppressed by state definition of the reform agenda.
% DISAPPEARANCE_RATIONALE: If the principled intervention doctrine vanished, marginalized individuals within religious communities would lose the external enforcement mechanism protecting their rights; religious majorities and institutions would recover unilateral governance authority; state administrative capacity devoted to religious oversight would reorient; and constitutional interpretation would shift toward either strict neutrality or reformist readings (each with different enforcement machinery). The landscape of religious authority, minority protection, and state capacity would reorganize fundamentally.
% FOUNDING_PROBLEM: Religious communities institutionalize and enforce discriminatory hierarchies (caste systems, patriarchal marriage laws, exclusionary access to sacred spaces) that burden marginalized members who cannot exit without catastrophic social cost. The state must have authority to intervene in defense of fundamental constitutional rights (equality, dignity) even within traditionally autonomous religious domains.
% FOUNDING_PROBLEM_CORROBORATION: Marginalized religious minorities and human rights organizations attest that the founding problem is live and acute — documented cases of caste discrimination, child marriage, female infanticide, and exclusion persist within religious institutional settings. Religious autonomy defenders and strict neutrality advocates contest both the diagnosis and the remedy: they attest that the founding problem is overstated, that community self-reform is preferable, and that state intervention creates worse downstream harms (majoritarian capture, erosion of minority religion autonomy, doctrinal incoherence). Constitutional courts globally split on whether the founding problem justifies the doctrine; some courts (e.g., India's) embrace it; others (e.g., strict-establishment jurisprudence in some U.S. contexts) reject it. No corroboration exists from parties wholly outside this contest — the founding problem is itself contested across all seats.
narrative_ontology:disappearance_verdict(constitutional_secularism__principled_intervention_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__principled_intervention_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__principled_intervention_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_secularism__principled_intervention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__principled_intervention_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__principled_intervention_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__principled_intervention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end, rising from 0.51) because the constraint permanently transfers decision-making authority from religious communities to state agents on fundamental questions of religious practice and governance. The measurement series shows extractiveness accumulating over the interval: early doctrine is restrained; as precedent accumulates, the state's capacity to expand intervention grows, and each marginal expansion is justified as 'principled' under the original reading. Suppression is similarly high (0.71) because enforcement depends on active state machinery suppressing (1) counter-interpretations from strict neutrality advocates, (2) religious majority resistance to individual interventions, and (3) alternative definitions of reform coming from within marginalized communities themselves. The suppression is structural: religious majorities cannot opt out of state oversight without losing legal recognition; communities cannot appeal to a competing reading because the doctrine frames such appeals as obstruction. Theater ratio rises from 0.28 to 0.42 because enforcement machinery increasingly performs 'reform' and 'protection' narratives to justify expansions that operate more as administrative control. The temporal pattern shows extraction and suppression ramping in the early years as doctrine consolidates, then plateauing as the reading reaches institutional equilibrium — this is consistent with a tangled_rope that successfully embedded both coordination (marginalized protection) and extraction (state authority capture) into a single mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The asymmetry is extreme. From the reformist state's seat, the constraint solves a genuine coordination problem (how to honor both equality and religious freedom); it is legitimate use of state capacity. From the payer seats, the same structure is rationalized extraction of governance authority from religious communities into state hands, with reform rhetoric as the cover story. The reading's own axiom (state capacity for benevolent intervention) is precisely what the strict neutrality reading denies — the denial is not empirical but normative (state should not intervene even if it can do so benevolently). This is why the engine's per-seat computation is crucial: it surfaces whether the payer-seat classification differs from the agenda-setter's claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Reform-oriented state: d ≈ 0.2 (analytical exit, institutional power, sets the rules — strong beneficiary directionality despite institutional power, because the constraint expands state authority and the state is the reading's principal author). Marginalized religious minorities: d ≈ 0.3 (trapped exit, powerless, but genuine protection benefit — moderate beneficiary directionality because protection is real even though it comes with loss of autonomy; exit is identity-locked for most, so they cannot threaten to leave). Religious majorities: d ≈ 0.8 (constrained exit, organized power but no authority over the constraint, lose governance discretion — strong target directionality). Autonomous religious communities: d ≈ 0.85 (constrained exit, moderate institutional power, lose authority over own governance — strong target directionality). Reformist judges/administrators: d ≈ 0.25 (mobile exit, institutional power, gain prestige and authority expansion — beneficiary, though institutional rather than personal extraction). Strict neutrality advocates: excluded, so d is undefined in the normal sense, but their structural position is d ≈ 0.75 if they were to advocate openly (would be suppressed, target-side). Constitutional court: d ≈ 0.5 (observer seat; the court's own interpretation can shift d for other agents over time).
 *
 * MANDATROPHY ANALYSIS:
 *   The principled intervention reading risks mandatrophy through two pathways: (1) Mission creep — as the state gains capacity to intervene in religious affairs under the rubric of 'protection and reform,' each new intervention becomes easier to justify, and the boundary between legitimate protection and administrative control dissolves. What began as intervention in clearly oppressive practices (caste discrimination, child marriage) expands to regulation of ritual, finance, and succession within majority communities. The founding problem (protection of marginalized minorities) remains live, but the doctrine's scope has expanded far beyond it. (2) Majoritarianism capture — the doctrine's legitimacy rests on protecting weaker sections, but the execution-seat (reformist judges/administrators) gains institutional power and career incentive to expand state authority. Over time, the constraint may be used not to protect minorities but to advantage a majoritarian coalition's preferred vision of religious practice, reversing the reading's original beneficiary structure. The measurement series shows extraction plateauing by T=30, not declining, which suggests the doctrine has stabilized at a new, higher equilibrium of state authority rather than reverting toward neutrality or contracting toward genuine case-by-case protection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_genuine_natural_law,
    'Is principled intervention a reading of an ambiguous constitutional kernel, or does constitutional secularism have a determinate meaning that the state has either correctly or incorrectly understood?',
    'Comparative constitutional analysis across jurisdictions with similar constitutional texts: do courts in different democracies converge on the same interpretation of secularism, or do they diverge predictably based on institutional structures and political majorities?',
    'If convergence occurs, the reading gains structural legitimacy as a natural interpretation of constitutional language; if divergence is systematic (correlated with majority religion, state capacity, or reform agenda), the reading is better understood as one instantiation of an ambiguous kernel, not a discovered constitutional truth. This would reclassify the analysis from ''what does the constitution mean'' to ''which interpretation did the dominant coalition manage to entrench.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_genuine_natural_law, conceptual, 'Whether the principled intervention reading is the correct interpretation of constitutional secularism or one reading of an irreducibly contested kernel.').

omega_variable(
    suppression_structural_vs_internalized,
    'To what extent is resistance to the principled intervention doctrine suppressed by structural dependency (loss of state recognition and property rights) versus internalized acceptance of the state''s reform framing?',
    'Post-intervention case studies: when religious institutions are removed from state oversight (jurisdictional transfer, constitutional amendment, regime change), do they immediately revert to pre-reform practices, or do they maintain reformed practices voluntarily?',
    'High structural suppression, low internalization means the constraint is reversible — removing state machinery would restore prior practices. High internalization means the constraint has become self-sustaining cultural change — even if state machinery were removed, communities might maintain reformed practices. The distinction matters for assessing whether the constraint is extractive (requires active suppression) or coordinative (communities have accepted the new equilibrium as legitimate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is maintained by external coercion or has been internalized as legitimate change.').

omega_variable(
    reformist_agenda_definition,
    'Who defines what counts as ''reform'' and ''protection of weaker sections,'' and can this definition be captured by majoritarian preferences?',
    'Longitudinal analysis of which practices have been targeted for state intervention: do they track documented harm and community preference, or do they track majoritarian religious preferences about what constitutes ''proper'' religious practice?',
    'If reform definitions remain aligned with the needs expressed by marginalized communities themselves, the beneficiary structure holds and the constraint is tangled_rope (genuine protection + state authority expansion). If reform definitions drift toward majoritarian enforcement of religious orthodoxy, the constraint becomes snare — the marginalized beneficiaries are captured as an instrument for advancing majoritarian religious preferences while maintaining the appearance of protecting them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reformist_agenda_definition, empirical, 'Whether the reform agenda remains aligned with marginalized communities'' own priorities or has been captured by majoritarian preferences.').

omega_variable(
    alternative_readings_silencing,
    'Are strict neutrality advocates and defenders of religious autonomy excluded from the reading''s legitimacy frame because their position is incoherent, or because the reading has become institutionally dominant and suppresses dissent?',
    'Audit of constitutional court decisions, legislative debates, and academic output over the interval: do strict neutrality arguments appear in dissents with substantial support, or are they dismissed as mere obstruction? Do jurisdictions where strict neutrality remains dominant show systematically different outcomes for marginalized minorities?',
    'If strict neutrality advocates are excluded because their reading is logically incoherent, then the principled intervention reading has detected a genuine constitutional truth. If they are excluded because the principled intervention reading became institutionally dominant and redefined the terms of constitutional discourse, then the exclusion is an effect of power, not reason — the reading is not uniquely correct but merely dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_readings_silencing, conceptual, 'Whether strict neutrality readings are excluded due to logical incoherence or due to institutional power dynamics.').

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates the ''principled intervention'' reading of the constitutional secularism kernel. How would the strict neutrality and reformist readings instantiate different constraints with different ε, beneficiary/victim structures, and classifications?',
    'Generate the sibling readings as separate constraint stories (strict_neutrality_reading, reformist_reading) and compare their structural properties to this one.',
    'The three readings decompose constitutional secularism into three distinct constraints with different extractiveness profiles, different beneficiary/victim sets, and different types. The reading-vs-neutral question surfaces whether the kernel allows principled interpretation (readings are genuine alternatives) or whether one reading is constitutionally correct and the others incoherent (reading is dissolved, the constraint is singular).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The irreducible contestation between sibling readings of the constitutional secularism kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__principled_intervention_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_secularism__principled_intervention_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cons_tr_t5, constitutional_secularism__principled_intervention_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(cons_tr_t10, constitutional_secularism__principled_intervention_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(cons_tr_t15, constitutional_secularism__principled_intervention_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(cons_tr_t20, constitutional_secularism__principled_intervention_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(cons_tr_t25, constitutional_secularism__principled_intervention_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(cons_tr_t30, constitutional_secularism__principled_intervention_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(cons_tr_t40, constitutional_secularism__principled_intervention_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_secularism__principled_intervention_reading, base_extractiveness, 0, 0.51).
narrative_ontology:measurement(cons_be_t5, constitutional_secularism__principled_intervention_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(cons_be_t10, constitutional_secularism__principled_intervention_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(cons_be_t15, constitutional_secularism__principled_intervention_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(cons_be_t20, constitutional_secularism__principled_intervention_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(cons_be_t25, constitutional_secularism__principled_intervention_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(cons_be_t30, constitutional_secularism__principled_intervention_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(cons_be_t40, constitutional_secularism__principled_intervention_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_secularism__principled_intervention_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(cons_su_t5, constitutional_secularism__principled_intervention_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement(cons_su_t10, constitutional_secularism__principled_intervention_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(cons_su_t15, constitutional_secularism__principled_intervention_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(cons_su_t20, constitutional_secularism__principled_intervention_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(cons_su_t25, constitutional_secularism__principled_intervention_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(cons_su_t30, constitutional_secularism__principled_intervention_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(cons_su_t40, constitutional_secularism__principled_intervention_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__principled_intervention_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_secularism__principled_intervention_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, constitutional_secularism__reformist_reading).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, religious_autonomy_doctrine).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, majoritarian_capture_risk).

% DUAL FORMULATION NOTE:
% The constitutional secularism kernel decomposes into three readings: strict neutrality (state equals distance from all religions), principled intervention (state may intervene for reform and protection), and reformist (state affirmative duty to eliminate oppressive practices). Each reading is a distinct constraint with its own ε, beneficiary/victim structure, and type. This story instantiates only the principled intervention reading. The network links this reading to its siblings and to downstream constraints (religious autonomy doctrine, majoritarian capture risk) affected by this reading's institutionalization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_secularism__principled_intervention_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
