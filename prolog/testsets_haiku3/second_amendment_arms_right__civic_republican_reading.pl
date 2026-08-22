% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__civic_republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__civic_republican_reading, []).

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
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Armed Citizenship as Republican Civic Duty and Right
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the civic republican reading of the
 *   Second Amendment: the constitutional right to bear arms protects armed
 *   citizenship as a structural prerequisite for republican self-governance,
 *   neither grounding the right in purely individual pre-political liberty
 *   nor restricting it to state-militia control. Under this reading, citizens
 *   have a right to maintain arms and militia readiness, but the right is
 *   exercised within a framework of civic participation, training, and
 *   qualification norms that federal and state authorities can enforce
 *   without violating the right itself. The reading positions itself between
 *   two sibling interpretations: the individual-rights reading (which treats
 *   the right as prior to government and resists conditioning it on civic
 *   duty) and the collective-militia reading (which ties the right to
 *   state-organized militia and denies individual ownership claims). The
 *   civic republican reading accepts both an individual dimension (citizens
 *   do have a protected right to arms) and a collective dimension (the right
 *   serves republican self-governance and carries civic obligations), but
 *   organizes them around civic participation rather than either pure
 *   libertarian individualism or state monopoly.
 *
 * KEY AGENTS:
 *   - armed_citizens_militia_members: Dual beneficiaries and duty-bearers; right to maintain arms conditioned on civic-participation norms and training standards (moderate/constrained power)
 *   - federal_regulatory_authority: Constrained from disarming citizens but retains regulatory power over training, qualification, and safety (institutional power)
 *   - state_militia_authorities: Benefit from a constitutional doctrine that treats militia readiness as legitimate regulatory domain (powerful/arbitrage exit)
 *   - tyranny_prevention_constituency: Distributed benefit of an armed populace as structural check on government power (analytical classification)
 *   - gun_control_advocates: Excluded from the civic republican justificatory frame; would prioritize public safety over tyranny prevention (organized/constrained)
 *   - libertarian_individualist_reading_adherents: Excluded; treat the right as prior to government and resist conditioning it on civic duty (powerful/mobile)
 *   - collective_militia_reading_adherents: Excluded; restrict the right to state-organized militia context and deny individual ownership (powerful/mobile)
 *   - analytical_observer: Examines how the civic republican reading positions itself between individualist and collectivist interpretations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, 0.38).
domain_priors:suppression_score(second_amendment_arms_right__civic_republican_reading, 0.22).
domain_priors:theater_ratio(second_amendment_arms_right__civic_republican_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Armed Citizenship as Republican Civic Duty and Right").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, '34266854-8de9-46e8-a250-a0debb906507').
narrative_ontology:cs_kernel_codification('34266854-8de9-46e8-a250-a0debb906507', fixed_text).
narrative_ontology:cs_authority_grounding('34266854-8de9-46e8-a250-a0debb906507', lineage).
narrative_ontology:cs_interpretation_layer_present('34266854-8de9-46e8-a250-a0debb906507').
narrative_ontology:cs_reading_relation('34266854-8de9-46e8-a250-a0debb906507', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('34266854-8de9-46e8-a250-a0debb906507', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('34266854-8de9-46e8-a250-a0debb906507', foundational, armed_citizenship_republican_prerequisite).
narrative_ontology:cs_axiom_status(armed_citizenship_republican_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('34266854-8de9-46e8-a250-a0debb906507', armed_citizenship_republican_prerequisite, deontological).
narrative_ontology:cs_axiom('34266854-8de9-46e8-a250-a0debb906507', foundational, civic_duty_conditions_the_right).
narrative_ontology:cs_axiom_status(civic_duty_conditions_the_right, holdable).
narrative_ontology:cs_axiom_grounding('34266854-8de9-46e8-a250-a0debb906507', civic_duty_conditions_the_right, conventional).
narrative_ontology:cs_reference_frame('34266854-8de9-46e8-a250-a0debb906507', founding_era_militia_tradition).
narrative_ontology:cs_drift_state('34266854-8de9-46e8-a250-a0debb906507', contemporary_democratic_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('34266854-8de9-46e8-a250-a0debb906507', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, armed_citizens_militia_members).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, republican_self_governance_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, state_militia_authorities).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, tyranny_prevention_constituency).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, civic_republicanism_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, armed_populace_tyranny_prevention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Citizens who maintain arms and militia readiness as part of their civic duty and right. Under this reading, they are simultaneously beneficiaries (protected right to own weapons) and duty-bearers (trained participants in the civic defense infrastructure). Their practical situation involves balancing individual ownership liberty with militia training standards and qualification requirements that the civic framework imposes. They benefit from a constitutional guarantee against federal disarmament while accepting that the right is not unlimited — training, safety standards, and civic-participation norms apply as conditions on the right's exercise.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, armed_citizens_militia_members, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, armed_citizens_militia_members, agenda_setter).

% Congress and federal courts charged with interpreting and enforcing the Second Amendment. Under the civic republican reading, the authority is constrained from disarming the citizenry entirely (cannot eliminate the right), but retains regulatory power to establish militia training, qualification, and safety standards that serve the civic-participation framework. The authority's mandate is to preserve armed citizenship as a structural check on tyranny while maintaining order and civic accountability.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, federal_regulatory_authority, agenda_setter,
    institutional, generational, analytical, national).

% State and local officials administering militia training, registration, and civic-participation mechanisms. They benefit from a constitutional doctrine that treats militia readiness as a legitimate regulatory domain and civic obligation. Their practical authority encompasses setting standards, conducting training, and integrating armed citizens into structured defense frameworks — all justified by the civic-participation norm rather than pure state control or pure individual liberty.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, state_militia_authorities, beneficiary,
    powerful, generational, arbitrage, national).

% The distributed public benefit of an armed populace capable of resisting governmental tyranny. This is not a concentrated actor but a structural condition: the right's justification rests on its role in maintaining the balance between government power and popular sovereignty. The constituency benefits from the constraint's persistence through the civilization-scale time horizon (protection against tyranny is a long-term structural condition, not a short-term tactical advantage).
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, tyranny_prevention_constituency, beneficiary,
    moderate, civilizational, analytical, national).

% Organized groups advocating for strong gun regulations or prohibition. Under the civic republican reading, they are excluded from the central justification framework — the reading grounds the right in a civic-participation and tyranny-prevention norm that gun-control advocates reject or subordinate to public-safety concerns. They would argue that widespread armed citizenship increases the risk of internal violence more than it prevents external tyranny, but that argument sits outside the civic republican frame (which takes tyranny prevention as foundational). Their exclusion is structural to this reading's logic, not accidental.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, gun_control_advocates, excluded,
    organized, biographical, constrained, national).

% Legal scholars, advocates, and courts that ground the Second Amendment in pre-political individual rights rather than civic duty. Under this reading, they would resist the civic republican frame's imposition of training, qualification, and civic-participation norms on the right to bear arms — arguing that the right is prior to government and cannot be conditioned on fulfilling civic duties. They are excluded from this reading's beneficiary structure because their interpretive frame treats individual liberty as foundational rather than balanced against civic obligation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, libertarian_individualist_reading_adherents, excluded,
    powerful, biographical, mobile, national).

% Legal scholars, advocates, and courts that read the Second Amendment as protecting only organized state militia authority, not individual ownership. Under this reading, they would reject the civic republican frame's integration of individual arms rights with militia duty — arguing that the right is tied to militia service and does not extend to private ownership outside that context. They are excluded from this reading's beneficiary structure because their interpretive frame treats militia organization as state-centric rather than citizen-rooted.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, collective_militia_reading_adherents, excluded,
    powerful, biographical, mobile, national).

% Constitutional scholars and courts examining the Second Amendment's structure and function. They observe how the civic republican reading positions itself between individualist and collectivist interpretations, and how that positioning generates specific regulatory possibilities (training standards, militia participation norms) that neither pure libertarian nor pure state-militia readings would permit or require.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, analytical_observer, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__civic_republican_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__civic_republican_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates armed citizenship as a civic institution: citizens maintain arms and militia readiness as a structural check on tyranny and a prerequisite for republican self-governance. The constraint solves the coordination problem of how to maintain popular sovereignty without either libertarian atomization (pure individual rights disconnected from civic duty) or state monopoly on force.
% TRANSFER_FUNCTION: Moves regulatory authority FROM federal power to disarm (which is constrained by the right) TO citizens and state militia (who gain protected access to arms and structured participation in civic defense). The constraint also transfers a duty of civic participation and training onto citizens — the right comes with an obligation to maintain readiness and comply with qualification norms.
% ABSENT_VOICES: Gun-control advocates (who would prioritize public safety over tyranny prevention) and libertarian individualists (who would reject civic-participation duties as conditions on the right) are structurally excluded from the civic republican frame. They would argue for either stricter regulation or unlimited individual ownership, positions that sit outside the constraint's justificatory logic. No party represents a purely state-militia reading in this stakeholder set, but that reading's adherents would also reject the civic republican integration of individual rights with collective duty.
% DISAPPEARANCE_RATIONALE: If the civic republican reading and its regulatory framework vanished, the constitutional balance would shift toward either pure libertarian individualism (no training or qualification requirements) or pure state-militia collectivism (no individual ownership rights outside militia context). Armed citizenship as a civic institution anchored in both individual right and civic duty would dissolve. The distribution of power between federal authority and armed citizens would reorganize around one of the competing readings.
% FOUNDING_PROBLEM: The founding problem is twofold: (1) how to preserve republican self-governance against tyranny when government has a monopoly on force, and (2) how to structure armed citizenship so it serves popular sovereignty rather than either anarchic individualism or state oppression. The civic republican reading grounds the Second Amendment in the necessity of an armed populace as a structural check on governmental power, while maintaining that the right is exercised within a framework of civic duty and qualification.
% FOUNDING_PROBLEM_CORROBORATION: Historians and constitutional scholars specializing in republican political theory (Gordon Wood, J.G.A. Pocock, Joyce Lee Malcolm on early-modern English and American militia traditions) corroborate the founding problem: the founding generation explicitly worried about tyranny and viewed an armed populace as a preventive mechanism. However, modern gun-control scholars and public-safety advocates attest that the founding problem is partially solved (modern standing armies and democratic institutions provide checks on tyranny without requiring mass gun ownership) and that armed-citizenry creates new problems (internal violence) not present in the founding era. Militia experts and National Guard officials (outside the libertarian or collectivist interpretive camps) affirm that civic participation frameworks can integrate training, safety, and arms access. The founding problem remains live in republican political theory but is increasingly contested in empirical public-safety contexts.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__civic_republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__civic_republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__civic_republican_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_arms_right__civic_republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__civic_republican_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__civic_republican_reading_tests).
:- end_tests(second_amendment_arms_right__civic_republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the civic republican reading imposes regulatory authority on the exercise of the right — training requirements, qualification standards, militia participation norms — that neither pure libertarian nor pure state-militia readings would permit. This is extractive from the perspective of someone who sees the right as absolutely pre-political, but coordinative from the perspective of someone who sees civic duty as intrinsic to republican self-governance. The measurement trajectory is nearly flat after t=20, reflecting that the civic republican frame stabilizes once established: the extractive elements (duty, qualification, participation) are constitutive, not accumulating. Suppression is low (0.22) because the constraint gains its force from civic legitimacy and the logic of tyranny prevention, not from coercion; federal authority is constrained from disarming citizens, so the enforced suppression is minimal. Theater is very low (0.12) because the constraint's functional operation (militia training, civic participation, arms access) is not substantially performative — it carries real civic and defensive logic. The slight rise from t=0 to t=20 reflects increasing professionalization and codification of militia standards; the plateau from t=20 onward suggests the framework stabilizes. The time grid is shared across all three metrics (every metric authored at every time point).
 *
 * PERSPECTIVAL GAP:
 *   The federal regulatory authority and armed citizens might compute different seat-level types. From the federal authority's seat, the constraint is genuine coordination of civic defense (moderate extraction, legitimized by popular sovereignty). From an armed citizen's seat who resists civic-duty framing, the same constraint looks more extractive (duties imposed on the right). From a libertarian individualist observer, it looks like a snare (the right is conditioned on government-defined participation). From a collective-militia observer, it looks like rope coordinating militia service without individual ownership freedom. The engine computes per-seat classifications from the structural data; the narrative context explains why these divergent readings are plausible.
 *
 * DIRECTIONALITY LOGIC:
 *   Armed citizens and state militia authorities are net beneficiaries: they gain protected access to arms and structured civic authority, respectively (d near beneficiary end). Federal regulatory authority sits near symmetric: it must respect the armed right but gains regulatory power over training and qualification (d ~0.5). The tyranny-prevention constituency is a distributed analytical beneficiary (long-term structural benefit, not concentrated). Gun-control and libertarian-individualist advocates are not in the stakeholder set at all, but if they were, they would be positioned as targets or excluded — their interests diverge from the civic republican frame. The directionality logic hinges on whether one accepts the civic-participation norm as legitimate: if legitimate, the constraint is coordinative; if illegitimate, the regulatory requirements are extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   The civic republican reading avoids mandatrophy (founding problem → atrophied function) by maintaining that the founding problem (tyranny prevention and republican self-governance) remains live. The constraint does not become a piton because the civic-participation framework keeps the militia function operationalized (training, qualification, civic integration). However, a competing mandatrophy claim is live: if internal-violence risk grows while tyranny-risk recedes, the founding problem atrophies and the constraint becomes performative (ritual civic participation without real defense logic). This competing verdict is captured in the omegas section.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_atrophy_mandate,
    'Does the founding problem (tyranny prevention via armed citizenry) remain live in contemporary democratic states with separation of powers, standing armies, and electoral mechanisms, or has it atrophied relative to new problems (mass gun violence, domestic extremism)?',
    'Empirical analysis of tyranny incidence in armed vs. disarmed democracies, and comparative public-safety outcomes; theoretical analysis of whether modern democratic institutions obsolete the founding problem; social science research on militia participation and its effects.',
    'If the founding problem has substantially atrophied, the constraint becomes mandatrophic (persists as civic theater while its justification vanishes), triggering reclassification toward piton. If the founding problem remains live, the constraint maintains its coordinative rope status. If empirically contested (some democracies see tyranny risk, others don''t), the constraint remains contested across stakeholder seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_atrophy_mandate, empirical, 'Whether tyranny prevention remains a live founding problem or has atrophied in contemporary democracies.').

omega_variable(
    civic_duty_vs_libertarian_priority,
    'Is the civic-participation norm (training, qualification, militia duty as condition on the right) a legitimate structural requirement of republican self-governance, or an extractive condition that violates the right''s pre-political status?',
    'Constitutional genealogy and original-intent analysis (how the founding generation viewed militia duty vs. individual ownership); comparative constitutional law examining how other democracies ground arms rights; deliberative democracy studies on whether civic duties enhance or diminish individual liberty.',
    'If civic duty is legitimate, the constraint is genuinely coordinative rope. If it violates pre-political rights, the constraint is extractive (snare or tangled rope). The resolution changes the baseline ε: legitimacy increases ε''s coordinative character; violations increase its extractive character. This is a framing question, not an empirical measurement — the impact is conceptual reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_duty_vs_libertarian_priority, conceptual, 'Whether civic-participation duty is legitimate structure or extractive condition on the right.').

omega_variable(
    individual_vs_collective_dimension_boundary,
    'Is there a coherent boundary between the individual right to bear arms and the collective civic-participation framework, or are they necessarily in tension such that emphasizing one dimension necessarily suppresses the other?',
    'Case-law analysis of whether courts can maintain both dimensions simultaneously (affirming individual ownership while enforcing training and qualification norms); empirical study of militia systems that integrate both; theoretical analysis of whether republicanism necessarily requires collective framing.',
    'If a coherent boundary exists, the civic republican reading is stable and maintains rope status. If the dimensions necessarily conflict, the reading is unstable and courts will be pulled toward either pure individualism or pure collectivism, fragmenting the constraint into competing types. Instability would suggest the reading is conceptually under-determined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_vs_collective_dimension_boundary, conceptual, 'Whether the individual and collective dimensions of the civic republican reading are coherently integrated or necessarily in tension.').

omega_variable(
    regulatory_capture_by_state_militia,
    'In practice, do civic-participation requirements become a mechanism for state militia authorities to gradually expand regulatory control over arms access, subordinating the individual right to collective state prerogatives?',
    'Historical analysis of militia-participation requirements and whether they tend to accumulate over time, gradually restricting individual ownership; comparative study of jurisdictions with different civic-participation frameworks; analysis of regulatory drift in training and qualification standards.',
    'If regulatory capture occurs systematically, the constraint drifts from rope toward tangled rope or snare (individual right becomes subordinated to state militia control). The extraction metric would increase over time, theater would rise (performative qualification standards), and the constraint would approach mandatrophy (civic duty framing becomes cover for state monopolization). If regulatory capture does not occur, the constraint maintains its coordinative balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_by_state_militia, empirical, 'Whether civic-participation requirements become a vector for gradual state regulatory capture of the individual right.').

omega_variable(
    internal_vs_external_threat_frame,
    'Is tyranny-prevention logic in the founding problem oriented primarily toward external threats (foreign invasion, federal overreach) or internal threats (state oppression), and does this framing affect which regulatory authorities are legitimate?',
    'Historical analysis of militia training and deployment in founding-era sources; examination of whether contemporary civic-participation frameworks address external or internal threats; analysis of how different threat framings distribute authority between federal and armed-citizen actors.',
    'If threat frame is external, federal military modernization may obsolete the citizen-militia role, making the constraint mandatrophic (no longer needed for tyranny prevention). If threat frame is internal, citizen militia remains relevant but creates tension with federal authority — regulatory conflicts between federal disarmament and citizen-armed-check logics intensify, pulling toward tangled rope. The framing affects whether the constraint is coordinative or conflictual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_vs_external_threat_frame, conceptual, 'Whether tyranny-prevention logic is oriented toward external or internal threats, and how this affects the legitimacy distribution of regulatory authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_arms_right__civic_republican_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(seco_tr_t0, observed).
narrative_ontology:measurement(seco_tr_t5, second_amendment_arms_right__civic_republican_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement_basis(seco_tr_t5, observed).
narrative_ontology:measurement(seco_tr_t10, second_amendment_arms_right__civic_republican_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(seco_tr_t10, observed).
narrative_ontology:measurement(seco_tr_t15, second_amendment_arms_right__civic_republican_reading, theater_ratio, 15, 0.11).
narrative_ontology:measurement_basis(seco_tr_t15, observed).
narrative_ontology:measurement(seco_tr_t20, second_amendment_arms_right__civic_republican_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(seco_tr_t20, observed).
narrative_ontology:measurement(seco_tr_t25, second_amendment_arms_right__civic_republican_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(seco_tr_t25, projected).
narrative_ontology:measurement(seco_tr_t30, second_amendment_arms_right__civic_republican_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement_basis(seco_tr_t30, projected).
narrative_ontology:measurement(seco_tr_t35, second_amendment_arms_right__civic_republican_reading, theater_ratio, 35, 0.12).
narrative_ontology:measurement_basis(seco_tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(seco_be_t0, observed).
narrative_ontology:measurement(seco_be_t5, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 5, 0.34).
narrative_ontology:measurement_basis(seco_be_t5, observed).
narrative_ontology:measurement(seco_be_t10, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement_basis(seco_be_t10, observed).
narrative_ontology:measurement(seco_be_t15, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement_basis(seco_be_t15, observed).
narrative_ontology:measurement(seco_be_t20, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(seco_be_t20, observed).
narrative_ontology:measurement(seco_be_t25, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(seco_be_t25, projected).
narrative_ontology:measurement(seco_be_t30, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(seco_be_t30, projected).
narrative_ontology:measurement(seco_be_t35, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 35, 0.38).
narrative_ontology:measurement_basis(seco_be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(seco_su_t0, observed).
narrative_ontology:measurement(seco_su_t5, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 5, 0.19).
narrative_ontology:measurement_basis(seco_su_t5, observed).
narrative_ontology:measurement(seco_su_t10, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement_basis(seco_su_t10, observed).
narrative_ontology:measurement(seco_su_t15, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 15, 0.21).
narrative_ontology:measurement_basis(seco_su_t15, observed).
narrative_ontology:measurement(seco_su_t20, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement_basis(seco_su_t20, observed).
narrative_ontology:measurement(seco_su_t25, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 25, 0.22).
narrative_ontology:measurement_basis(seco_su_t25, projected).
narrative_ontology:measurement(seco_su_t30, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement_basis(seco_su_t30, projected).
narrative_ontology:measurement(seco_su_t35, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 35, 0.22).
narrative_ontology:measurement_basis(seco_su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__civic_republican_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__collective_right_reading).

% DUAL FORMULATION NOTE:
% The civic republican reading is one reading of the contested kernel second_amendment_arms_right. It shares the referent (the Second Amendment's constitutional meaning) with sibling readings (individual_right_reading and collective_right_reading) but instantiates a structurally distinct constraint with its own beneficiary/victim structure, regulatory logic, and ε value. The civic republican reading treats the right as integrating individual ownership with collective civic duty; the individual-right reading treats the right as prior to government and resistant to duty-framing; the collective-right reading ties the right to state-militia control and denies individual ownership. Each reading generates a different constraint with different implications for regulatory authority, citizen power, and the tyranny-prevention logic. The network links reflect how the civic republican reading influences (but does not foreclose) the other readings — it creates structural pressure toward civic-participation frameworks that the libertarian-individualist reading resists and that the state-militia reading would subordinate to federal control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
