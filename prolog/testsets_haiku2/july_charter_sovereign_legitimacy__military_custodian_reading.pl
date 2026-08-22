% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__military_custodian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__military_custodian_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__military_custodian_reading
 *   human_readable: Charter Military Custodian Reading: Permanent Institutional Guardian Authority
 *   domain: constitutional_law/political_transition/post_revolutionary_state_building
 *
 * SUMMARY:
 *   A post-revolutionary charter ratifies the military officer corps as the
 *   permanent constitutional guardian of state stability and ideological
 *   continuity. Under this reading, military veto authority over civilian
 *   institutions is not temporary emergency governance but a structural
 *   feature of the post-revolutionary order itself—justified by the founding
 *   narrative that civilian factionalism threatens state cohesion. The
 *   constraint bundles genuine post-revolutionary coordination (preventing
 *   factional civil war) with asymmetric extraction (military institutional
 *   autonomy, veto over civilian contestation, suppression of movements that
 *   challenge military authority). This is ONE reading of a contested kernel:
 *   the secular-democratic reading would subordinate the military to civilian
 *   authority; the guided-nationalism reading would ground sovereignty in
 *   religious identity rather than institutional guardianship. The
 *   claim/metric gap is intentional: this reading CLAIMS the arrangement as
 *   tangled_rope (real coordination + enforced asymmetry); authored metrics
 *   describe rising extractiveness and suppression intensification over time,
 *   indicating institutional hardening.
 *
 * KEY AGENTS:
 *   - military_officer_corps: agenda-setter and structural beneficiary — sets security red lines, controls veto authority, gains institutional autonomy and budgetary protection
 *   - autonomous_political_parties: payer and constrained political actor — operate within military-defined boundaries, subordinated on security/foreign policy
 *   - student_movement: victim and powerless actor — systematically suppressed when organizing outside military-approved channels; trapped within national borders
 *   - civil_society_organizations: payer and constrained — operate under licensing frameworks that implicitly accept military custodianship
 *   - executive_civilian_leadership: dual-positioned beneficiary/payer — gains legitimacy from charter but constrained by military veto; performs civilian authority while administering military constraints
 *   - revolutionary_founding_cohort: institutional beneficiary — protected by constraint against future contestation of post-revolutionary order
 *   - religious_establishment: institutional beneficiary — gains protection from military against secular challenges to ideological interpretation
 *   - exiled_opposition: structurally excluded — would contest military custodian reading but are forced into exile or underground; their absence from conversation is enforced by security apparatus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, 0.78).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__military_custodian_reading, 0.81).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__military_custodian_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__military_custodian_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__military_custodian_reading, "Charter Military Custodian Reading: Permanent Institutional Guardian Authority").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__military_custodian_reading, "constitutional_law/political_transition/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__military_custodian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__military_custodian_reading, '97661d31-d383-4556-92e8-9c3b64e64970').
narrative_ontology:cs_kernel_codification('97661d31-d383-4556-92e8-9c3b64e64970', formalized).
narrative_ontology:cs_authority_grounding('97661d31-d383-4556-92e8-9c3b64e64970', extraction).
narrative_ontology:cs_interpretation_layer_present('97661d31-d383-4556-92e8-9c3b64e64970').
narrative_ontology:cs_reading_relation('97661d31-d383-4556-92e8-9c3b64e64970', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('97661d31-d383-4556-92e8-9c3b64e64970', july_charter_sovereign_legitimacy__guided_nationalism_reading, coexists_with).
narrative_ontology:cs_axiom('97661d31-d383-4556-92e8-9c3b64e64970', foundational, permanent_military_guardianship_necessary).
narrative_ontology:cs_axiom_status(permanent_military_guardianship_necessary, holdable).
narrative_ontology:cs_axiom_grounding('97661d31-d383-4556-92e8-9c3b64e64970', permanent_military_guardianship_necessary, empirically_contingent).
narrative_ontology:cs_axiom('97661d31-d383-4556-92e8-9c3b64e64970', secondary, civilian_contestation_destabilizes_state).
narrative_ontology:cs_axiom_status(civilian_contestation_destabilizes_state, holdable).
narrative_ontology:cs_axiom_grounding('97661d31-d383-4556-92e8-9c3b64e64970', civilian_contestation_destabilizes_state, empirically_contingent).
narrative_ontology:cs_reference_frame('97661d31-d383-4556-92e8-9c3b64e64970', post_revolutionary_military_institutional_continuity).
narrative_ontology:cs_drift_state('97661d31-d383-4556-92e8-9c3b64e64970', contemporary_third_generation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('97661d31-d383-4556-92e8-9c3b64e64970', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_officer_corps).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, security_apparatus).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, civil_society_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, executive_civilian_leadership).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, revolutionary_founding_cohort).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, religious_establishment).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, executive_civilian_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Positioned by the charter as the permanent guardian of state stability and constitutional order. Controls appointment of key security posts, retains veto authority over executive decisions affecting national security, and administers the internal security apparatus. Justifies its authority as necessary to prevent ideological fragmentation and ensure continuity of the post-revolutionary state. Collects institutional prestige, budgetary autonomy, and de facto veto power over civilian political contestation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_officer_corps, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__military_custodian_reading, military_officer_corps, beneficiary).

% Nominally participate in electoral and parliamentary processes under the charter, but their autonomy is bounded by military red lines on ideological content, party structure, and external relations. They must accept that significant policy domains (security, foreign relations, religious doctrine interpretation) remain outside civilian control. Exit means dissolution or exile; reformation inside the framework requires accepting permanent subordination.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties, payer,
    moderate, biographical, constrained, national).

% Activists and student organizations face systematic suppression when organizing around autonomy, secular governance, or criticism of military authority. The security apparatus treats unauthorized political mobilization as destabilizing. Students are trapped within the national territory and its education system; exit means permanent displacement. Their organizing capacity is regularly disrupted by detention and surveillance.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement, payer,
    powerless, biographical, trapped, national).

% NGOs, human rights groups, and civic associations operate under licensing frameworks that require implicit acceptance of military custodianship. Those that challenge military authority on governance, detention, or security operations face de-registration or funding cutoffs. Their ability to organize collective action is contingent on staying within bounds the security apparatus defines as non-threatening.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, civil_society_organizations, payer,
    moderate, biographical, constrained, national).

% Civilians in executive positions gain legitimacy from the charter's ratification and implement day-to-day governance. But their authority is structurally capped: major decisions require military consent, particularly on security policy, foreign relations, and ideological boundaries. They benefit from the appearance of civilian authority while actual veto power sits with the military. This dual position creates performance pressure — they must implement civilian rhetoric while administering military constraints.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, executive_civilian_leadership, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__military_custodian_reading, executive_civilian_leadership, payer).

% Military and civilian revolutionary leaders who authored the post-revolutionary order gain institutional continuity through the charter. The military custodian reading codifies their authority against future civilian contestation. They benefit from the constraint's protection of revolutionary ideology and institutional hierarchy against democratic dilution. Over time this founding cohort ages out, but the constraint persists as institutional inertia.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, revolutionary_founding_cohort, beneficiary,
    institutional, generational, arbitrage, national).

% Religious clerics and institutions with aligned ideological commitments to the post-revolutionary order gain structural protection from the military's custodian role. The military defends religious doctrine interpretation against secular contestation. This creates coupling: military authority and religious authority reinforce each other's legitimacy. Secular or pluralist movements that would contest religious establishment get suppressed as threats to state stability.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, religious_establishment, beneficiary,
    institutional, generational, arbitrage, national).

% Foreign governments, human rights monitors, and international institutions watch the constraint's operation. They document patterns of suppression, analyze whether military veto authority prevents democratic transition, and produce reports that influence sanctions, aid flows, and diplomatic standing. They sit outside the constraint but their assessments affect its external legitimacy.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, international_observers, observer,
    analytical, biographical, analytical, global).

% Political parties, activists, and ideological movements that reject the military custodian reading are forced into exile or underground operation. They would contest the charter's ratification of military authority, advocate for civilian supremacy, and mobilize alternative readings of post-revolutionary legitimacy. Their exclusion from the conversation is enforced by the security apparatus; their return requires accepting the custodian reading or overthrowing it.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, exiled_opposition, excluded,
    moderate, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__military_custodian_reading, military_officer_corps).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__military_custodian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The military custodian arrangement solves a real post-revolutionary coordination problem: how to maintain state continuity, prevent ideological fragmentation across competing visions of the revolution's meaning, and hold together a coalition of military and civilian elites around a shared institutional order when civil conflict threatens. The charter provides a framework where both military and civilian institutions claim legitimacy and governance is distributed (however asymmetrically) between them rather than collapsing into either full military rule or civil war.
% TRANSFER_FUNCTION: The constraint transfers political autonomy and governance authority from autonomous civil institutions (parties, movements, civic associations) to the military officer corps and security apparatus. It moves decision-making power over security, foreign relations, and ideological boundaries from elected or appointed civilian positions into the military's veto authority. It also transfers institutional legitimacy: military authority becomes constitutional and permanent rather than temporary or conditional.
% ABSENT_VOICES: Secular democratic movements, secular intellectuals and academics, autonomous student organizations, and human rights advocates are systematically excluded from the charter's framing. They would argue that the military custodian reading contradicts post-revolutionary democratic commitments, that perpetual military guardianship is incompatible with civilian self-rule, and that security stability should rest on rule of law and civilian oversight rather than officer-corps discretion. Their exclusion is structural: they are either in exile, underground, or suppressed within the national conversation.
% DISAPPEARANCE_RATIONALE: If the military custodian reading and its enforcement apparatus vanished overnight, political contestation would immediately expand: autonomous parties would challenge ideological red lines, student movements would re-mobilize, secular reformers would emerge from exile or hiding, and religious authority would face civilian scrutiny unconstrained by military protection. The state's institutional architecture would either consolidate into a civilian-democratic model, fragment into competing power centers, or undergo renewed civil conflict. The outcome would depend on whether the military could be subordinated to civilian authority or whether the dissolution of the constraint would trigger counter-mobilization by military hardliners. The post-revolutionary order itself would become contestable.
% FOUNDING_PROBLEM: The post-revolutionary state faced a coordination crisis: multiple factions with incompatible visions of the revolution's proper meaning competed for control; civil war and ideological fragmentation threatened institutional collapse; neither pure military dictatorship nor unguarded democratic opening seemed viable. The charter was authored to lock in a hybrid arrangement where military institutional continuity and civilian governance coexist under a shared framework that prevents any single faction from imposing its vision unilaterally.
% FOUNDING_PROBLEM_CORROBORATION: Military and founding civilian leadership attest the problem remains live: ideological fragmentation and security threats justify permanent custodianship. International observers, human rights advocates, and exiled opposition movements attest the founding problem is substantially resolved and the constraint now persists as institutional power-preservation; academic analysis of comparable post-revolutionary transitions and testimony from democratic reformers outside the benefiting parties support this reading. The contest turns on whether military veto is still necessary or has become entrenched cover for suppression.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__military_custodian_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__military_custodian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__military_custodian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78 at interval end) is high because military institutional autonomy is decoupled from civilian oversight and grows over time; the charter ratifies permanent veto authority rather than temporary emergency measures. Suppression (0.81) is higher still because the constraint's persistence depends on systematically preventing autonomous political contestation—parties must self-censor on security matters, student movements are disrupted before mobilization reaches scale, and civil society operates under implicit threat. Theater (0.42, rising from 0.28) indicates meaningful performance work: the constraint presents as post-revolutionary constitutionalism while functioning as institutional entrenchment. Accessibility collapse (0.68) is moderate because alternatives exist (secular democracy, religious nationalism) but are actively foreclosed by security apparatus; parties know the alternative readings but cannot voice them without penalty. Resistance (0.72) is substantial because autonomous actors continually test boundaries and are suppressed for it—student movements re-emerge periodically, parties probe security red lines, and exiled opposition maintains pressure through international advocacy. The time series across all metrics shows enforcement intensification (all three metrics rise monotonically from t0 to t40), indicating the constraint hardened over its interval as the founding generation aged out and subsequent cohorts solidified military guardianship into institutional routine.
 *
 * PERSPECTIVAL GAP:
 *   The military agenda-setter and the student-movement payer sit at opposite ends of the directionality spectrum (d ≈ 0.20 vs. d ≈ 0.88) despite ostensibly sharing membership in the post-revolutionary order. Why? The military controls the rule system and benefits from its continuity; students are trapped within it and bear its costs (surveillance, arrested development, delayed political voice). This structural asymmetry is the constraint's defining feature. The executive civilian leadership's dual role creates a distinctive seat: they gain title and ceremonial authority (beneficiary move) while their actual power is capped by military veto (payer move). This is not hypocrisy or ambivalence—it is structural dual-positioning that the engine should compute and flag as a seat with internal tension.
 *
 * DIRECTIONALITY LOGIC:
 *   Military officer corps: structural beneficiary, institutional power, arbitrage exit (can shift internationally or to private security), so d approaches 0.15–0.25 (beneficiary subsidy). Autonomous political parties: moderate power but constrained exit (cannot leave national politics), pay through autonomy sacrifice, so d approaches 0.70–0.78 (target extraction). Student movement: powerless, trapped exit (no international mobility, education system constrained), highest victim status, so d approaches 0.85–0.92 (full target). Civil society organizations: moderate power, constrained exit (licensed operation or forced closure), payer role, so d approaches 0.60–0.72. Revolutionary founding cohort and religious establishment: institutional power, arbitrage exit, beneficiary roles tied to military protection, so d approaches 0.15–0.25. Executive civilian leadership: powerful but constrained by military veto, dual role (perform authority while subordinated), so d approaches 0.45–0.55 (symmetric or slightly target-tilted). Exiled opposition: moderate power but structurally excluded and trapped (cannot safely return), so their d would be near 1.0 if they were in the conversation—their exclusion is the enforcement mechanism itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is post-revolutionary civil war and ideological fragmentation—a live, real problem that military custodianship addressed at t0. The measurements show rising extractiveness and suppression over 40 time units: suppression_requirement from 0.61 → 0.81, base_extractiveness from 0.58 → 0.78. This is NOT a constraint whose founding problem is dead (which would trigger mandatrophy signals) but one whose founding function has been achieved and now the constraint persists for institutional reasons. The rising suppression signature indicates enforcement intensification—the founding generation is gone, but subsequent military cohorts have hardened custodianship into routine institutional practice. Theater is rising (0.28 → 0.42) but not dominant (< 0.5), suggesting the constraint retains real function alongside performance. The mandatrophy resolver: if disappearance_verdict (world_rearranges) matches founding_problem_status (contested), the mismatch consumer does NOT fire (both contestable). But the rising suppression + rising theater + moderate theater_ratio pattern flags a transition zone: the constraint retains functional justification but is drifting toward piton territory if suppression continues rising and alternatives remain foreclosed. Omega omega_institutional_hardening captures this trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    military_necessity_vs_institutional_entrenchment,
    'Is military custodian authority necessary to prevent ideological fragmentation and civil war, or has the post-revolutionary coalition solidified enough that custodianship now functions primarily as institutional entrenchment?',
    'Controlled relaxation experiment: jurisdictions that have transitioned from military guardianship to civilian democratic rule while maintaining post-revolutionary institutional continuity can serve as comparison cases. Domestic evidence would include: (1) whether autonomous political movements emerge peacefully when suppression is reduced; (2) whether military hardliners mobilize counter-insurgency when custodian authority is curtailed; (3) longitudinal stability indicators (state capacity, violence trends) in transition scenarios.',
    'If necessity is established: military veto remains structurally justified and the constraint stays tangled_rope (real coordination + real enforcement). If entrenchment is established: the coordination function has been achieved and suppression now serves institutional power-preservation, shifting the constraint toward snare or piton. This is the gate between legitimate post-revolutionary stability mechanism and illegitimate power monopoly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(military_necessity_vs_institutional_entrenchment, empirical, 'Whether military custodian authority remains functionally necessary or has become institutionally entrenched power-preservation.').

omega_variable(
    autonomy_constraint_internalization,
    'Has autonomous-actor suppression (student movements, parties, civil society) caused those actors to internalize the custodian reading as legitimate, or are they constrained by external coercion and would mobilize differently if suppression were removed?',
    'Post-relaxation behavioral trajectory: if actors maintain subordination voluntarily, internalization has occurred; if they immediately expand contestation when external suppression is reduced or removed, the constraint operates primarily through coercion rather than legitimacy acceptance.',
    'If internalized: suppression is lower than the structural measure suggests and the constraint has achieved deeper control (identity-lock). If coercive: suppression is accurate and the constraint relies on continuous active enforcement. This distinguishes whether the constraint has captured the targets'' self-conception or only their behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_constraint_internalization, empirical, 'Whether suppression operates through internalized legitimacy or external coercion.').

omega_variable(
    alternative_reading_foreclosure,
    'Do the military-custodian and secular-democratic readings logically foreclose each other within a single post-revolutionary framework, or can they coexist as different parties'' simultaneous commitments?',
    'Constitutional interpretation analysis: If the charter''s text can be read to permit both military veto and civilian supremacy (ambiguous or distributed authority), the readings coexist. If the charter explicitly codifies military guardianship as permanent and inalienable, the readings foreclose each other (choosing one requires denying the other''s constitutional validity).',
    'If foreclosure: one reading must ultimately prevail and the other cannot be adopted while honoring the charter; this routes to revolution or rewriting. If coexistence: the readings can be held by different factions in ongoing political dispute without one logically eliminating the other; this routes to political contestation and potential compromise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Whether military-custodian and secular-democratic readings logically foreclose each other or coexist as live alternatives.').

omega_variable(
    omega_institutional_hardening,
    'As the founding generation of military and civilian leaders ages out, does military custodianship harden into institutional routine that can no longer justify itself by appeal to founding-crisis necessity, increasing the risk that subsequent generations perceive it as illegitimate entrenchment?',
    'Generational transition analysis: when founding-cohort military leaders retire and are replaced by officers who did not author the original compromise, do their successors maintain custodian authority with the same founding narrative, or adopt a different justification (institutional tradition, institutional identity, performance routine)? If justification shifts, the constraint becomes vulnerable to delegitimization.',
    'If hardening occurs without narrative shift: the constraint becomes piton-adjacent (persists by institutional inertia despite weakened founding legitimacy). If narrative evolves: the constraint may stabilize under a new legitimacy claim (institutional tradition replaces founding necessity). If successors cannot defend the constraint, it becomes politically vulnerable to acceleration toward secular-democratic or guided-nationalism readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_institutional_hardening, empirical, 'Generational succession risk to military custodian narrative as founding-crisis justification ages out.').

omega_variable(
    kernel_reading_committer_stakes,
    'Does authoring this reading as military_custodian_reading rather than collapsing the charter into a single constraint-object change which facts about the post-revolutionary order get highlighted in analysis?',
    'Comparative reading analysis: the secular-democratic reading would highlight autonomy-suppression and victim testimony; the guided-nationalism reading would highlight religious-establishment coupling; the military-custodian reading (this one) highlights institutional-veto authority and enforcement intensification. Each reading makes different structural facts salient. The question is whether the decomposition itself is sound or whether the readings over-represent one reading''s framing.',
    'If the decomposition is sound: each reading captures a genuinely different ε and different victim/beneficiary topology, justifying three separate constraint stories. If the decomposition over-represents military framing: the military-custodian reading should be reclassified as a snare (pure extraction with coordination cover) rather than tangled_rope. This is the ε-invariance test applied to kernel readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_stakes, conceptual, 'Whether the three kernel readings decompose the charter legitimately or whether military-custodian reading over-claims its coordination function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__military_custodian_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(july_tr_t0, observed).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(july_tr_t5, observed).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(july_tr_t10, observed).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(july_tr_t15, observed).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(july_tr_t20, observed).
narrative_ontology:measurement(july_tr_t25, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(july_tr_t25, observed).
narrative_ontology:measurement(july_tr_t30, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(july_tr_t30, observed).
narrative_ontology:measurement(july_tr_t40, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(july_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(july_be_t0, observed).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 5, 0.63).
narrative_ontology:measurement_basis(july_be_t5, observed).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(july_be_t10, observed).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement_basis(july_be_t15, observed).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement_basis(july_be_t20, observed).
narrative_ontology:measurement(july_be_t25, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement_basis(july_be_t25, observed).
narrative_ontology:measurement(july_be_t30, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement_basis(july_be_t30, observed).
narrative_ontology:measurement(july_be_t40, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(july_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0, 0.61).
narrative_ontology:measurement_basis(july_su_t0, observed).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 5, 0.66).
narrative_ontology:measurement_basis(july_su_t5, observed).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(july_su_t10, observed).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement_basis(july_su_t15, observed).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement_basis(july_su_t20, observed).
narrative_ontology:measurement(july_su_t25, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 25, 0.79).
narrative_ontology:measurement_basis(july_su_t25, observed).
narrative_ontology:measurement(july_su_t30, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 30, 0.8).
narrative_ontology:measurement_basis(july_su_t30, observed).
narrative_ontology:measurement(july_su_t40, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 40, 0.81).
narrative_ontology:measurement_basis(july_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__military_custodian_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__military_custodian_reading, 0.12).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested july_charter_sovereign_legitimacy kernel. Each reading (military_custodian, secular_democratic, guided_nationalism) instantiates a different constraint with distinct ε, victim/beneficiary topology, and classified type. The three stories are linked via network.affects_constraints and represent genuine structural alternatives rather than observables of a single constraint. See commentary.kernel_context for the foundational axiom differences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__military_custodian_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
