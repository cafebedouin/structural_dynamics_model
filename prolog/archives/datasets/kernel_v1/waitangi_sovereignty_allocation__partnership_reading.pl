% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__partnership_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__partnership_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: waitangi_sovereignty_allocation__partnership_reading
 *   human_readable: Treaty of Waitangi Partnership Reading: Ongoing Crown-Māori Governance
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) represents a foundational commitment in New
 *   Zealand's constitutional order, but its meaning has been radically
 *   contested since inception. The partnership_reading interprets the Treaty
 *   as establishing an ongoing governance relationship between Crown and
 *   Māori, with consultation obligations, resource protections, and
 *   enforceable principles constraining Crown authority. This reading emerged
 *   formally in the Waitangi Tribunal (1985) and was gradually incorporated
 *   into case law (1989-1995). The partnership_reading is NOT the only
 *   plausible interpretation of the same text. The rival
 *   crown_sovereignty_reading treats the Treaty as a historical agreement
 *   whose obligations have been discharged through legislation and
 *   settlement, leaving Crown sovereignty supreme and Parliament
 *   unconstrained. The rival rangatiratanga_reading interprets Article Two as
 *   transferring chiefly authority to Māori, making Crown recognition of that
 *   authority the core obligation, not ongoing consultation. This story
 *   instantiates the partnership_reading as a single constraint with its own
 *   extractiveness, suppression, and theatrical performance profile. The
 *   reading stabilized the Treaty's ambiguous text into a formal interpretive
 *   principle — 'the principles of the Treaty' — that courts could apply
 *   across domains. Over the 1975-2005 interval, the partnership_reading
 *   gained institutional force (Waitangi Tribunal, case law, policy mandates)
 *   while simultaneously showing rising theater (consultation processes
 *   expanded while binding power remained limited). The extractiveness
 *   actually decreased over the interval as Māori capacity to assert
 *   counterclaims grew, but suppression also fell as legal pathways opened.
 *
 * KEY AGENTS:
 *   - Crown Executive and Parliament: Primary beneficiary (institutional/arbitrage) — retains ultimate authority while appearing responsive to partnership obligations; can selectively deepen or shallow partnership based on political convenience
 *   - Organized Iwi Leadership: Secondary beneficiary and victim (organized/constrained) — gains formal recognition, policy leverage, and settlement benefits; constrained by Parliamentary sovereignty doctrine; experiences mix of coordination and extraction
 *   - Māori Communities Without Formal Settlement: Primary victim (powerless/trapped) — affected by consultation framework but lack direct seat at negotiation; trapped within Crown-defined partnership structure
 *   - Judiciary: Institutional mediator (institutional/constrained) — interprets partnership obligations but cannot override Parliament; constrained by Parliamentary supremacy doctrine; genuine agency through principles jurisprudence
 *   - Waitangi Tribunal: Quasi-judicial interpreter (organized/constrained) — formalized the partnership_reading; constrained by recommendations-only authority; influenced judicial and political discourse significantly
 *   - Treaty Settlement Bureaucracy: Institutional process (institutional/arbitrage) — manages grievances and negotiates settlements; theatrical institution (processes elaborate without fundamental power transfer)
 *   - Analytical Observer: Detached perspective (analytical/analytical) — risks treating partnership as natural law of constitutional order when it is a contingent reading choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, 0.38).
domain_priors:suppression_score(waitangi_sovereignty_allocation__partnership_reading, 0.48).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__partnership_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__partnership_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__partnership_reading, "Treaty of Waitangi Partnership Reading: Ongoing Crown-Māori Governance").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__partnership_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__partnership_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__partnership_reading, 'e70b4bfd-0f2d-479d-995c-ebbd1d283805').
narrative_ontology:cs_kernel_codification('e70b4bfd-0f2d-479d-995c-ebbd1d283805', fixed_text).
narrative_ontology:cs_authority_grounding('e70b4bfd-0f2d-479d-995c-ebbd1d283805', lineage).
narrative_ontology:cs_interpretation_layer_present('e70b4bfd-0f2d-479d-995c-ebbd1d283805').
narrative_ontology:cs_reading_relation('e70b4bfd-0f2d-479d-995c-ebbd1d283805', waitangi_sovereignty_allocation__crown_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('e70b4bfd-0f2d-479d-995c-ebbd1d283805', waitangi_sovereignty_allocation__rangatiratanga_reading, influences).
narrative_ontology:cs_axiom('e70b4bfd-0f2d-479d-995c-ebbd1d283805', foundational, crown_consultation_obligation_binding).
narrative_ontology:cs_axiom_status(crown_consultation_obligation_binding, holdable).
narrative_ontology:cs_axiom_grounding('e70b4bfd-0f2d-479d-995c-ebbd1d283805', crown_consultation_obligation_binding, deontological).
narrative_ontology:cs_axiom('e70b4bfd-0f2d-479d-995c-ebbd1d283805', foundational, parliamentary_sovereignty_preserved).
narrative_ontology:cs_axiom_status(parliamentary_sovereignty_preserved, holdable).
narrative_ontology:cs_axiom_grounding('e70b4bfd-0f2d-479d-995c-ebbd1d283805', parliamentary_sovereignty_preserved, conventional).
narrative_ontology:cs_axiom('e70b4bfd-0f2d-479d-995c-ebbd1d283805', secondary, principles_doctrine_judicially_enforceable).
narrative_ontology:cs_axiom_status(principles_doctrine_judicially_enforceable, holdable).
narrative_ontology:cs_axiom_grounding('e70b4bfd-0f2d-479d-995c-ebbd1d283805', principles_doctrine_judicially_enforceable, conventional).
narrative_ontology:cs_reference_frame('e70b4bfd-0f2d-479d-995c-ebbd1d283805', ongoing_partnership_requiring_active_protection).
narrative_ontology:cs_drift_state('e70b4bfd-0f2d-479d-995c-ebbd1d283805', contemporary_2005, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('e70b4bfd-0f2d-479d-995c-ebbd1d283805', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, crown_executive).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_organizations).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, rangatiratanga_authority).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, maori_resource_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MĀORI COMMUNITIES DEPENDENT ON CONSULTATION (SNARE) — Trapped within a framework that promises partnership but subordinates their authority to Crown discretion. Consultation rights exist but lack binding force; 'good faith' is procedural, not substantive. Exit from the relationship is not available (territorial, historical, relational embededness). Maximum extraction experienced: the appearance of partnership masks Crown sovereignty preservation. Theater is high — elaborate consultation processes substitute for actual power transfer.
constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__partnership_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED IWI LEADERSHIP (TANGLED ROPE) — Can exit through litigation, policy advocacy, and coalition pressure; possess organizational capacity and legal standing. Experience genuine coordination function (co-management arrangements, settlement protocols, joint decision-making in specific domains) alongside asymmetric extraction (Crown retains override authority, consultation outcomes non-binding). Significant agency but constrained by Parliamentary sovereignty doctrine.
constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__partnership_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CROWN EXECUTIVE (ROPE) — Experiences the constraint as coordination mechanism enabling legitimate Crown governance while managing political risk of indigenous grievance. Consultation requirement is coordination cost, not extraction — it solves Crown's need to maintain legitimacy and reduce political instability. Net beneficiary: arbitrage option allows Crown to adjust partnership depth across policy domains based on political convenience. Effective extraction runs away from this agent.
constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__partnership_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: JUDICIARY (TANGLED ROPE) — Institutionally constrained by precedent and Parliamentary supremacy doctrine; cannot override legislative will. Yet possesses real interpretive power (principles jurisprudence has expanded Treaty protections beyond text). Experiences hybrid: genuine coordination function (clarifying ambiguous obligations) alongside structural extraction (serves Crown's interest in managed, rule-bound indigeneity rather than authentic power-sharing). Bounded agency through doctrine.
constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__partnership_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TREATY SETTLEMENT BUREAUCRACY (PITON) — Performative institutional structure that processes grievances without transferring substantive authority. Settlement negotiations, Office of Treaty Settlements, mandated consultation frameworks are theatrical: they signal commitment to partnership while preserving Crown control. Theater ratio high (0.62) — elaborate process rituals substitute for actual sovereignity redistribution. Institution persists through inertia; primary function (addressing historical injustice) has atrophied while procedural form remains.
constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__partnership_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PARLIAMENTARY SOVEREIGNTY VIEW (MOUNTAIN) — From the universal/civilizational analytical perspective, this constraint may appear as an immutable feature of Westminster constitutional order: no Parliament can bind its successors; Crown sovereignty is legally inalienable; treaty obligations derive from legislative grace. This perspective treats partnership as aspirational but structurally impossible — a natural law limit on what any constitutional framework can deliver. ENGINE ALERT: False summit candidate. The structural data reveals this 'immutability' as a choice (the partnership_reading itself) not a law of nature. The rival crown_sovereignty_reading and rangatiratanga_reading instantiate different constitutional orderings, not interpretations of an invariant limit.
constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__partnership_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__partnership_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(waitangi_sovereignty_allocation__partnership_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(waitangi_sovereignty_allocation__partnership_reading, TR),
    TR >= 0.70.

:- end_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low. The partnership reading allows Māori to claim rights, file claims, negotiate settlements, and establish co-management arrangements in specific domains. The extraction experienced by Māori communities is real but mitigated by their ability to organize legally and achieve partial recognition. The extracted value is subordination of rangatiratanga (chiefly authority) to consultation rights (advisory power) — a real loss of potential autonomy, but not total denial. The Crown's extraction is the preservation of ultimate sovereignty while appearing to share power. The intermediate value (not low like rope, not high like snare) reflects genuine coordination gains (settlement protocols, co-management) alongside real asymmetry (Crown retains override). Suppression (0.48): Moderate. Barriers to exit include: territorial embededness (Māori cannot leave the nation), historical dependency relationships, Crown control of litigation timelines and settlement terms, Parliamentary sovereignty doctrine preventing judicial override. But suppression is not total — legal channels exist, litigation succeeds partially, coalition pressure produces policy change. Over the interval, suppression decreased as litigation success increased and legal paths widened. Theater ratio (0.62): High and rising. The partnership reading requires extensive consultation processes, establishment of the Waitangi Tribunal, creation of Office of Treaty Settlements, and mandated consultation in various policy domains. The performative element is high: these processes signal commitment to partnership while preserving Crown authority to reject consultation outcomes. Theater increased from 0.45 (1975, before formal jurisprudence) to 0.62 (2005, after Tribunal and case law established principles doctrine). The court system and settlement apparatus are increasingly theatrical — elaborate procedure substituting for substantive power transfer.
 *
 * PERSPECTIVAL GAP:
 *   The partnership reading produces radically different classifications depending on the agent's structural position. The Crown sees coordination (Rope) — the consultation requirement solves legitimacy problems. Organized iwi see mixed coordination and extraction (Tangled Rope) — settlement gains are real but subordinated to Crown authority. Powerless Māori communities see pure extraction (Snare) — promised partnership without binding effect. The judiciary sees bounded coordination (Tangled Rope) — genuine interpretive power constrained by Parliamentary supremacy. The settlement bureaucracy sees its own degradation (Piton) — increasingly elaborate processes with declining substantive function. The analytical observer risks treating Parliamentary supremacy as a natural law (Mountain) when it is itself a reading choice. The perspectival gap reveals that 'partnership' means radically different things depending on who is measuring it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. The Crown (institutional/arbitrage) is a net beneficiary with maximum exit — derives d ≈ 0.05-0.15, producing negative or minimal f(d), low χ. Organized iwi (organized/constrained) are both beneficiaries (settlement gains, co-management) and victims (subordinated authority) — mixed status produces d ≈ 0.45-0.55, producing moderate f(d) ≈ 0.65, moderate χ. Powerless Māori (powerless/trapped) are pure victims with no exit — derives d ≈ 0.95, producing high f(d) ≈ 1.42, high χ. Judiciary (institutional/constrained) operates under doctrine constraint producing d ≈ 0.50-0.60, producing moderate f(d). The settlement bureaucracy experiences its own agency (arbitrage-like flexibility in procedure design) while constrained by declining functional relevance — d ≈ 0.35-0.45. The partnership reading's overall χ is anchored by the powerless agents' experience but modulated by the organized iwi's capacity to assert counterclaims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consultation_binding_force_ambiguity,
    'Does ''good faith consultation'' create enforceable obligations or merely procedural requirements? Can courts compel substantive outcomes from consultation, or only process compliance?',
    'Jurisprudential analysis of Treaty claims doctrine; examination of successful vs defeated cases challenging Crown consultation adequacy; international comparison with duty-to-consult jurisprudence in other post-colonial states',
    'If substantive: partnership reading approaches rangatiratanga_reading (Māori authority protected). If procedural only: partnership reading collapses toward crown_sovereignty_reading (partnership is theater). Current doctrine is unstable on this axis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consultation_binding_force_ambiguity, empirical, 'Whether good faith consultation creates enforceable substantive outcomes or only procedural compliance').

omega_variable(
    settlement_finality_vs_ongoing_partnership,
    'Do Treaty settlements (financial redress + rights recognition) discharge Crown''s partnership obligation, or do they establish ongoing co-governance structures requiring active negotiation?',
    'Longitudinal analysis of settlement implementation; tracking whether settlement-phase consultation obligations continue post-settlement or terminate; examination of revised settlement protocols over 30-year period',
    'If settlements discharge: partnership obligation is temporal (post-settlement era moves toward weaker protection). If ongoing: partnership obligation is permanent and deepening (constrains Crown across generations). Case law trajectory suggests ongoing interpretation, but legislative attempts periodically foreclose it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_finality_vs_ongoing_partnership, empirical, 'Whether Treaty settlements discharge or establish ongoing partnership obligations').

omega_variable(
    parliamentary_sovereignty_vs_treaty_supremacy,
    'Can Parliament legislate to diminish or eliminate consultation obligations without Māori consent, or does the Treaty''s status as supreme law constrain Parliamentary power?',
    'Constitutional law analysis comparing New Zealand''s unwritten supremacy doctrine with written constitutions; examination of Charter of Rights jurisprudence; investigation of whether ''principles of the Treaty'' are now judicially entrenched or legislatively revisable',
    'If Parliament remains unconstrained: partnership_reading is structurally subordinate to crown_sovereignty_reading; partnership is revocable. If Treaty constrains Parliament: partnership_reading is entrenched; moves toward rangatiratanga_reading. This is the deepest structural question; current doctrine is in active flux.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_sovereignty_vs_treaty_supremacy, conceptual, 'Whether Parliamentary sovereignty is constrained by Treaty supremacy or Treaty obligations remain legislatively revisable').

omega_variable(
    reading_kernelization_history,
    'When did the partnership_reading emerge as a formalized interpretive approach? Was it a stable principle from 1840, or a constructed modern doctrine?',
    'Historiography of Treaty jurisprudence; tracing emergence of ''partnership'' language in case law (first major articulation: Waitangi Tribunal 1985, affirmed in case law 1989-1995); comparison with contemporary indigenous constitutional movements globally',
    'If stable from 1840: partnership_reading has lineage authority and deeper entrenchment. If constructed post-1985: partnership_reading is recent and potentially reversible; authority is thinner. Historical evidence: partnership_reading is a modern construction, particularly the ''principles'' doctrine. This affects interpretation_layer_present assessment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernelization_history, empirical, 'Historical emergence and authority trajectory of partnership reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__partnership_reading, 1975, 2005).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(waitangi_part_theater_1975, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(waitangi_part_theater_1990, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(waitangi_part_theater_2005, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(waitangi_part_extract_1975, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(waitangi_part_extract_1990, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(waitangi_part_extract_2005, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(waitangi_part_suppress_1975, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(waitangi_part_suppress_1990, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(waitangi_part_suppress_2005, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__partnership_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% The waitangi_sovereignty_allocation kernel has three constraint readings with different extractiveness and beneficiary/victim structures. partnership_reading (ε≈0.38) represents the middle position institutionalized in Tribunal and case law. crown_sovereignty_reading (ε≈0.15) represents Parliament's traditional position (low extraction because sovereignty is settled law). rangatiratanga_reading (ε≈0.68) represents Māori rights advocates' position (high extraction because Crown prevents authority recognition). The three constraints form a family; network edges show that partnership_reading influences both siblings by constraining the interpretive space (jurisprudence on partnership sets precedent for both). Each reading operates from its own reference frame and authority grounding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(waitangi_sovereignty_allocation__partnership_reading, institutional, 0.1).
constraint_indexing:directionality_override(waitangi_sovereignty_allocation__partnership_reading, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
