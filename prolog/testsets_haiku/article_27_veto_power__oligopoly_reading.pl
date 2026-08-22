% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__oligopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__oligopoly_reading, []).

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
 *   constraint_id: article_27_veto_power__oligopoly_reading
 *   human_readable: P5 Veto Power as Oligopoly Entrenchment (Oligopoly Reading)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   Article 27 of the UN Charter grants each of the five permanent Security
 *   Council members (US, Russia, China, UK, France) the power to veto any
 *   substantive Council resolution. This reading interprets the veto not as a
 *   coordination mechanism for great-power war prevention, but as a
 *   structural entrenchment of geopolitical oligopoly. The veto uses Charter
 *   immutability (amendment requires P9 approval, each P5 can block) to
 *   extract ongoing authority rents and block institutional evolution that
 *   would redistribute power to aspiring great powers and the non-P5
 *   majority. The founding problem—preventing great-power war through
 *   consensus—has been dead for decades, yet the veto persists, now
 *   functioning primarily to block reform and protect P5 interests unrelated
 *   to war prevention. The constraint is CLAIMED as Snare (pure extraction
 *   with suppressed alternatives) while the authored metrics trace its
 *   operation across 80 years of geopolitical change. This is ONE reading of
 *   a contested kernel; the sibling readings (coordination_reading,
 *   sovereignty_reading) instantiate different constraints with different ε
 *   values and beneficiary structures.
 *
 * KEY AGENTS:
 *   - Permanent five members: institutional power, civilizational time horizon, trapped exit — hold the veto and set the operational agenda; unilateral exit would mean losing permanent seat and geopolitical authority. Benefit from perpetual exemption from enforcement and ability to block reform.
 *   - Non-P5 UN majority (188 states): organized to powerful power distribution, generational horizon, constrained exit — cannot veto, cannot reform the system, cannot exit UN without forfeiting global voice. Pay through institutional paralysis and selective enforcement.
 *   - Aspiring great powers (India, Brazil, Japan, Germany): powerful power, generational horizon, constrained exit — seek Council seats proportional to contemporary power but cannot force expansion because P5 collectively block it.
 *   - Small and developing states: powerless, generational horizon, trapped exit — subject to veto-based enforcement but cannot block P5 actions; cannot exit without losing all leverage.
 *   - Transnational atrocity victims: powerless, immediate horizon, trapped locally — populations in non-strategic regions suffering genocidal violence, subject to veto by disinterested P5 members.
 *   - P5-aligned client states: powerful, biographical horizon, mobile exit — benefit from P5 veto protection of their interests.
 *   - International law reformers: analytical seat, generational horizon, analytical exit — document institutional failure and propose alternatives; have no formal power but generate knowledge revealing the veto's extractive character.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, 0.82).
domain_priors:suppression_score(article_27_veto_power__oligopoly_reading, 0.79).
domain_priors:theater_ratio(article_27_veto_power__oligopoly_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__oligopoly_reading, snare).
narrative_ontology:human_readable(article_27_veto_power__oligopoly_reading, "P5 Veto Power as Oligopoly Entrenchment (Oligopoly Reading)").
narrative_ontology:topic_domain(article_27_veto_power__oligopoly_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:requires_active_enforcement(article_27_veto_power__oligopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__oligopoly_reading, 'f656c0b3-1373-40f1-871f-7e10effbe1ce').
narrative_ontology:cs_kernel_codification('f656c0b3-1373-40f1-871f-7e10effbe1ce', fixed_text).
narrative_ontology:cs_authority_grounding('f656c0b3-1373-40f1-871f-7e10effbe1ce', extraction).
narrative_ontology:cs_interpretation_layer_present('f656c0b3-1373-40f1-871f-7e10effbe1ce').
narrative_ontology:cs_reading_relation('f656c0b3-1373-40f1-871f-7e10effbe1ce', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('f656c0b3-1373-40f1-871f-7e10effbe1ce', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('f656c0b3-1373-40f1-871f-7e10effbe1ce', foundational, veto_as_oligopoly_entrenchment).
narrative_ontology:cs_axiom_status(veto_as_oligopoly_entrenchment, holdable).
narrative_ontology:cs_axiom_grounding('f656c0b3-1373-40f1-871f-7e10effbe1ce', veto_as_oligopoly_entrenchment, empirically_contingent).
narrative_ontology:cs_axiom('f656c0b3-1373-40f1-871f-7e10effbe1ce', foundational, institutional_power_locking_supersedes_coordination).
narrative_ontology:cs_axiom_status(institutional_power_locking_supersedes_coordination, holdable).
narrative_ontology:cs_axiom_grounding('f656c0b3-1373-40f1-871f-7e10effbe1ce', institutional_power_locking_supersedes_coordination, empirically_contingent).
narrative_ontology:cs_reference_frame('f656c0b3-1373-40f1-871f-7e10effbe1ce', p5_institutional_oligopoly_maintenance).
narrative_ontology:cs_drift_state('f656c0b3-1373-40f1-871f-7e10effbe1ce', contemporary_post_decolonization_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f656c0b3-1373-40f1-871f-7e10effbe1ce', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__oligopoly_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, permanent_five_members).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, non_p5_un_membership).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, aspiring_great_powers).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, majority_of_humanity_through_non_p5_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, aspiring_great_powers).
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, p5_aligned_client_states).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, small_and_developing_states).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, transnational_atrocity_victims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five permanent members (US, Russia, China, UK, France) set the operational agenda of the Security Council through their collective control of the veto. Each member can unilaterally block any substantive resolution. They directly collect the rents of the veto system: exemption from enforcement action, ability to condition Council action on their interests, and immunity from institutional reform. They maintain the system by blocking all amendment proposals (which require P9 approval, each P5 having veto power). Exit from the system is not available without catastrophic loss of institutional authority and geopolitical standing.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, permanent_five_members, agenda_setter,
    institutional, civilizational, trapped, global).

% The 188 non-permanent UN members comprise the vast majority of UN membership and represent the majority of humanity by population and land area. They participate in rotating temporary Security Council seats (ten seats, two-year terms) with no veto power. They can be blocked unilaterally by any P5 member from any enforcement action, peacekeeping deployment, or institutional reform. They bear the costs of institutional immobility: unresolved conflicts, selective enforcement based on P5 interests rather than need, atrocities in non-strategic regions left unaddressed, and inability to upgrade the Council to reflect contemporary geopolitical conditions. Exit means leaving the UN entirely, forfeiting the only global forum for collective action.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, non_p5_un_membership, payer,
    organized, generational, constrained, global).

% States like India, Brazil, Japan, Germany have become regional or near-global powers since 1945, with economic and military capacity comparable to or exceeding some P5 members. They seek permanent or weighted Council seats proportional to their contemporary geopolitical weight. The P5 veto blocks expansion of the permanent category—any reform requires P9 approval and P5 consensus, which the existing P5 members collectively reject. These aspiring powers pay through institutional irrelevance (their voice carries no veto weight), reduced security guarantees, and inability to translate economic power into governance authority. They benefit marginally from being inside the UN system (rather than outside), but are locked out of the institutional power structure. Exit means leaving the UN and losing all leverage in global governance.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, aspiring_great_powers, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, aspiring_great_powers, beneficiary).

% The vast majority of UN member states are small and developing, with limited military or economic capacity. They have formal equality in the General Assembly but no enforcement mechanism there. In the Security Council, they rotate through temporary seats with no real influence or veto power. They are subject to Council enforcement actions (sanctions, military intervention) that can be vetoed to protect powerful states' allies but cannot veto actions against themselves. They cannot reform the system, cannot override P5 protection of powerful states, and cannot exit without forfeiting their only global forum.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, small_and_developing_states, payer,
    powerless, generational, trapped, global).

% Allies of P5 members (NATO members for US/UK/France, Russia-allied states, China-aligned states, regional powers backed by P5 sponsors) benefit directly from veto protection. When enforcement action is proposed against them or their interests, their P5 patron can and does veto. They receive exemption from accountability that smaller or unaligned states do not enjoy. Their alignment is contingent on ongoing benefit; they maintain patron relationships as long as the patron's veto provides protection.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, p5_aligned_client_states, beneficiary,
    powerful, biographical, mobile, global).

% Populations in non-strategic regions experiencing mass atrocity, genocide, ethnic cleansing, or humanitarian catastrophe. When their state is unaligned with P5 powers or strategically insignificant, Security Council military intervention or enforcement can be vetoed by any P5 member. Their only recourse is the General Assembly (which has no enforcement power) or regional bodies (which are often under-resourced and lack international legitimacy). They cannot exit; they are trapped in geographic zones abandoned by the international institutional system.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, transnational_atrocity_victims, payer,
    powerless, immediate, trapped, local).

% Scholars, practitioners of international law, human rights advocates, and civil society actors who analyze the Council's structural failures and propose institutional reforms. They do not hold formal power in the UN system but generate the comparative institutional knowledge that reveals the veto's extractive character relative to other possible governance structures. Their seat is analytical; they document institutional failure modes and produce alternative designs.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, international_law_reformers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__oligopoly_reading, permanent_five_members).
narrative_ontology:fixing_cost_class(article_27_veto_power__oligopoly_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In the oligopoly reading: NONE currently operative. The veto was originally presented as coordinating mechanism to prevent great-power military conflict by ensuring no P5 member could be compelled into war. This reading contests that function is now active—it has been decoupled from the veto's actual operation. The veto now paralyzes the Council when any P5 member's interests diverge from the majority position, blocking rather than enabling coordination.
% TRANSFER_FUNCTION: Moves geopolitical authority and exemption from enforcement from the non-P5 majority and aspiring great powers to the P5 permanent members and their client states. Specifically: (1) Each P5 member holds unilateral veto over all enforcement actions, peacekeeping mandates, and institutional reform; (2) This exemption protects P5 interests and those of P5-aligned states even when Council majority and global consensus diverge; (3) The P5 collectively block institutional evolution that would redistribute Council authority proportional to contemporary geopolitical power; (4) Non-P5 states, aspiring powers, and affected populations pay through institutional paralysis, selective enforcement based on P5 interests rather than need or justice, and locked-out institutional voice.
% ABSENT_VOICES: The non-P5 majority, aspiring great powers (India, Brazil, Japan, Germany), and populations suffering atrocities in non-strategic regions would object loudly if present in decision-making about the veto. They are excluded by the Charter structure itself—veto power is granted only to the original five. International law reformers and human rights advocates would also carry positions diverging from P5 interests and would be systematically over-ruled by veto. Regional powers (like Japan, Germany, India, Brazil) have proposed Council expansion and veto limitation but are blocked by P5 votes.
% DISAPPEARANCE_RATIONALE: If the veto power disappeared overnight, the Security Council would become a genuinely majoritarian institution reflecting actual global consensus rather than P5 interests. Council votes would require simple majority or supermajority rather than P5 consensus. Enforcement actions against non-compliant states (including current or former P5 members) would become possible. Institutional reform (Council expansion, weighted voting, veto limitation) would become negotiable. The geopolitical order would reorganize: P5 immunity would evaporate, great-power relations would shift from unilateral veto-play to coalition-building for majority support, institutional authority would redistribute toward larger economies and populations. The world system does not depend on the veto existing; it depends on P5 military and economic power existing. The veto entrenches that power by blocking institutional updating.
% FOUNDING_PROBLEM: At the UN's founding (1945), nuclear weapons were nascent or absent for most states; the Allied victors (US, USSR, UK, France) had unique military-industrial capacity; decolonization had not yet occurred; and the founding consensus was that great-power agreement was necessary to prevent great-power military conflict. The veto was justified as a safeguard ensuring that no permanent member could be compelled into military confrontation against its will, thereby preventing Nuclear Age interstate war through institutional consensus.
% FOUNDING_PROBLEM_CORROBORATION: International relations scholars, institutional reform advocates, non-P5 UN members, and even P5 members themselves (in contemporary discourse) attest that the founding problem has been decoupled from the veto's current operation. No great-power war has occurred in the nuclear era—a fact that P5 defenders attribute to deterrence, not to the UN veto. The veto now functions primarily to block institutional change and to protect P5 interests in non-strategic conflicts, not to prevent great-power war. P5 members no longer invoke great-power war prevention as the justification for maintaining the veto; they instead assert geopolitical power and institutional permanence justify it. The founding problem's shift from live to dead is corroborated by eighty years of institutional failure documentation, UN reform debates (which date to the 1960s and intensify every decade), and the empirical record: veto use clusters around blocking humanitarian enforcement in non-strategic regions and blocking institutional reform, not around preventing great-power military confrontation.
narrative_ontology:disappearance_verdict(article_27_veto_power__oligopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__oligopoly_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__oligopoly_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_27_veto_power__oligopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__oligopoly_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__oligopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_27_veto_power__oligopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.82 (t=80) because the veto structure transfers geopolitical authority from the 193-member majority to a 5-member club, with that club using Charter immutability to block reform. The transfer mechanism is stark: any P5 member can unilaterally block enforcement against its allies, peacekeeping in regions it deems non-strategic, and institutional expansion that would dilute its authority. The measurement series shows extraction rising from 0.48 (1945, shortly after founding when great-power consensus was more aligned) to 0.82 (2025, when geopolitical power has redistributed but institutional authority remains locked). Suppression is high (0.79) because the veto blocks all paths to reform: Charter amendment requires P9 approval (each P5 can block); Council expansion requires P5 agreement; weighted voting requires P5 agreement. The non-P5 majority has no unilateral path to change the system. Theater ratio is moderate (0.41) and rising: the veto is initially justified as preventing great-power war, but as the founding problem dies (no great-power war outbreak in the nuclear era), the veto increasingly functions as purely extractive rent-maintenance and reform-blocking. The theater tracks the divergence between stated function and actual operation.
 *
 * PERSPECTIVAL GAP:
 *   The P5 seat and the non-P5 majority seat compute completely different types. From the P5 perspective, the constraint is best described as power-maintenance (or in the coordination reading, as war-prevention mechanism)—it is protective and stabilizing. From the non-P5 majority perspective, the constraint is pure extraction: it locks them out of institutional power, blocks reform, and permits P5 allies to act with impunity. The engine will compute per-seat classification reflecting this asymmetry. P5 seats likely compute as beneficiary/institutional (or coordinate if the coordination reading dominates). Non-P5 seats compute as snare victims. Aspiring powers compute as targets of power-locking. The mandatrophy analysis hinges on this gap: the founding problem (great-power consensus for war prevention) is dead by non-P5 testimony, but the constraint persists for pure authority-locking; the mismatch is the signature of a zombie institution extracting rent while its original justification has evaporated.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality diverges sharply between P5 members and non-P5 majority. For P5 members: d approaches 0.0 (full beneficiaries). They hold unilateral veto, can block any enforcement against themselves or their clients, and crucially, they can block all institutional reform that would dilute their authority. Their exit is trapped (leaving the UN means losing permanent seat and great-power institutional status), but the constraint benefits them enormously, so trapped exit is irrelevant to their directionality—they are structural beneficiaries of the status quo. For non-P5 states: d approaches 1.0 (full targets). They pay through inability to enforce against P5-aligned states, paralyzed Council action in non-strategic crises, and blocked institutional reform despite changed geopolitical conditions. Their exit is constrained (formally free to leave UN but forfeiting all voice in global governance), and the constraint extracts from them continuously. For aspiring great powers (India, Brazil, etc.): d is high (0.7-0.8) because the veto blocks their institutional ascension despite their contemporary geopolitical power; they are targets of the constraint's power-locking function. For atrocity victims in non-strategic regions: d approaches 1.0 (full targets, identity-locked by geography, powerless). The P5 veto permits atrocity because the perpetrating state has no P5 patron.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading diagnoses mandatrophy at the structural level. The founding problem was concrete: the atomic age made great-power military conflict catastrophic, and the UN's founding logic was to lock great-power consent into the enforcement mechanism so that none could be compelled into war. That problem is DEAD by the testimony of international relations scholars, institutional reformers, and even P5 members (none of whom now invoke great-power war prevention as the veto's justification). Yet the veto persists, now serving purely as a mechanism for P5 authority-locking and reform-blocking. The constraint exhibits classical mandatrophy symptoms: (1) The founding justification is no longer invoked; (2) The contemporary function is decoupled from the original function; (3) The beneficiary set has shifted from 'security-conscious great powers' to 'P5 institutional oligopoly'; (4) The P5 actively block institutional evolution that would update the system to reflect contemporary geopolitical realities. The mandatrophy is not latent—it is actively defended. The P5 collectively block every reform proposal. Reform is suppressed not by institutional inertia but by active enforcement of immutability. This marks the constraint as extractive snare, not degraded rope or coordinating mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_death_attestation,
    'Has the founding problem (preventing great-power military conflict through institutional consensus) genuinely died, or does it remain live and unrecognized?',
    'Historical analysis of great-power relations and Security Council voting patterns: if great-power military conflict prevention were still the active function, veto use should cluster around preventing interstate wars between P5 members or preventing enforcement against P5-allied states. If veto use clusters instead around blocking humanitarian enforcement in non-strategic regions and blocking institutional reform, the founding problem is empirically dead.',
    'If the founding problem is dead, the constraint''s classification shifts decisively toward extractive snare (pure rent-extraction maintaining a dead mandate). If the founding problem is still live but unrecognized, the constraint retains a coordination function component and should be classified as tangled rope (coordination + extraction). The mandatrophy diagnosis depends entirely on this omega''s resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_death_attestation, empirical, 'Whether the veto''s original justification (great-power war prevention) remains the actual operative function.').

omega_variable(
    charter_immutability_as_enforcement,
    'Is the Charter''s amendment requirement (P9 approval, each P5 can block) a structural feature of the veto''s persistence, or are other institutional factors more important?',
    'Counterfactual institutional design: if the amendment requirement were lowered (e.g., requiring 7 P5 approval instead of 9, or supermajority instead of consensus), would reform proposals advance? If P5 members would still block reform using alternative mechanisms (control over implementation, counter-reform, withdrawal threats), then immutability is reinforcing but not sole cause. If reform proposals would advance immediately, immutability is the active enforcement mechanism.',
    'If immutability is the sole enforcement mechanism, the constraint''s extractiveness depends on maintaining the super-majority gate; if other mechanisms (power asymmetry, threat coordination) would suffice, the constraint is more resilient to institutional change. The high suppression measure (0.79) rests partly on the assumption that the amendment gate is the primary suppression mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(charter_immutability_as_enforcement, empirical, 'Whether Charter immutability is the primary mechanism enforcing the veto''s persistence or whether institutional power dynamics would sustain it regardless.').

omega_variable(
    aspiring_power_coalition_possibility,
    'If aspiring great powers (India, Brazil, Japan, Germany) formed a unified coalition demanding Council reform, could they overcome P5 blocking, or is the P5 block genuinely unbreakable within the current institutional structure?',
    'Political economy of great-power coordination: can aspiring powers offer side-payments (technology, market access, alliance shifts) to induce P5 defection on reform? Do P5 interests diverge on Council expansion (some might benefit from having more powerful partners to balance others)? Empirical tracking of reform negotiation outcomes would reveal whether the blocking is structural or contingent on P5 unity.',
    'If P5 unity on blocking reform is contingent and could be broken with sufficient coalition pressure, the accessibility_collapse measure (0.71) overstates the true suppression—alternatives exist if organized. If P5 unity is structural and unbreakable (each P5 faces incentives to preserve its veto regardless of other issues), the suppression is genuine. This affects the resistance measure (0.68) and the degree of identity-locking for aspiring powers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(aspiring_power_coalition_possibility, empirical, 'Whether P5 unity on institutional reform-blocking is fragile or structurally robust.').

omega_variable(
    oligopoly_vs_sovereignty_reading_distinction,
    'Is the veto best understood as rent-extraction by an institutional oligopoly (this reading), or as codification of Westphalian sovereignty for states with unilateral global-reach military capacity?',
    'Examine the justifications P5 members advance for maintaining the veto in contemporary discourse. If they invoke oligopoly language (protecting P5 institutional authority, blocking dilution of their power), the oligopoly reading is corroborated. If they invoke sovereignty language (no state can be bound without consent, especially great powers with enforcement capacity), the sovereignty reading is supported. If they invoke coordination language (preventing great-power war), the coordination reading is supported.',
    'If the sovereignty reading is more accurate, the beneficiary structure shifts: beneficiaries would be ''states with global enforcement capacity'' rather than ''P5 institutional members,'' and the victim set would be ''states without enforcement capacity.'' The classification might shift from snare (which assumes institutional entrenchment) to something closer to tangled rope or even rope (if the sovereignty principle is genuine and not a cover for oligopoly). This omega routes through reading_relations and axioms in the cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oligopoly_vs_sovereignty_reading_distinction, conceptual, 'Whether the veto is best understood as oligopoly rent-extraction or as Westphalian sovereignty principle applied to nuclear-armed states.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of reform primarily structural (external barriers like the amendment requirement and P5 voting power) or internalized (non-P5 states have come to accept P5 authority as legitimate)?',
    'Survey data on non-P5 state attitudes toward Council reform: do non-P5 members actively propose reform or have they internalized the idea that reform is impossible? Longitudinal analysis of General Assembly proposals: are reform proposals declining over time (sign of internalized suppression) or steady/rising (sign of structural suppression)?',
    'If suppression is internalized, the constraint''s persistence depends less on active enforcement and more on cognitive capture—the non-P5 majority has accepted P5 authority as natural. This would support a piton classification (maintained by inertia rather than active enforcement). If suppression is structural, the constraint is actively enforced and the snare classification is appropriate. The measurement series tracks theater_ratio as a proxy for this: rising theater_ratio suggests internalization and theatrical maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of institutional reform is maintained by external structural barriers or by internalized acceptance of P5 authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__oligopoly_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__oligopoly_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t10, article_27_veto_power__oligopoly_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(arti_tr_t10, observed).
narrative_ontology:measurement(arti_tr_t20, article_27_veto_power__oligopoly_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(arti_tr_t20, observed).
narrative_ontology:measurement(arti_tr_t30, article_27_veto_power__oligopoly_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(arti_tr_t30, observed).
narrative_ontology:measurement(arti_tr_t40, article_27_veto_power__oligopoly_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement_basis(arti_tr_t40, observed).
narrative_ontology:measurement(arti_tr_t50, article_27_veto_power__oligopoly_reading, theater_ratio, 50, 0.37).
narrative_ontology:measurement_basis(arti_tr_t50, observed).
narrative_ontology:measurement(arti_tr_t60, article_27_veto_power__oligopoly_reading, theater_ratio, 60, 0.39).
narrative_ontology:measurement_basis(arti_tr_t60, observed).
narrative_ontology:measurement(arti_tr_t70, article_27_veto_power__oligopoly_reading, theater_ratio, 70, 0.4).
narrative_ontology:measurement_basis(arti_tr_t70, observed).
narrative_ontology:measurement(arti_tr_t80, article_27_veto_power__oligopoly_reading, theater_ratio, 80, 0.41).
narrative_ontology:measurement_basis(arti_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__oligopoly_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t10, article_27_veto_power__oligopoly_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(arti_be_t10, observed).
narrative_ontology:measurement(arti_be_t20, article_27_veto_power__oligopoly_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(arti_be_t20, observed).
narrative_ontology:measurement(arti_be_t30, article_27_veto_power__oligopoly_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(arti_be_t30, observed).
narrative_ontology:measurement(arti_be_t40, article_27_veto_power__oligopoly_reading, base_extractiveness, 40, 0.74).
narrative_ontology:measurement_basis(arti_be_t40, observed).
narrative_ontology:measurement(arti_be_t50, article_27_veto_power__oligopoly_reading, base_extractiveness, 50, 0.78).
narrative_ontology:measurement_basis(arti_be_t50, observed).
narrative_ontology:measurement(arti_be_t60, article_27_veto_power__oligopoly_reading, base_extractiveness, 60, 0.8).
narrative_ontology:measurement_basis(arti_be_t60, observed).
narrative_ontology:measurement(arti_be_t70, article_27_veto_power__oligopoly_reading, base_extractiveness, 70, 0.81).
narrative_ontology:measurement_basis(arti_be_t70, observed).
narrative_ontology:measurement(arti_be_t80, article_27_veto_power__oligopoly_reading, base_extractiveness, 80, 0.82).
narrative_ontology:measurement_basis(arti_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_27_veto_power__oligopoly_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t10, article_27_veto_power__oligopoly_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(arti_su_t10, observed).
narrative_ontology:measurement(arti_su_t20, article_27_veto_power__oligopoly_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(arti_su_t20, observed).
narrative_ontology:measurement(arti_su_t30, article_27_veto_power__oligopoly_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement_basis(arti_su_t30, observed).
narrative_ontology:measurement(arti_su_t40, article_27_veto_power__oligopoly_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement_basis(arti_su_t40, observed).
narrative_ontology:measurement(arti_su_t50, article_27_veto_power__oligopoly_reading, suppression_requirement, 50, 0.77).
narrative_ontology:measurement_basis(arti_su_t50, observed).
narrative_ontology:measurement(arti_su_t60, article_27_veto_power__oligopoly_reading, suppression_requirement, 60, 0.78).
narrative_ontology:measurement_basis(arti_su_t60, observed).
narrative_ontology:measurement(arti_su_t70, article_27_veto_power__oligopoly_reading, suppression_requirement, 70, 0.79).
narrative_ontology:measurement_basis(arti_su_t70, observed).
narrative_ontology:measurement(arti_su_t80, article_27_veto_power__oligopoly_reading, suppression_requirement, 80, 0.79).
narrative_ontology:measurement_basis(arti_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__oligopoly_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_27_veto_power__oligopoly_reading, 0.08).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel: article_27_veto_power. The oligopoly_reading treats the veto as structural entrenchment of geopolitical oligopoly and authority rent-extraction (Snare, ε=0.82). The coordination_reading treats it as a mechanism for great-power consensus to prevent military conflict (Rope or Tangled Rope, lower ε). The sovereignty_reading treats it as codification of Westphalian sovereignty for states with global enforcement capacity (Rope or Tangled Rope, intermediate ε). Each reading instantiates a different constraint with a different beneficiary/victim structure and different classification. They share the same kernel (Article 27 Charter text) but diverge in their diagnosis of what the veto structurally does and whose interests it serves. All three readings are live positions held by different institutional actors; no single framework can hold all three simultaneously. The network links all three as structurally interdependent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_27_veto_power__oligopoly_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
