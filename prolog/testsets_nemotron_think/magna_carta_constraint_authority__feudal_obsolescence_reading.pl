% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__feudal_obsolescence_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Magna Carta as Feudal Compact with No Modern Binding Authority
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint story represents the feudal obsolescence reading of the
 *   Magna Carta constraint authority kernel. The reading asserts that Magna
 *   Carta was a 13th century baronial compact addressing specific feudal
 *   grievances (relief, wardship, marriage, scutage) and that its authority
 *   exhausted with the feudal system. It has no binding force on modern
 *   sovereignty structures — Parliament, the executive, or the courts. The
 *   reading is advanced by executive power theorists, administrative state
 *   architects, and certain originalist jurists to clear the field of
 *   charter-derived limits on state power. The constraint operates as a
 *   snare: the coordination story (historical contextualization) is cover for
 *   extraction (executive discretion maximized, popular constitutionalism and
 *   juridical restraint displaced).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.62).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, snare).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Magna Carta as Feudal Compact with No Modern Binding Authority").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__feudal_obsolescence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, 'aa2a3a57-a7ec-4709-92f2-6a023bed0097').
narrative_ontology:cs_kernel_codification('aa2a3a57-a7ec-4709-92f2-6a023bed0097', fixed_text).
narrative_ontology:cs_authority_grounding('aa2a3a57-a7ec-4709-92f2-6a023bed0097', lineage).
narrative_ontology:cs_interpretation_layer_present('aa2a3a57-a7ec-4709-92f2-6a023bed0097').
narrative_ontology:cs_reading_relation('aa2a3a57-a7ec-4709-92f2-6a023bed0097', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_reading_relation('aa2a3a57-a7ec-4709-92f2-6a023bed0097', magna_carta_constraint_authority__parliamentary_sovereignty_reading, influences).
narrative_ontology:cs_axiom('aa2a3a57-a7ec-4709-92f2-6a023bed0097', foundational, historical_context_exhausts_authority).
narrative_ontology:cs_axiom_status(historical_context_exhausts_authority, holdable).
narrative_ontology:cs_axiom_grounding('aa2a3a57-a7ec-4709-92f2-6a023bed0097', historical_context_exhausts_authority, conventional).
narrative_ontology:cs_axiom('aa2a3a57-a7ec-4709-92f2-6a023bed0097', secondary, executive_discretion_presumption).
narrative_ontology:cs_axiom_status(executive_discretion_presumption, holdable).
narrative_ontology:cs_axiom_grounding('aa2a3a57-a7ec-4709-92f2-6a023bed0097', executive_discretion_presumption, instrumental).
narrative_ontology:cs_reference_frame('aa2a3a57-a7ec-4709-92f2-6a023bed0097', feudal_compact_originalism).
narrative_ontology:cs_drift_state('aa2a3a57-a7ec-4709-92f2-6a023bed0097', contemporary_administrative_state, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('aa2a3a57-a7ec-4709-92f2-6a023bed0097', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_branch).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_power_scholars).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, administrative_state_architects).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, due_process_protections).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, rule_of_law_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, parliament_legislature).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, judiciary).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__feudal_obsolescence_reading, historical_originalism).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_discretion_presumption).
narrative_ontology:constraint_vindicates(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_supremacy_over_charter).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims Magna Carta is a historical feudal document with no binding force on modern governance. Uses this reading to maximize discretionary authority in security, emergency powers, and administrative rulemaking. Benefits from the constraint's extraction by facing fewer judicially enforceable limits derived from charter tradition.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_branch, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_branch, beneficiary).

% Academic and legal theorists who advance the feudal obsolescence reading to support broad executive authority. Gain professional standing and influence by providing the intellectual architecture for executive discretion. Their careers benefit from the constraint's dominance in elite legal discourse.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_power_scholars, beneficiary,
    organized, biographical, mobile, national).

% Bureaucratic and regulatory actors who build governance structures on the premise that charter-derived restraints are obsolete. The constraint enables them to design agencies and rulemaking processes without Magna Carta-inflected due process constraints. Exit would mean restructuring the administrative state.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, administrative_state_architects, beneficiary,
    institutional, generational, constrained, national).

% Movements and scholars who view Magna Carta as a living charter of popular rights against arbitrary power. Bear the cost when the feudal obsolescence reading is invoked to defeat popular constitutional claims — their mobilization energy is extracted as the constraint renders their historical anchor 'merely feudal.'
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism, payer,
    organized, generational, constrained, national).

% The judicial self-conception as a restraint on power through inherited charter principles. When the feudal obsolescence reading prevails, courts that would invoke Magna Carta for due process or habeas corpus are told the charter has no authority — their institutional identity as charter-guardians is eroded, and they cannot exit this role without ceasing to be the judiciary as traditionally conceived.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint, payer,
    institutional, civilizational, identity_locked, national).

% The substantive protections (habeas corpus, fair trial, protection from arbitrary seizure) that trace lineage to Magna Carta clauses 39 and 40. When the feudal obsolescence reading is authoritative, these protections lose their deepest historical anchor and become purely statutory — revocable by the same legislature that creates them. The individuals who rely on them have no exit from the legal system.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, due_process_protections, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_non_agent(magna_carta_constraint_authority__feudal_obsolescence_reading, due_process_protections).

% Civil society actors, international observers, and domestic reformers who invoke Magna Carta as a symbol and substantive source of lawful restraint. Their advocacy is weakened when the feudal obsolescence reading is treated as settled law — they lose a rhetorical and legal resource. Exit means abandoning the rule-of-law framework entirely.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, rule_of_law_advocates, payer,
    moderate, biographical, constrained, global).

% Courts that must decide whether Magna Carta has continuing authority. When they adopt the feudal obsolescence reading, they surrender a historic source of legitimacy for judicial review; when they resist, they face accusations of judicial activism. Their institutional identity is fused to the charter's fate — they cannot exit the question without redefining their role.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, judiciary, payer,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__feudal_obsolescence_reading, judiciary, agenda_setter).

% The legislative body that inherits parliamentary sovereignty reading's claim. Benefits from feudal obsolescence reading because it clears the field of charter-based challenges to statutes — Parliament becomes the sole source of rights. But also sets the agenda by deciding whether to enact statutory equivalents of charter protections.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, parliament_legislature, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_constraint_authority__feudal_obsolescence_reading, parliament_legislature, beneficiary).

% Produces the historical, doctrinal, and theoretical work that sustains or challenges the feudal obsolescence reading. Does not directly collect rents or bear costs from the constraint's operation, but shapes the discourse that determines which reading prevails in courts and legislatures.
narrative_ontology:constraint_stakeholder(magna_carta_constraint_authority__feudal_obsolescence_reading, legal_academy, observer,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clean historical break: by declaring Magna Carta a spent feudal compact, the reading coordinates legal discourse around the premise that modern constitutional authority derives solely from subsequent enactments (statutes, constitutions, judicial doctrines) rather than an 1215 charter. This simplifies the legal field by removing a contested ancient source.
% TRANSFER_FUNCTION: Transfers interpretive authority and restraint-power from charter-derived principles (popular constitutionalism, juridical restraint, due process) to the executive branch and legislative supremacy. The extraction is the discretionary space the executive gains when courts cannot invoke Magna Carta as a binding limit.
% ABSENT_VOICES: The 13th century barons and free men whose grievances produced the charter — they cannot object to the reading that their document is obsolete. Also absent: colonial and post-colonial subjects in former British territories where Magna Carta was invoked as a rights charter against empire; their historical experience of the charter as a living restraint is excluded from the feudal obsolescence frame.
% DISAPPEARANCE_RATIONALE: If the feudal obsolescence reading vanished overnight, courts would regain Magna Carta as an available authority for due process and habeas corpus claims; executive branch would face renewed charter-based challenges to discretionary powers; popular constitutionalism movements would recover a historic anchor; the administrative state would confront a thicker web of judicially enforceable charter-derived limits. The legal field would rearrange around a re-activated ancient constraint.
% FOUNDING_PROBLEM: The reading was built to solve the problem of an ancient charter being invoked against modern state power — specifically, 17th-19th century struggles where Magna Carta was used to challenge royal prerogative, then parliamentary reform, then colonial administration. The feudal obsolescence frame neutralized the charter by historicizing it: it became a museum piece, not a weapon.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (e.g., J.C. Holt, Nicholas Vincent) corroborate that Magna Carta's 13th century context was feudal and that its survival as a binding instrument required repeated reissue and reinterpretation. But constitutional scholars (e.g., A.V. Dicey on the 'law of the constitution,' modern common law constitutionalists) attest that the charter's principles were successfully reinvented as binding restraints — the founding problem of 'charter vs. state power' is not dead, it recurs in each era. No non-beneficiary source treats the obsolescence as settled.
narrative_ontology:disappearance_verdict(magna_carta_constraint_authority__feudal_obsolescence_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_constraint_authority__feudal_obsolescence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__feudal_obsolescence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is moderate-high because the reading transfers substantial discretionary power to the executive by removing a historic source of judicially enforceable restraint. Suppression (0.62) is significant because maintaining the reading requires active judicial doctrine (refusing Magna Carta arguments), academic enforcement (marginalizing living constitutionalist scholarship), and legislative acquiescence. Theater ratio (0.55) is high because ceremonial invocations of Magna Carta (anniversaries, monuments, rhetorical nods) persist while the reading denies it operative force — the charter becomes a performed symbol rather than a legal rule. Accessibility collapse (0.45) is moderate: alternatives (living constitutionalism, parliamentary sovereignty) remain intellectually available but are suppressed in official doctrine. Resistance (0.58) reflects ongoing contestation from common law constitutionalists, habeas corpus advocates, and popular movements.
 *
 * PERSPECTIVAL GAP:
 *   From the executive/agenda-setter seat, the constraint looks like legitimate historical clarification — a coordination function that removes an anachronism. From the payer seats (juridical restraint, popular constitutionalism), the same structure operates as extraction: their inherited tools of restraint are confiscated by a reading that presents itself as neutral history. The engine computes this divergence from the declared beneficiary/victim structure and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch, executive power scholars, and administrative state architects are structural beneficiaries — they collect the discretionary space the constraint creates. Their exit options range from arbitrage (executive can switch readings instrumentally) to constrained (administrative architects embedded in the structure). Popular constitutionalism, juridical restraint, due process protections, and rule of law advocates are payers — they bear the cost of lost anchor, lost legitimacy, lost protections. The judiciary is dually positioned: payer (loses charter authority) and agenda_setter (decides the reading's fate). Parliament is agenda_setter/beneficiary — it gains legislative supremacy but loses a historic check. The legal academy is an analytical observer.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (if any) was to resolve 13th century feudal grievances — that problem is dead. But the reading persists not as a scaffold (no sunset clause) but as a snare: the obsolescence claim is actively maintained to enable modern executive extraction. The mandatrophy is unresolved — the reading claims the charter's function is dead while using that claim to extract discretionary power. This is not a piton (theatrical maintenance of an atrophied coordination) because the extraction is active and consequential, not inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_authority_exhaustion,
    'Does the feudal origin of Magna Carta logically exhaust its authority, or can a document''s authority transcend its historical context through re-interpretation and re-enactment?',
    'Comparative study of other ancient charters (e.g., English Bill of Rights 1689, US Constitution 1787) — do their origins limit their modern authority? Historical analysis of Magna Carta''s reissues and statutory confirmations (1216, 1217, 1225, 1297) — each reissue was a new enactment, not mere feudal survival.',
    'If authority can transcend origin, the feudal obsolescence reading''s foundational axiom fails and the constraint loses its coordinating premise. If origin exhausts authority, the reading''s claim is structurally sound but its extraction from juridical restraint remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_authority_exhaustion, conceptual, 'Whether historical origin deterministically fixes a legal document''s modern authority.').

omega_variable(
    executive_discretion_vs_charter_restraint,
    'How much executive discretion is actually gained when courts reject Magna Carta arguments, versus discretion that would exist anyway under statutory and constitutional frameworks?',
    'Empirical study of cases where Magna Carta was invoked vs. rejected in UK, Commonwealth, and US courts; measure the delta in executive outcomes. Counterfactual modeling: what restraints would exist from statute and common law alone?',
    'If the discretion delta is small, the constraint''s extractiveness is overstated — the reading is more theater than substance. If large, the snare classification is confirmed: the reading actively enables extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(executive_discretion_vs_charter_restraint, empirical, 'The measurable executive power gain from the feudal obsolescence reading''s doctrinal dominance.').

omega_variable(
    committer_framing_ambiguity,
    'Is this reading a genuine historical claim or a strategic frame deployed to enable executive power? The kernel structure means the same text generates different constraints — is the feudal obsolescence reading held because it is historically compelled, or because it produces a preferred distribution of authority?',
    'Genealogical analysis of the reading''s advocates: do they adopt it before or after committing to broad executive authority? Citation network analysis: does the reading migrate across ideological camps, or track with executive power positions?',
    'If strategic, the constraint is a snare by design — the historical claim is cover. If genuine, it is a contested historical claim that happens to enable extraction — still a snare in operation, but with different legitimacy implications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_ambiguity, preference, 'Whether the reading''s adoption is driven by historical conviction or authority-distribution preference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 1600, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magna_carta_feudal_obsolescence_tr_t1600, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_tr_t1700, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1700, 0.15).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_tr_t1789, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1789, 0.25).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_tr_t1850, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1850, 0.35).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_tr_t1900, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1900, 0.42).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_tr_t1950, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1950, 0.48).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_tr_t2000, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2000, 0.52).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_tr_t2024, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2024, 0.55).

% Extraction over time
narrative_ontology:measurement(magna_carta_feudal_obsolescence_be_t1600, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1600, 0.15).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_be_t1700, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1700, 0.25).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_be_t1789, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1789, 0.35).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_be_t1850, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1850, 0.42).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_be_t1900, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1900, 0.52).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_be_t1950, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1950, 0.58).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_be_t2000, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_be_t2024, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(magna_carta_feudal_obsolescence_su_t1600, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1600, 0.2).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_su_t1700, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1700, 0.3).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_su_t1789, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1789, 0.4).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_su_t1850, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1850, 0.48).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_su_t1900, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1900, 0.52).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_su_t1950, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_su_t2000, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(magna_carta_feudal_obsolescence_su_t2024, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__feudal_obsolescence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, habeas_corpus_modern_scope).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, due_process_constitutional_anchor).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_emergency_powers_constraint).

% DUAL FORMULATION NOTE:
% This reading decomposes the natural-language concept 'Magna Carta authority' from its siblings by fixing the referent: the charter as a 1215 feudal compact whose authority terminated with the feudal system. The living constitutionalism reading fixes a different referent: the charter as a living precedent continuously reinterpreted. The parliamentary sovereignty reading fixes a third: the charter as a statutory resource absorbable by Parliament. Each has distinct ε, beneficiaries, and victims — the ε-invariance principle requires separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_constraint_authority__feudal_obsolescence_reading, institutional, 0.15).
constraint_indexing:directionality_override(magna_carta_constraint_authority__feudal_obsolescence_reading, organized, 0.25).
constraint_indexing:directionality_override(magna_carta_constraint_authority__feudal_obsolescence_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
