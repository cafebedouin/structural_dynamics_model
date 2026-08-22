% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__cohabitation_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__cohabitation_equilibrium_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: fifth_republic_constitution__cohabitation_equilibrium_reading
 *   human_readable: Fifth Republic Cohabitation Equilibrium: Dual Executive Authority Allocation
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The Fifth Republic's constitutional architecture allocates executive
 *   authority between a directly-elected President and an
 *   Assembly-accountable Prime Minister. This reading construes the
 *   arrangement as a cohabitation equilibrium: a deliberate tension requiring
 *   continuous negotiation between the two executives when they represent
 *   opposing political forces. Both executives retain real veto power;
 *   neither can unilaterally dominate policy. The constraint coordinates
 *   checks-and-balances but extracts from policy coherence and electoral
 *   clarity — voters cannot hold a single accountable executive, and domestic
 *   policy implementation slows under negotiation. This reading treats the
 *   equilibrium as unstable and moderately extractive; it coexists with
 *   sibling readings that see either the President as dominant
 *   (hyper-presidential reading) or the Assembly as supreme
 *   (parliamentary-constraint reading).
 *
 * KEY AGENTS:
 *   - President: claims popular mandate and foreign policy sovereignty; leverage via Assembly dissolution and emergency powers; constrained by budgetary dependence on Assembly-controlled Prime Minister
 *   - Prime Minister: structurally positioned as mediator between President and Assembly; appointment answers to President but confidence answers to Assembly; high tenure instability during cohabitation
 *   - Political party controlling Assembly: domestically dominant; controls legislation, budget, civil service personnel; excludes foreign policy and defense from its domain
 *   - Political party controlling Presidency: foreign-policy dominant; controls defense, judicial appointments, emergency powers; excluded from direct domestic agenda-setting
 *   - Constitutional Court: arbiter of boundary disputes; invoked repeatedly during cohabitation to clarify jurisdiction when executive actors disagree
 *   - Civil service apparatus: experiences whipsaw between conflicting executive directives; navigates dual command structure without formal hierarchy except in narrow domain rules
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.58).
domain_priors:suppression_score(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.42).
domain_priors:theater_ratio(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__cohabitation_equilibrium_reading, "Fifth Republic Cohabitation Equilibrium: Dual Executive Authority Allocation").
narrative_ontology:topic_domain(fifth_republic_constitution__cohabitation_equilibrium_reading, "constitutional/political").

domain_priors:requires_active_enforcement(fifth_republic_constitution__cohabitation_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__cohabitation_equilibrium_reading, '2010a517-7b8d-4468-8e02-54ed9f7b70dc').
narrative_ontology:cs_kernel_codification('2010a517-7b8d-4468-8e02-54ed9f7b70dc', fixed_text).
narrative_ontology:cs_authority_grounding('2010a517-7b8d-4468-8e02-54ed9f7b70dc', lineage).
narrative_ontology:cs_interpretation_layer_present('2010a517-7b8d-4468-8e02-54ed9f7b70dc').
narrative_ontology:cs_reading_relation('2010a517-7b8d-4468-8e02-54ed9f7b70dc', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('2010a517-7b8d-4468-8e02-54ed9f7b70dc', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('2010a517-7b8d-4468-8e02-54ed9f7b70dc', foundational, dual_executive_authority_unstable).
narrative_ontology:cs_axiom_status(dual_executive_authority_unstable, holdable).
narrative_ontology:cs_axiom_grounding('2010a517-7b8d-4468-8e02-54ed9f7b70dc', dual_executive_authority_unstable, instrumental).
narrative_ontology:cs_axiom('2010a517-7b8d-4468-8e02-54ed9f7b70dc', foundational, constitutional_text_deliberately_ambiguous).
narrative_ontology:cs_axiom_status(constitutional_text_deliberately_ambiguous, holdable).
narrative_ontology:cs_axiom_grounding('2010a517-7b8d-4468-8e02-54ed9f7b70dc', constitutional_text_deliberately_ambiguous, conventional).
narrative_ontology:cs_axiom('2010a517-7b8d-4468-8e02-54ed9f7b70dc', secondary, negotiated_compromise_preferable_to_hierarchy).
narrative_ontology:cs_axiom_status(negotiated_compromise_preferable_to_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('2010a517-7b8d-4468-8e02-54ed9f7b70dc', negotiated_compromise_preferable_to_hierarchy, instrumental).
narrative_ontology:cs_reference_frame('2010a517-7b8d-4468-8e02-54ed9f7b70dc', balanced_dual_executive_negotiation).
narrative_ontology:cs_drift_state('2010a517-7b8d-4468-8e02-54ed9f7b70dc', contemporary_electoral_fragmentation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2010a517-7b8d-4468-8e02-54ed9f7b70dc', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, political_party_controlling_assembly).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, political_party_controlling_presidency).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, electoral_mandate_clarity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, president).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, voters).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, civil_service_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected directly by popular vote for seven years (now five after 2000 reform). Commands foreign policy, defense, judicial appointments (Constitutional Council), and emergency powers (Article 16). During cohabitation with opposing Assembly majority, loses control of domestic policy agenda (education, healthcare, social policy, budgeting) to an Assembly that can override presidential veto via supermajority or refuse to pass presidential initiatives. Can dissolve Assembly, triggering new elections — this is primary leverage against hostile majority. Cannot unilaterally legislate; must work through Prime Minister who answers to Assembly. Pays by accepting policy domain loss and dependence on a Prime Minister who may represent opposing party.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, president, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, president, payer).

% Appointed by the President (Article 8) but accountable to the National Assembly (can be dismissed by no-confidence vote). During cohabitation, executes domestic law and represents government to Assembly but has zero authority over foreign policy or defense (President's domain). Navigates between two masters: President who appointed them (and can remove them unilaterally) and Assembly majority who can remove them collectively (no-confidence). Highest structural instability: serves at pleasure of both and subject to removal by either. Acts as mediator and negotiator of policy compromise between President and Assembly.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister, agenda_setter,
    institutional, generational, trapped, national).

% Controls domestic legislation through Assembly majority. Sets budget, shapes civil service, controls Prime Minister selection (confidence votes). During cohabitation, blocks presidential initiatives in their domain and shapes Prime Minister (who leads 'their' government in practice). Pays by surrendering foreign policy, defense, and judicial appointments to President; also faces the political cost of sharing executive responsibility with opposing President (complicates electoral messaging; they are blamed for domestic failures even though President controls some variables).
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, political_party_controlling_assembly, beneficiary,
    organized, generational, mobile, national).

% Controls foreign policy, defense, and constitutional prerogatives. During cohabitation, retains international standing and strategic authority but loses direct control of domestic agenda. Can potentially dissolve Assembly and trigger new elections (primary leverage). Pays by accepting that their domestic program cannot be unilaterally imposed; must either negotiate with hostile Prime Minister or absorb electoral cost of Assembly dissolution and failure to win new majority.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, political_party_controlling_presidency, beneficiary,
    organized, generational, mobile, national).

% Elect President separately from National Assembly; elections occur on separate schedules (presidential every 5 years after 2000 reform; Assembly every 5 years but typically called between by presidential dissolution). May inadvertently create cohabitation by voting for opposite majorities (or may deliberately do so to enforce balance). During cohabitation, cannot hold a single accountable executive; signals get mixed; policy implementation slows. Must wait for next election cycle to alter the configuration.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, voters, payer,
    powerless, biographical, mobile, national).

% The Constitutional Council adjudicates disputes between President and Prime Minister over jurisdiction. During cohabitation, frequently asked to clarify: which authority controls particular policy domains, who can act unilaterally, what requires negotiated agreement. Does not govern but is drawn into ambiguity resolution. Acts as referee in disputes and as interpreter of constitutional text.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_court, observer,
    institutional, generational, analytical, national).

% Professional administrative corps receives directives from both President and Prime Minister; formal hierarchy is ambiguous. During cohabitation, may receive contradictory instructions (President wants one policy; Prime Minister wants another in overlapping domain). Must navigate without clear authority hierarchy. Experiences institutional stress and uncertainty. Exit is difficult (career path is embedded in civil service); identity-locked to institutional role.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, civil_service_apparatus, payer,
    moderate, generational, trapped, national).

% A non-agent entity tracked as victim: the coherence and consistency of government policy. During cohabitation, policy coherence suffers from dual-executive negotiation — contradictory signals, delayed implementation, compromises that satisfy neither seat, and institutional strain that diverts energy from policy execution to political negotiation.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).

% A non-agent entity tracked as victim: the clarity of electoral mandates. When voters create cohabitation (or when it emerges from separate electoral cycles), no single executive holds a clear mandate from the majority. Voters cannot attribute success or failure to a single accountable source; accountability is diffused.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, electoral_mandate_clarity, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_non_agent(fifth_republic_constitution__cohabitation_equilibrium_reading, electoral_mandate_clarity).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__cohabitation_equilibrium_reading, diffuse).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__cohabitation_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Splits executive authority between popularly-elected President (foreign policy, defense, emergency powers, judicial appointments) and Assembly-accountable Prime Minister (domestic legislation, budgeting, civil administration), preventing either branch from monopolizing executive power and requiring negotiated consensus on cross-domain issues.
% TRANSFER_FUNCTION: Transfers policy priority from whichever party controls Assembly to whichever party controls Presidency; when aligned (no cohabitation), power concentrates; when opposed (cohabitation), neither party can unilaterally implement program. The constraint redistributes authority to require explicit negotiation rather than hierarchical command.
% ABSENT_VOICES: Citizens excluded from ongoing constitutional renegotiation — the courts, scholarly tradition, and parliamentary precedent evolve the boundary without direct electoral input. Non-governmental stakeholders affected by delayed or incoherent domestic policy have no formal seat in cohabitation management. Regional governments and subnational actors experience the constraint but have limited voice in how it operates.
% DISAPPEARANCE_RATIONALE: If this authority allocation constraint vanished and the Fifth Republic collapsed into either hyper-presidential or parliamentary form, the domestic policy regime would reorganize: power would concentrate in one executive source, checks-and-balances would shift, and the electoral incentives that currently push voters to create cohabitation would evaporate. The French constitutional order would become structurally different.
% FOUNDING_PROBLEM: Prevent executive tyranny and protect legislative independence from presidential monopoly by constitutionalizing direct presidential election (drawing legitimacy from popular will) while preserving Assembly accountability and budgetary control; balance national sovereignty claims of the President with republican representation in the Assembly.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and Presidents attest the founding problem remains live — checks on tyranny are structurally necessary. Opposition parties and some constitutional theorists attest the original problem (fear of legislative weakness or executive dictatorship) has shifted: they contend the cohabitation mechanism now serves to hide policy incoherence rather than prevent tyranny. Judicial decisions and legislative debates document the contest without resolving it.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__cohabitation_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__cohabitation_equilibrium_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fifth_republic_constitution__cohabitation_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the moderate but real cost of the dual-executive coordination: policy domains outside the controlling party's reach become contested and slower to implement. Early in the Fifth Republic (1958–1978, extracted 0.35) the President retained substantial informal dominance and cohabitation was rare; as electoral cycles shifted and cohabitation occurred (1986–1988, 1993–1995, 1997–2002, 2016–2017), the structural ambiguity became operative and extractiveness rose. Theater ratio (0.48) captures the growing mismatch between ceremonial constitutional language (President embodies national will; Prime Minister administers domestic affairs) and operational reality (neither unilaterally controls their ostensible domain during cohabitation; negotiation substitutes for command). Suppression is moderate (0.42) because the constraint persists through formal legitimacy, not active coercion — institutional rules and constitutional precedent hold it in place, though the arrangement increasingly faces pressure from reform movements and electoral volatility. Accessibility collapse (0.35) is low because alternatives remain visible (a reformed constitution assigning full presidential power, or a true parliamentary system) but are politically difficult to achieve; voters express discontent by altering electoral choices without committing to structural reform. Resistance (0.71) is high because political parties actively contest the constraint's meaning — the hyper-presidential reading and parliamentary-constraint reading remain live positions in constitutional disputes, and intellectual/judicial energy flows toward clarifying or reshaping the equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   The President's seat and the Assembly-controlling-party's seat compute divergent types from identical structural data. From the President's position during cohabitation, the constraint appears as an imposed constraint on prerogatives previously exercised unilaterally — high directionality toward victimhood (near 1.0), higher extractiveness perceived, grievance high. From the Assembly-controlling-party's position, the constraint is a coordination mechanism preventing presidential monopoly — beneficiary seat (near 0.0 directionality), lower extractiveness perceived, grudging acceptance of power-sharing. The civil service experiences the constraint as pure suppression: conflicting command authority, no escape, identity-locked to institutional roles. The engine computes per-seat types from these structural asymmetries; the authored claim (tangled_rope) reflects the structure that produces divergence, not a averaged position.
 *
 * DIRECTIONALITY LOGIC:
 *   The President and party-controlling-Assembly are both beneficiaries when they hold power (they set policy in their domain) and both payers when they do not (the other executive constrains their reach). Directionality is asymmetric not by power level (both are institutional actors) but by exit options and identity-lock: once elected, the President cannot simply relinquish office if the Assembly is hostile — the identity-fusion to the presidency is near-total for the seven-year term. The Prime Minister is trapped between hierarchies (appointed by President, accountable to Assembly) with no clean exit except dismissal. The political parties have higher mobility than the individual executives — they can shift strategy, negotiate differently, or seek electoral repositioning — but are identity-locked to the partisan program they must defend. Voters face identity-lock to the nation-state (exit = emigration, nearly trapped) even though their electoral choice formally determines the constraint's operation. The civil service is trapped (career depends on institutional continuity) and identity-locked (professional identity constituted through service to abstract state authority, not to either executive). Directionality for the President (payer seat when Assembly opposes): d ≈ 0.72. For the Assembly-controlling-party (beneficiary in their domain, payer when constrained on cross-domain issues): d ≈ 0.45. For the civil service (trapped, identity-locked, receives conflicting command): d ≈ 0.85.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (prevent tyranny; balance popular will with republican representation) has not disappeared but has transformed. Early Fifth Republic (1958–1978) witnessed the founding problem acutely alive: fear of presidential overreach was real; the Assembly needed constitutional protection. From 1986 forward, the constraint's operation shifted. When cohabitation occurred, voters experienced not tyranny-prevention but rather policy incoherence and slow implementation — the constraint now extracted the cost of dual command without delivering the founding justification (because both executives were legitimate, neither was a tyrant trying to override the other). The constitutional scholarship and electoral behavior diverged: some theorists (hyper-presidential reading) argued the founding problem was solved and the equilibrium should tilt presidential; others (parliamentary-constraint reading) argued it was never the real problem and the constraint should clarify legislative supremacy; the cohabitation-equilibrium reading (this one) treats the founding problem as permanently contested — the mechanism itself creates the conditions that make tyranny-detection ambiguous. Mandatrophy is not fully resolved but is partially apparent: the constraint persists with legitimacy fraying as voters experience cohabitation phases as dysfunctional rather than protective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_text_ambiguity,
    'Does the Fifth Republic Constitution intentionally leave executive authority allocation ambiguous, or does it contain a resolution that different readings have obscured?',
    'Examination of the Constitutional Commission proceedings (1958), de Gaulle''s constitutional intentions as recorded in contemporary sources, and the pattern of judicial decisions clarifying Article 8 (Prime Minister dismissal), Article 5 (President role), and emergency powers. If the commission intentionally left it open, it is feature; if it intended hierarchy but successive practice obscured it, the reading forks.',
    'If intentionally ambiguous: the cohabitation-equilibrium reading is a legitimate instantiation of constitutional design, and mandatrophy is minimal (the constraint does what it was meant to do, even if it extracts policy cost). If unintentionally ambiguous: the constraint is a design failure, and constitutional reform is justifiable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_text_ambiguity, empirical, 'Whether the constitutional ambiguity is intentional design or unintended omission').

omega_variable(
    kernel_foreclusion_test,
    'Can all three readings (hyper-presidential, cohabitation-equilibrium, parliamentary-constraint) coexist as live interpretations of the same constitutional text, or does adoption of one reading logically foreclose the others?',
    'Test via counterfactual: if a credible political force acted on the hyper-presidential reading (President unilaterally legislates), would the constitutional court treat it as foreclosed, or as a live alternative interpretation? Historical precedent: Mitterrand''s 1981–1986 presidency acted on quasi-parliamentary reading (strong PM role); Chirac''s 2002–2007 acted on hyper-presidential reading after term-limit reform. No reading was foreclosed; all remain available to future governments. This suggests coexistence, not foreclosure.',
    'If readings coexist: this constraint is a genuine tangled_rope with persistent ambiguity — no seat can claim the Constitution resolved the dispute in their favor. If one reading forecloses others: the constraint reduces to a single type, and the others are misreadings to be corrected.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_foreclusion_test, conceptual, 'Whether the three readings logically exclude each other or coexist as live alternatives').

omega_variable(
    policy_coherence_as_victim,
    'Is policy incoherence during cohabitation a necessary extraction cost of checks-and-balances, or a pathology that reform could eliminate?',
    'Comparative constitutional analysis: do other dual-executive systems (e.g., Germany with chancellor + ceremonial president, Austria, Ireland) experience the same incoherence during divided government? If yes, it is inherent. If no, design differences explain it. Also: measurement of policy implementation speed and success rates during aligned vs. opposed presidencies and assemblies in France; trend analysis of citizen satisfaction with government responsiveness.',
    'If inherent cost: the extraction (0.58) fairly represents necessary friction in a checks-and-balances system. If design-contingent: a reformed constitution could reduce extraction by clarifying hierarchy or streamlining negotiation. Directs mandatrophy assessment: is it unresolved ambiguity (calls for reform) or permanent feature (calls for acceptance)?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_coherence_as_victim, empirical, 'Whether policy incoherence during cohabitation is necessary or remediable').

omega_variable(
    electoral_mandate_clarity_victim,
    'Does the constraint (separate presidential and Assembly elections) intentionally obscure mandate clarity, or is mandate obscurity an unintended side effect of institutional design?',
    'Historical record: in 1986, 1993, 1997, 2002, 2016–2017 when cohabitation occurred, did voters deliberately create it (signaling preference for dual-executive balance) or inadvertently (voting on separate local/national concerns at separate elections)? Polling data and electoral analysis can distinguish strategic from random cohabitation creation. If strategic: mandate clarity is sacrificed for other values (balance, restraint). If random: it is an unintended extraction.',
    'If strategic: voters consciously accept the clarity cost in exchange for checks-and-balances; the extraction is consensual and the constraint is legitimate tangled_rope. If random: the extraction represents unintended negative externality; reform to clarify mandate becomes defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(electoral_mandate_clarity_victim, empirical, 'Whether voters deliberately create cohabitation or encounter it as unintended consequence').

omega_variable(
    kernel_interpretation_foreclusion_dynamics,
    'Under what conditions would the Constitutional Court treat one reading as foreclosed by another? Are there empirical or normative pressures that would push the court toward selecting one reading permanently?',
    'Examination of landmark constitutional decisions (Constitutional Council decisions 1986–2017 during cohabitation periods). Does the Court systematically favor one reading over others when directly asked? Or does it refuse to foreclose, treating ambiguity as legitimate? Also: whether EU law integration, human rights jurisprudence, or reform referenda have created upstream pressures that privilege one reading.',
    'If the Court shows foreclosure tendency: the cohabitation-equilibrium reading may be transitional, destined to collapse into hyper-presidential or parliamentary as external pressures accumulate. If the Court maintains ambiguity: cohabitation-equilibrium is institutionally stable. Feeds terminal-state prediction and mandatrophy timeline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_interpretation_foreclusion_dynamics, conceptual, 'Foreclosure dynamics and trajectory toward terminal reading state').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__cohabitation_equilibrium_reading, 1958, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1958, 0.25).
narrative_ontology:measurement(fift_tr_t1978, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1978, 0.28).
narrative_ontology:measurement(fift_tr_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1993, 0.4).
narrative_ontology:measurement(fift_tr_t2007, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2007, 0.44).
narrative_ontology:measurement(fift_tr_t2017, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2017, 0.47).
narrative_ontology:measurement(fift_tr_t2025, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1958, 0.35).
narrative_ontology:measurement(fift_be_t1978, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1978, 0.42).
narrative_ontology:measurement(fift_be_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1993, 0.48).
narrative_ontology:measurement(fift_be_t2007, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2007, 0.55).
narrative_ontology:measurement(fift_be_t2017, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2017, 0.57).
narrative_ontology:measurement(fift_be_t2025, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1958, 0.3).
narrative_ontology:measurement(fift_su_t1978, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1978, 0.35).
narrative_ontology:measurement(fift_su_t1993, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1993, 0.38).
narrative_ontology:measurement(fift_su_t2007, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2007, 0.42).
narrative_ontology:measurement(fift_su_t2017, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2017, 0.42).
narrative_ontology:measurement(fift_su_t2025, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2025, 0.42).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1958, tn=2025
narrative_ontology:measurement(fift_grid_01, fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse(class), 1958, 0.18).
narrative_ontology:measurement(fift_grid_02, fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse(class), 2025, 0.25).
narrative_ontology:measurement(fift_grid_03, fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse(individual), 1958, 0.22).
narrative_ontology:measurement(fift_grid_04, fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse(individual), 2025, 0.28).
narrative_ontology:measurement(fift_grid_05, fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse(organizational), 1958, 0.35).
narrative_ontology:measurement(fift_grid_06, fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse(organizational), 2025, 0.42).
narrative_ontology:measurement(fift_grid_07, fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse(structural), 1958, 0.45).
narrative_ontology:measurement(fift_grid_08, fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse(structural), 2025, 0.48).
narrative_ontology:measurement(fift_grid_09, fifth_republic_constitution__cohabitation_equilibrium_reading, resistance(class), 1958, 0.72).
narrative_ontology:measurement(fift_grid_10, fifth_republic_constitution__cohabitation_equilibrium_reading, resistance(class), 2025, 0.78).
narrative_ontology:measurement(fift_grid_11, fifth_republic_constitution__cohabitation_equilibrium_reading, resistance(individual), 1958, 0.55).
narrative_ontology:measurement(fift_grid_12, fifth_republic_constitution__cohabitation_equilibrium_reading, resistance(individual), 2025, 0.62).
narrative_ontology:measurement(fift_grid_13, fifth_republic_constitution__cohabitation_equilibrium_reading, resistance(organizational), 1958, 0.68).
narrative_ontology:measurement(fift_grid_14, fifth_republic_constitution__cohabitation_equilibrium_reading, resistance(organizational), 2025, 0.75).
narrative_ontology:measurement(fift_grid_15, fifth_republic_constitution__cohabitation_equilibrium_reading, resistance(structural), 1958, 0.8).
narrative_ontology:measurement(fift_grid_16, fifth_republic_constitution__cohabitation_equilibrium_reading, resistance(structural), 2025, 0.82).
narrative_ontology:measurement(fift_grid_17, fifth_republic_constitution__cohabitation_equilibrium_reading, stakes_inflation(class), 1958, 0.28).
narrative_ontology:measurement(fift_grid_18, fifth_republic_constitution__cohabitation_equilibrium_reading, stakes_inflation(class), 2025, 0.35).
narrative_ontology:measurement(fift_grid_19, fifth_republic_constitution__cohabitation_equilibrium_reading, stakes_inflation(individual), 1958, 0.32).
narrative_ontology:measurement(fift_grid_20, fifth_republic_constitution__cohabitation_equilibrium_reading, stakes_inflation(individual), 2025, 0.38).
narrative_ontology:measurement(fift_grid_21, fifth_republic_constitution__cohabitation_equilibrium_reading, stakes_inflation(organizational), 1958, 0.55).
narrative_ontology:measurement(fift_grid_22, fifth_republic_constitution__cohabitation_equilibrium_reading, stakes_inflation(organizational), 2025, 0.61).
narrative_ontology:measurement(fift_grid_23, fifth_republic_constitution__cohabitation_equilibrium_reading, stakes_inflation(structural), 1958, 0.52).
narrative_ontology:measurement(fift_grid_24, fifth_republic_constitution__cohabitation_equilibrium_reading, stakes_inflation(structural), 2025, 0.58).
narrative_ontology:measurement(fift_grid_25, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression(class), 1958, 0.22).
narrative_ontology:measurement(fift_grid_26, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression(class), 2025, 0.28).
narrative_ontology:measurement(fift_grid_27, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression(individual), 1958, 0.15).
narrative_ontology:measurement(fift_grid_28, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression(individual), 2025, 0.18).
narrative_ontology:measurement(fift_grid_29, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression(organizational), 1958, 0.38).
narrative_ontology:measurement(fift_grid_30, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression(organizational), 2025, 0.42).
narrative_ontology:measurement(fift_grid_31, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression(structural), 1958, 0.32).
narrative_ontology:measurement(fift_grid_32, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression(structural), 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__cohabitation_equilibrium_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.18).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__parliamentary_constraint_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Fifth Republic Constitution kernel. The three readings (cohabitation_equilibrium_reading, hyper_presidential_reading, parliamentary_constraint_reading) share the same constitutional text but fork on the interpretation of executive authority allocation. The cohabitation-equilibrium reading treats authority as unstable and negotiated; it coexists with sibling readings that assert either presidential dominance or parliamentary supremacy. All three readings are instantiated simultaneously in constitutional practice — the President and Assembly act on their respective readings, and the courts arbitrate. The network links capture the mutual structural influence: each reading's persistence affects the feasibility and interpretation of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__cohabitation_equilibrium_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
