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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: article_27_veto_power__oligopoly_reading
 *   human_readable: UN Charter Article 27 Veto — Geopolitical Oligopoly Reading
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   UN Charter Article 27 grants five states permanent, unconditional
 *   blocking power over every substantive Security Council decision; Article
 *   108 completes the structure by requiring ratification by all five for any
 *   amendment, so the arrangement itself cannot be changed without the
 *   consent of everyone it privileges. Across the 80-year interval the
 *   membership grew from 51 to 193 states, empires dissolved, and power
 *   diffused — the allocation did not move. This file instantiates the
 *   OLIGOPOLY READING of the contested article_27_veto_power kernel: the
 *   standing arrangement as a maintained privilege structure whose
 *   war-prevention story functions partly as cover for ongoing authority
 *   rents, with the non-P5 majority as the party that bears the arrangement's
 *   costs with no path to reform. Per the one-reading rule, the sibling
 *   readings (coordination_reading, sovereignty_reading) are separate
 *   constraints in separate files with their own epsilon, beneficiaries, and
 *   classifications; epsilon here refers only to the standing arrangement as
 *   this reading assesses it. KEY AGENTS (by structural relationship): -
 *   p5_permanent_members: Agenda-setting beneficiary
 *   (institutional/arbitrage) — holds the blocking power and the amendment
 *   gate; collects agenda control, self-exemption, and leverage; can act
 *   outside the system when it displeases - non_p5_general_membership:
 *   Primary target (organized/trapped) — bears decisions made without check;
 *   no exit, no reform path bypassing P5 ratification - elected_ten_members:
 *   Secondary target (moderate/constrained) — full workload, zero blocking
 *   power, term-limited access - g4_expansion_candidates: Excluded claimant
 *   (powerful/identity_locked) — locked out by the same rule that protects
 *   incumbents - climate_vulnerable_small_states: Concentrated-cost target
 *   (moderate/trapped) — faces enforcement-dependent threats the paralyzed
 *   Council cannot meet - un_secretariat: Analytical observer
 *   (institutional/analytical) — documents the mandate-function gap from
 *   inside
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, 0.79).
domain_priors:suppression_score(article_27_veto_power__oligopoly_reading, 0.8).
domain_priors:theater_ratio(article_27_veto_power__oligopoly_reading, 0.49).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, theater_ratio, 0.49).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__oligopoly_reading, snare).
narrative_ontology:human_readable(article_27_veto_power__oligopoly_reading, "UN Charter Article 27 Veto — Geopolitical Oligopoly Reading").
narrative_ontology:topic_domain(article_27_veto_power__oligopoly_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:requires_active_enforcement(article_27_veto_power__oligopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__oligopoly_reading, '418dda5c-b254-4d43-b91f-fb2bc957ee3c').
narrative_ontology:cs_kernel_codification('418dda5c-b254-4d43-b91f-fb2bc957ee3c', fixed_text).
narrative_ontology:cs_authority_grounding('418dda5c-b254-4d43-b91f-fb2bc957ee3c', extraction).
narrative_ontology:cs_interpretation_layer_present('418dda5c-b254-4d43-b91f-fb2bc957ee3c').
narrative_ontology:cs_reading_relation('418dda5c-b254-4d43-b91f-fb2bc957ee3c', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('418dda5c-b254-4d43-b91f-fb2bc957ee3c', article_27_veto_power__sovereignty_reading, influences).
narrative_ontology:cs_axiom('418dda5c-b254-4d43-b91f-fb2bc957ee3c', foundational, charter_amendment_lock_functions_as_rent_shield).
narrative_ontology:cs_axiom_status(charter_amendment_lock_functions_as_rent_shield, holdable).
narrative_ontology:cs_axiom_grounding('418dda5c-b254-4d43-b91f-fb2bc957ee3c', charter_amendment_lock_functions_as_rent_shield, empirically_contingent).
narrative_ontology:cs_axiom('418dda5c-b254-4d43-b91f-fb2bc957ee3c', foundational, unaccountable_permanent_authority_is_illegitimate).
narrative_ontology:cs_axiom_status(unaccountable_permanent_authority_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('418dda5c-b254-4d43-b91f-fb2bc957ee3c', unaccountable_permanent_authority_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('418dda5c-b254-4d43-b91f-fb2bc957ee3c', san_francisco_privilege_compact).
narrative_ontology:cs_drift_state('418dda5c-b254-4d43-b91f-fb2bc957ee3c', contemporary_postcolonial_membership_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('418dda5c-b254-4d43-b91f-fb2bc957ee3c', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__oligopoly_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, p5_permanent_members).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, non_p5_general_membership).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, elected_ten_members).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, climate_vulnerable_small_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent seats with unconditional blocking power over every substantive Security Council decision, and hold an absolute gate on Charter amendment: Article 108 requires ratification by all five, so any one of them kills any redistribution of the arrangement itself. Each collects agenda control over peace and security, exemption from Council action directed at itself, and bargaining leverage over the other 188 members. When the Council obstructs them, each retains the option of acting outside the UN system entirely — coalitions of the willing, alliances, unilateral force — while keeping the seat and its protections.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, p5_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, p5_permanent_members, beneficiary).

% One hundred eighty-eight states formally equal in the organization, voting in the General Assembly on a one-state-one-vote basis, yet subject to binding Council decisions they cannot block and unable to place enforcement behind Assembly preferences. They fund peacekeeping, supply troops, and host sanctions effects. Exit from the UN system would forfeit recognition, treaty infrastructure, and legitimacy; reform of the arrangement requires unanimous P5 ratification, so the collective action of the majority terminates at a gate they do not control. Eighty years of membership growth from 51 to 193 states has not moved the allocation.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, non_p5_general_membership, payer,
    organized, generational, trapped, global).

% Rotate through two-year non-permanent Council seats carrying the full workload of drafting, negotiating, and legitimating Council output, with zero blocking power: any single permanent member can void whatever they build. They receive diplomatic access and prestige from the seat, which softens their position relative to the general membership, but their votes are overridable at will and their tenure ends regardless. The 1963-65 amendment that expanded their numbers succeeded precisely because it left permanent privileges untouched.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, elected_ten_members, payer,
    moderate, immediate, constrained, global).

% Brazil, Germany, India, and Japan — major regional powers and leading financial or troop contributors — have campaigned for permanent seats for decades. The amendment rule that protects the incumbents is the same rule that locks them out: any P5 member can kill expansion, and rivals of each candidate (the 'coffee club') add a second layer of blocking. Abandoning the bid would mean abandoning a core national aspiration tied to great-power status recognition, so the pursuit persists even as every attempt fails.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, g4_expansion_candidates, excluded,
    powerful, generational, identity_locked, global).

% Small island and low-lying states facing security threats — sea-level rise, resource conflict, displacement — that require enforcement-capable response. When any permanent member blocks Council action touching its interests or allies, these states have no alternative forum with coercive capacity, and no exit from the climate-security problem itself. They bear concentrated costs of a paralysis they have no instrument to break.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, climate_vulnerable_small_states, payer,
    moderate, generational, trapped, global).

% Administers the machinery whose political head is periodically paralyzed: Secretaries-General have repeatedly documented the gap between the Charter's mandate and the Council's functioning, while depending on P5 goodwill for budget and renewal. They see the full structure from inside, can name it, and cannot move it.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, un_secretariat, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__oligopoly_reading, p5_permanent_members).
narrative_ontology:fixing_cost_class(article_27_veto_power__oligopoly_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Guarantees that no Security Council decision can compel a great power into military confrontation it rejects, keeping the strongest states inside the institution rather than driving them into a rival league — the failure mode that destroyed the League of Nations.
% TRANSFER_FUNCTION: Moves agenda-setting authority and decision rights over international peace and security from the general membership (193 states, one-state-one-vote in the Assembly) to five states holding permanent blocking power; moves exemption from enforcement from the many to the few; moves the cost of systemic paralysis to those without blocking power.
% ABSENT_VOICES: The majority of today's membership was colonized or otherwise excluded when the allocation was fixed at Dumbarton Oaks and San Francisco; small-state objections were heard there and overruled. Today, expansion claimants (the G4, the African Union's Ezulwini consensus demanding permanent African representation) are structurally locked out of the conversation that would admit them, because the amendment rule gives the incumbents an absolute gate on who may next hold blocking power.
% DISAPPEARANCE_RATIONALE: If the veto and its amendment lock vanished overnight, the Council would reorganize around qualified majority voting; the great powers would face a choice between accepting binding constraints and defecting to parallel structures, rearranging alliance systems either way; agenda control, self-exemption, and bargaining leverage would redistribute immediately, and the 188 non-permanent members would gain a live reform path for the first time since 1945.
% FOUNDING_PROBLEM: Prevent a repeat of the League of Nations collapse: guarantee that great powers remain members of the organization by ensuring no enforcement action can be taken against them without their consent, so the institution survives great-power disagreement.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the historical record of the League's failure corroborates that the founding problem was real (standard diplomatic historiography, not P5 assertion); San Francisco conference proceedings corroborate that small states objected to the privilege and were overruled; the ACT group, the French-Mexican restraint initiative, and the Liechtenstein initiative's sponsors — all outside the P5 — attest that the founding rationale no longer justifies the standing arrangement. No corroborating source exists above the system itself; corroboration comes from member-state majorities and independent scholarship, which is itself a structural fact about the arrangement.
narrative_ontology:disappearance_verdict(article_27_veto_power__oligopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__oligopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__oligopoly_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_27_veto_power__oligopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__oligopoly_reading, 0.79, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.79 at interval end) because the rent is unconditional: agenda control, self-exemption from enforcement, and leverage accrue whether or not the veto is cast, and the 1963-65 episode is diagnostic — the only successful Charter amendment in the Council's history expanded the elected seats precisely because it left permanent privileges untouched, showing the lock protects the privilege, not the institution. Suppression is high (0.80) because persistence depends on the Article 108 unanimity gate actively held by the P5, not on participant preference; alternatives (Assembly resolutions, Uniting for Peace, regional bodies) persist formally but collapse in enforcement capacity, giving accessibility_collapse 0.65 — partial, not total, since the alternatives exist and are used symbolically. Theater_ratio 0.49 reflects a Council whose deliberative output increasingly substitutes statements, pledges, and procedural workarounds for binding action. Resistance 0.7 reflects eight decades of sustained reform pressure: repeated expansion bids, the Razali plan, the 2005 World Summit, restraint codes, the Liechtenstein initiative. The measurement series run on ONE shared time grid (every tracked metric authored at every decade point) and trace an enforcement CYCLE: Cold War intensification (veto barrages, suppression 0.70-0.74), post-Cold-War decay (fewest vetoes, suppression dipping to 0.52 as enforcement relaxed), then re-hardening after 2014 as reform pressure and great-power friction returned (suppression 0.80, with the P5 issuing a joint restraint pledge — defensive mobilization). The oscillation is driven by external geopolitical temperature, not by intermittent reinforcement design. Coalition check: the G77 and G4 demonstrate that coalition power is structurally inert here — collective action by the many cannot clear a unanimity gate controlled by non-coalition members, which distinguishes this arrangement from ordinary majority-rule politics where numbers convert to power. Claim/metric independence: claimed_type 'snare' is authored from the structural read (a coordination story whose persistence depends on suppressing exits and alternatives, with identifiable victims); the metrics are authored independently from the descriptive record, and the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   From the P5 seat the arrangement presents as prudent mutual insurance: each member calculates that it may need the veto, regards the others' privileges as the price of its own, and experiences the structure as self-defensive coordination it helped build — the engine should compute a low-chi, coordination-flavored experience there, amplified by their arbitrage exit (they lose least by defection). From the trapped non-P5 seats the same structure presents as unaccountable rule: binding decisions without recourse, a reform path that terminates at someone else's gate — high chi. The elected ten compute an intermediate seat: genuine access and prestige, zero leverage, term-limited. Same-level lateral dynamics: all member states are formally equal sovereigns, yet exit options diverge sharply along one constraint-specific axis — possession of the ratification gate. P5 members hold arbitrage (act outside, keep the seat); the G4 hold identity-locked commitment (the permanent-seat aspiration is fused with national self-conception and cannot be abandoned without identity cost); small vulnerable states are trapped with no substitute forum. Equal global standing, radically different structural relationships to the same text.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map to directionality as follows. p5_permanent_members: declared beneficiary with arbitrage-grade exit — d sits near the beneficiary pole, and effective extraction is damped toward subsidy: the arrangement pays them. non_p5_general_membership: declared victim, trapped — d near the full-target pole, amplified by global scope (verification of the arrangement's fairness is impossible; no auditor sits above the system). elected_ten_members: declared victim with constrained exit and a real participation benefit — high d but short of maximum. climate_vulnerable_small_states: declared victim, trapped, bearing concentrated costs of others' paralysis — near-maximal d. un_secretariat: observer seat, analytical exit — no directional stake. Scope amplification applies to the extractive side only; suppression enters unscaled as the raw structural property it is.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keep the great powers inside after the League's collapse — was real, and the arrangement addressed it; the classification must resist two opposite errors. Reading the whole structure as pure coordination licenses the P5's own account and erases the asymmetric transfer; reading it as pure confiscation erases the genuine participation-stabilization function the historical record supports. The mandatrophy question resolves as CONTESTED, not dead: great-power war remains possible, so the founding rationale has not plainly expired — this is not a resolved-mandatrophy piton awaiting burial. The snare classification turns on the asymmetry the coordination story conceals: the coordination benefit is diffuse, conditional, and shared; the rent is concentrated, unconditional, and private; and the amendment lock guarantees the rent outlives any change in the benefit. The engine's per-seat computation should surface exactly this divergence — a coordination-flavored experience at the P5 seat and an extraction-flavored experience at the trapped seats — computed from the same structural data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Which reading of the Article 27 kernel governs classification — coordination (war-prevention necessity), sovereignty (consent principle), or oligopoly (privilege entrenchment)?',
    'Locate where each reading''s warrant breaks: coordination fails if war prevention is attributable to nuclear deterrence rather than Council structure; sovereignty fails if 1945 consent cannot bind successor generations of a 193-state membership; oligopoly fails if P5 privileges track genuine, currently-load-bearing enforcement capacity rather than legacy position.',
    'Classification is reading-indexed: the coordination reading yields a low-epsilon, rope-like profile; the sovereignty reading yields a different victim set (all law-bound states generally); this reading yields a snare with the non-P5 majority as victims. The sibling files carry the sibling classifications; no single file can average across them without violating epsilon-invariance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: the constraint''s type depends on which reading of the shared kernel is instantiated; this file is the oligopoly member of a three-story family.').

omega_variable(
    war_prevention_attribution,
    'Has the veto actually prevented great-power war, or has nuclear deterrence done the work the coordination story credits to the Council structure?',
    'Comparative crisis analysis (Berlin, Cuba, Korea, Indochina, recent great-power confrontations): trace whether Council channels or bilateral deterrence carried the stabilization in each case, using archival and diplomatic-historical evidence independent of P5 self-accounting.',
    'If deterrence suffices, the coordination cover thins and this reading''s epsilon stands with less discount; if Council channels were load-bearing, part of the measured extraction is the genuine price of great-power participation and the arrangement shades toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(war_prevention_attribution, empirical, 'Attribution of the war-prevention effect between institutional structure and nuclear deterrence — the empirical hinge between the coordination and oligopoly readings.').

omega_variable(
    amendment_deadlock_absoluteness,
    'Is the Article 108 amendment lock absolute in practice, or does a reform package exist (expansion, restraint codification) that all five permanent members would ratify?',
    'Negotiation-track analysis of failed attempts — the Razali plan (1997), the 2005 World Summit, successive G4 bids, the French-Mexican restraint declaration, the ACT code of conduct — identifying whether each failed on P5 ratification power itself or on intra-P5 divergence that a different package could bridge.',
    'If the lock is absolute, the constraint is a closed snare with no internal reform path and exit-blocking is total; if a ratifiable package exists, the arrangement is contestable and shades toward tangled_rope — genuine coordination purchasable at a negotiated price.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_deadlock_absoluteness, empirical, 'Whether the suppression of alternatives is total or leaves a live bargaining margin.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__oligopoly_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__oligopoly_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(arti_tr_t10, article_27_veto_power__oligopoly_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(arti_tr_t20, article_27_veto_power__oligopoly_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(arti_tr_t30, article_27_veto_power__oligopoly_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(arti_tr_t40, article_27_veto_power__oligopoly_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(arti_tr_t50, article_27_veto_power__oligopoly_reading, theater_ratio, 50, 0.33).
narrative_ontology:measurement(arti_tr_t60, article_27_veto_power__oligopoly_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(arti_tr_t70, article_27_veto_power__oligopoly_reading, theater_ratio, 70, 0.44).
narrative_ontology:measurement(arti_tr_t80, article_27_veto_power__oligopoly_reading, theater_ratio, 80, 0.49).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__oligopoly_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(arti_be_t10, article_27_veto_power__oligopoly_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(arti_be_t20, article_27_veto_power__oligopoly_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(arti_be_t30, article_27_veto_power__oligopoly_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(arti_be_t40, article_27_veto_power__oligopoly_reading, base_extractiveness, 40, 0.69).
narrative_ontology:measurement(arti_be_t50, article_27_veto_power__oligopoly_reading, base_extractiveness, 50, 0.71).
narrative_ontology:measurement(arti_be_t60, article_27_veto_power__oligopoly_reading, base_extractiveness, 60, 0.73).
narrative_ontology:measurement(arti_be_t70, article_27_veto_power__oligopoly_reading, base_extractiveness, 70, 0.76).
narrative_ontology:measurement(arti_be_t80, article_27_veto_power__oligopoly_reading, base_extractiveness, 80, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_27_veto_power__oligopoly_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(arti_su_t10, article_27_veto_power__oligopoly_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(arti_su_t20, article_27_veto_power__oligopoly_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(arti_su_t30, article_27_veto_power__oligopoly_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(arti_su_t40, article_27_veto_power__oligopoly_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(arti_su_t50, article_27_veto_power__oligopoly_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement(arti_su_t60, article_27_veto_power__oligopoly_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(arti_su_t70, article_27_veto_power__oligopoly_reading, suppression_requirement, 70, 0.7).
narrative_ontology:measurement(arti_su_t80, article_27_veto_power__oligopoly_reading, suppression_requirement, 80, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__oligopoly_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__sovereignty_reading).

% DUAL FORMULATION NOTE:
% 'The P5 veto' is a colloquial label covering three structurally distinct claims with different epsilon, decomposed per the epsilon-invariance principle into a three-story constraint family. The coordination reading (upstream in citation practice: the P5 cite war-prevention as justification) carries low extraction if its empirical warrant holds. The sovereignty reading carries a consent-based warrant with a general victim set. This file is the oligopoly member: high extraction, victim = non-P5 majority, persistence via the Article 108 lock. Edges run from this story to both siblings: the coordination story supplies this one's cover narrative, and this story's delegitimation pressure shifts the sovereignty story's legitimacy conditions without foreclosing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
