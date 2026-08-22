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
 *   human_readable: Permanent Five Veto as Entrenched Geopolitical Oligopoly (Oligopoly Reading of Article 27)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   Five states hold permanent, individually decisive power over Security
 *   Council action on international peace and security, and the same five
 *   must each concur to amend the Charter provisions that grant it (Articles
 *   27 and 108). Eighty years of membership growth from 51 to 193 states,
 *   decolonization, and the redistribution of economic and demographic weight
 *   have left the 1945 seating chart untouched. This file instantiates ONE
 *   reading of the contested Article 27 kernel — the oligopoly_reading, which
 *   treats the arrangement as a self-protecting oligopoly: the beneficiaries
 *   of the allocation are the only agents who can lawfully alter it, and
 *   every reform channel terminates in their consent. Sibling readings
 *   (coordination_reading, sovereignty_reading) are separate constraint files
 *   linked through network.affects_constraints; per the epsilon-invariance
 *   rule this story authors a single stable epsilon for the standing
 *   arrangement as this reading assesses it. The claimed_type is authored
 *   independently of the metrics, which describe the arrangement's observed
 *   operation.
 *
 * KEY AGENTS:
 *   - permanent_five_members: Primary beneficiary and agenda-setter (institutional power, arbitrage exit) — collects continuous authority advantages and controls the only lawful amendment path
 *   - non_p5_member_states: Primary target (organized power, trapped exit) — bears subordination and funds the system with no reform route
 *   - elected_council_members: Secondary target with incidental gains (moderate power, constrained exit)
 *   - g4_aspirant_states: Excluded challenger (powerful, constrained exit) — barred from the seating decision itself
 *   - un_secretariat: Constrained administrator (institutional power, constrained exit) — runs the machine under the incumbents' blocking shadow
 *   - institutional_design_analysts: Analytical observer — sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, 0.77).
domain_priors:suppression_score(article_27_veto_power__oligopoly_reading, 0.84).
domain_priors:theater_ratio(article_27_veto_power__oligopoly_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, extractiveness, 0.77).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, suppression_requirement, 0.84).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__oligopoly_reading, snare).
narrative_ontology:human_readable(article_27_veto_power__oligopoly_reading, "Permanent Five Veto as Entrenched Geopolitical Oligopoly (Oligopoly Reading of Article 27)").
narrative_ontology:topic_domain(article_27_veto_power__oligopoly_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:requires_active_enforcement(article_27_veto_power__oligopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__oligopoly_reading, 'cf2a7e0c-7980-4f85-9444-3a28a7932d42').
narrative_ontology:cs_kernel_codification('cf2a7e0c-7980-4f85-9444-3a28a7932d42', fixed_text).
narrative_ontology:cs_authority_grounding('cf2a7e0c-7980-4f85-9444-3a28a7932d42', extraction).
narrative_ontology:cs_interpretation_layer_present('cf2a7e0c-7980-4f85-9444-3a28a7932d42').
narrative_ontology:cs_reading_relation('cf2a7e0c-7980-4f85-9444-3a28a7932d42', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf2a7e0c-7980-4f85-9444-3a28a7932d42', article_27_veto_power__sovereignty_reading, influences).
narrative_ontology:cs_axiom('cf2a7e0c-7980-4f85-9444-3a28a7932d42', foundational, veto_entrenches_p5_oligopoly).
narrative_ontology:cs_axiom_status(veto_entrenches_p5_oligopoly, holdable).
narrative_ontology:cs_axiom_grounding('cf2a7e0c-7980-4f85-9444-3a28a7932d42', veto_entrenches_p5_oligopoly, empirically_contingent).
narrative_ontology:cs_axiom('cf2a7e0c-7980-4f85-9444-3a28a7932d42', secondary, amendment_rule_locks_incumbent_advantage).
narrative_ontology:cs_axiom_status(amendment_rule_locks_incumbent_advantage, holdable).
narrative_ontology:cs_axiom_grounding('cf2a7e0c-7980-4f85-9444-3a28a7932d42', amendment_rule_locks_incumbent_advantage, conventional).
narrative_ontology:cs_reference_frame('cf2a7e0c-7980-4f85-9444-3a28a7932d42', yalta_concert_privilege_bargain).
narrative_ontology:cs_drift_state('cf2a7e0c-7980-4f85-9444-3a28a7932d42', contemporary_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cf2a7e0c-7980-4f85-9444-3a28a7932d42', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__oligopoly_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, permanent_five_members).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, non_p5_member_states).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, elected_council_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, elected_council_members).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, un_secretariat).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent seats on the Security Council with unilateral power to block any substantive resolution. Their concurrence is also required for any amendment to the Charter, so the rule allocating them this authority can only be changed with their own consent. They shape the Council's working agenda through the penholder system, shield themselves and allies from binding Council action, and gatekeep the selection of the Secretary-General. When Council paths close against their interests, they operate through national militaries, ad hoc coalitions, or parallel institutions, so remaining inside the arrangement costs them little.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, permanent_five_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, permanent_five_members, beneficiary).

% Supply the organization's budget, troops, and diplomatic labor while holding no permanent seat and no blocking power. Formally equal under the Charter, they are structurally second-tier on peace and security: any resolution touching a permanent member's core interests fails regardless of how the rest vote. Every reform avenue — working-group proposals, framework resolutions, code-of-conduct pledges — terminates in an amendment procedure that requires the incumbents' concurrence. Leaving the organization would forfeit its legal protections and legitimacy, and no rival body carries equivalent force.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, non_p5_member_states, payer,
    organized, generational, trapped, global).

% Win two-year seats through competitive Assembly elections, gaining prestige, diplomatic access, and a hand in drafting resolutions and chairing committees. On questions the permanent members treat as core interests, their votes count for nothing: drafts die regardless of the elected members' unanimous support. They carry the Council's workload and its public accountability while holding no power over its outcomes on the questions that matter most.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, elected_council_members, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, elected_council_members, beneficiary).

% Campaign openly for permanent seats commensurate with their current population, economic weight, and contributions — a bid sustained for two decades. Their admission would dilute the incumbents' relative authority, and the amendment procedure lets the incumbents decline indefinitely. They participate fully in the organization otherwise; what they cannot do is enter the room where the seating chart is decided.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, g4_aspirant_states, excluded,
    powerful, biographical, constrained, global).

% Administers the organization day to day, but its head is effectively chosen by the permanent members — any candidate one of them rejects is dropped before the Assembly ever votes. Peacekeeping mandates, budget priorities, and the Secretary-General's public positions all operate in the shadow of the blocking power. Independence is exercised at the margins the permanent members tolerate.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, un_secretariat, payer,
    institutional, biographical, constrained, global).

% Study the arrangement from outside: documenting voting patterns, tracing which draft resolutions fail and why, comparing the 1945 settlement's terms against the current distribution of power and membership, and publishing assessments of reform proposals. They hold no vote and bear no costs; their output informs delegations, courts, and reform coalitions.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, institutional_design_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__oligopoly_reading, permanent_five_members).
narrative_ontology:fixing_cost_class(article_27_veto_power__oligopoly_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates final decision authority over peace-and-security action in five states and guarantees that no binding Council decision can issue over any of their objections; by the same guarantee, keeps each of the five inside the institutional framework they could otherwise defy or abandon.
% TRANSFER_FUNCTION: Moves decision authority and agenda control from the general membership — 193 states voting equally in the Assembly — to five permanent holders; moves immunity from binding Council action to the five and their protected associates; moves appointment gatekeeping (Secretary-General selection, judicial recommendations) and control over the Charter text itself into the five's hands.
% ABSENT_VOICES: The four-nation aspirant bloc and the African Union's common position would object that a 1945 power map governs a 2025 organization; they sit outside the amendment conversation because the conversation's decision rule belongs to the incumbents. Populations in territories where Council action is blocked have no seat anywhere in the process. Small states without coalition membership are present in the Assembly but absent where the outcome is decided.
% DISAPPEARANCE_RATIONALE: Overnight removal converts the Council into a majoritarian body in which the five can be outvoted and bound. Either the five comply — transforming the organization's character and redistributing decision authority across the membership — or they defect and build or fund parallel arrangements, as the League's condemned members did. Either branch rearranges the current allocation of authority, which is precisely what this reading identifies as the arrangement's continuing yield; nothing about the present system survives the removal intact.
% FOUNDING_PROBLEM: After the League of Nations collapsed when condemned great powers walked out (Japan 1933, Italy 1935, Germany earlier), the 1945 founders faced the problem of designing a security body the strongest states would join and stay inside; the concession was explicit permanent privilege, accepted by the smaller states as the price of great-power participation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the League defection record (1931-1939) independently attests the founding problem was real. Its present status is disputed along the same line: the permanent five attest that retention still requires privilege; the Assembly's recurring reform supermajorities (framework resolutions since 1993, the 2022 post-veto debate mechanism) attest that most members judge the current terms no longer justified. No source outside the five attests that the founding problem still requires this particular allocation.
narrative_ontology:disappearance_verdict(article_27_veto_power__oligopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__oligopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__oligopoly_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_27_veto_power__oligopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__oligopoly_reading, 0.77, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.77: the transfer is continuous rather than episodic — agenda control through the penholder system, immunity of the five from binding Council action, gatekeeping of the Secretary-Generalship and of the Charter text itself — and it scales with the organization's growing responsibilities. Suppression 0.84: the arrangement's distinctive feature is that the exit from it (amendment) runs through the beneficiaries' own consent, making suppression near-total on the reform axis; on the action axis it is partial, since Assembly emergency sessions, regional bodies, and ad hoc coalitions give members costly but real substitutes. Theater ratio 0.32: genuine security management occurs (peacekeeping mandates, sanctions on unprotected actors), but a rising share of Council activity is positional — drafts tabled to be blocked, debates staged for audiences. Accessibility collapse 0.60: once the structure is understood, substitutes collapse substantially yet never fully, because none carry Chapter VII binding force. Resistance 0.70: eight decades of organized counter-pressure — the 1950 Assembly resolution on deadlocks threatening the peace, the open-ended working group sitting since 1993, the 2015 code-of-conduct pledge now covering over a hundred states, the 2022 Assembly mechanism obligating debate after every blocking vote. Coalition check: the victims are not isolated — they have built durable coalitions (the four-nation aspirant bloc, the uniting-for-consensus group, the continental common position) — and the coalitions nonetheless fail, because the decision rule converts majority power into nothing at the amendment stage; this is why the trap holds despite organized victims. Suppression is authored as a raw structural property; only extractiveness is scaled downstream, by directionality and global scope.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently. From the permanent-five seat the arrangement is a founding asset defended at modest cost: they face no compulsion they did not author, and their arbitrage exit (national action, coalitions, parallel institutions) means the arrangement constrains them almost nowhere. From the non-permanent majority's seat the same structure is unaccountable subordination with no lawful exit and no lawful amendment. From the elected-member seat it is responsibility without power — real work, public accountability, zero effect on core outcomes. From the secretariat's seat it is a ceiling on independence set by others' blocking power. The engine derives these divergent per-seat classifications from the structural data; the divergence between the agenda-setter seat and the trapped payer seats is the perspectival content of this story.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. The permanent five are declared beneficiaries with arbitrage-grade exit: directionality sits near the subsidized pole — the arrangement pays them authority continuously and charges them almost nothing. The non-permanent majority are declared victims with trapped exit: directionality sits near the full-target pole — they fund and staff the system, absorb subordination, and can neither leave nor amend. Elected council members are declared victims with a secondary beneficiary position: high directionality damped by real prestige and access gains. The aspirant states are excluded rather than coordinated — blocked from the benefit side entirely, hence near-full target. The secretariat is a payer whose institutional power does not protect it, because its independence is bounded by the same blocking power. Global spatial scope amplifies effective extraction for target seats by raising verification costs across 193 jurisdictions; suppression, by contrast, enters the computation unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping the great powers inside a collective security body after the League's defection collapse — was real and is corroborated from outside the benefiting parties by the League record itself. Whether it remains live is contested: the five attest that retention still requires privilege; the reform majority attests that the price now exceeds the benefit and that retention could be purchased differently. The classification guards against two mislabels. Reading the arrangement as pure coordination (the sibling coordination file's claim) would erase the asymmetric transfer that the beneficiary/victim structure documents; reading it as pure extraction with no coordination function would erase the retention service the mechanism demonstrably performs for the five's continued participation. The temporal series tracks the obsolescence risk directly: extraction rises across the interval while the founding problem's status stays contested — if the retention problem dies (great powers no longer positioned or willing to defect) while the transfer persists, the arrangement completes the transition from priced bargain to unpriced entitlement, and the rising base-extractiveness trajectory is the early signature of exactly that drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This file instantiates only the oligopoly_reading of kernel article_27_veto_power; the coordination_reading and sovereignty_reading are separate constraints with their own epsilon and beneficiary structures. Where exactly is the disagreement between the readings located?',
    'Structural comparison across the three family files: the disagreement is located in the attribution of the arrangement''s primary function (war-prevention coordination versus authority-transfer versus consent-principle instantiation), which determines whose costs count in the epsilon assessment.',
    'Under the coordination_reading the same mechanism computes near the coordination-cost floor and classifies toward rope; under the sovereignty_reading the victim set thins to coerced minorities and the arrangement reads as principled application of consent. Only the oligopoly_reading yields the present victim-majority structure and high epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer frame: one reading of a three-reading kernel; the contest is located in primary-function attribution.').

omega_variable(
    counterfactual_retention_baseline,
    'What would the five permanent members do absent the blocking power — comply with majoritarian decisions, or defect to parallel frameworks as the League''s condemned members did?',
    'Comparative institutional history: the League defection sequence of 1931-1939; observed permanent-member behavior in bodies where they can be outvoted (Assembly votes they lose, jurisdictions they decline); revealed preference when Council action binds them at the margins.',
    'If defection is the realistic counterfactual, part of the measured transfer is the price of great-power retention and net extraction falls; if the five would largely comply, the transfer approaches pure incumbent advantage and epsilon rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_retention_baseline, empirical, 'Whether the arrangement''s transfer reflects retention cost or pure incumbent advantage.').

omega_variable(
    reform_blockage_attribution,
    'Is the eighty-year absence of Charter reform attributable to active incumbent defense alone, or also to division within the non-permanent majority (rival aspirant and uniting-for-consensus blocs, regional seat disputes)?',
    'Roll-call analysis of Assembly reform votes (consistent supermajorities for framework resolutions since 1993) set against the unanimity requirement at the amendment stage; textual analysis of incumbent statements blocking specific models.',
    'If the incumbents alone block, suppression is external and concentrated and the present classification strengthens; if majority fragmentation shares causation, part of the persistence is ordinary coordination failure and effective suppression on the reform axis is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_blockage_attribution, empirical, 'Attribution of reform deadlock between incumbent defense and majority fragmentation.').

omega_variable(
    incumbent_defense_motivation,
    'Do the permanent members maintain the arrangement primarily for material advantages (agenda control, immunity, appointment gatekeeping) or for status identity (great-power recognition constituted by the seat itself)?',
    'Behavioral test: would the five accept a reformed Council preserving equivalent material safeguards without permanent status, or equivalent status without material safeguards? Observed negotiating red lines across the 2005 summit and subsequent intergovernmental negotiations.',
    'Material motivation predicts tradeable demands and eventual bargained reform; status motivation predicts refusal of any dilution regardless of compensation, hardening the persistence outlook and raising long-run extraction-accumulation risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_defense_motivation, conceptual, 'Interest versus identity basis of incumbent defense of the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__oligopoly_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_27_veto_power__oligopoly_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(arti_tr_t1955, article_27_veto_power__oligopoly_reading, theater_ratio, 1955, 0.22).
narrative_ontology:measurement(arti_tr_t1965, article_27_veto_power__oligopoly_reading, theater_ratio, 1965, 0.28).
narrative_ontology:measurement(arti_tr_t1975, article_27_veto_power__oligopoly_reading, theater_ratio, 1975, 0.33).
narrative_ontology:measurement(arti_tr_t1985, article_27_veto_power__oligopoly_reading, theater_ratio, 1985, 0.36).
narrative_ontology:measurement(arti_tr_t1995, article_27_veto_power__oligopoly_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(arti_tr_t2005, article_27_veto_power__oligopoly_reading, theater_ratio, 2005, 0.27).
narrative_ontology:measurement(arti_tr_t2015, article_27_veto_power__oligopoly_reading, theater_ratio, 2015, 0.31).
narrative_ontology:measurement(arti_tr_t2025, article_27_veto_power__oligopoly_reading, theater_ratio, 2025, 0.35).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_27_veto_power__oligopoly_reading, base_extractiveness, 1945, 0.55).
narrative_ontology:measurement(arti_be_t1955, article_27_veto_power__oligopoly_reading, base_extractiveness, 1955, 0.58).
narrative_ontology:measurement(arti_be_t1965, article_27_veto_power__oligopoly_reading, base_extractiveness, 1965, 0.63).
narrative_ontology:measurement(arti_be_t1975, article_27_veto_power__oligopoly_reading, base_extractiveness, 1975, 0.66).
narrative_ontology:measurement(arti_be_t1985, article_27_veto_power__oligopoly_reading, base_extractiveness, 1985, 0.68).
narrative_ontology:measurement(arti_be_t1995, article_27_veto_power__oligopoly_reading, base_extractiveness, 1995, 0.64).
narrative_ontology:measurement(arti_be_t2005, article_27_veto_power__oligopoly_reading, base_extractiveness, 2005, 0.7).
narrative_ontology:measurement(arti_be_t2015, article_27_veto_power__oligopoly_reading, base_extractiveness, 2015, 0.74).
narrative_ontology:measurement(arti_be_t2025, article_27_veto_power__oligopoly_reading, base_extractiveness, 2025, 0.77).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_27_veto_power__oligopoly_reading, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(arti_su_t1955, article_27_veto_power__oligopoly_reading, suppression_requirement, 1955, 0.62).
narrative_ontology:measurement(arti_su_t1965, article_27_veto_power__oligopoly_reading, suppression_requirement, 1965, 0.66).
narrative_ontology:measurement(arti_su_t1975, article_27_veto_power__oligopoly_reading, suppression_requirement, 1975, 0.64).
narrative_ontology:measurement(arti_su_t1985, article_27_veto_power__oligopoly_reading, suppression_requirement, 1985, 0.62).
narrative_ontology:measurement(arti_su_t1995, article_27_veto_power__oligopoly_reading, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement(arti_su_t2005, article_27_veto_power__oligopoly_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(arti_su_t2015, article_27_veto_power__oligopoly_reading, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(arti_su_t2025, article_27_veto_power__oligopoly_reading, suppression_requirement, 2025, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__oligopoly_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'P5 veto'. The label covers three structurally distinct claims with materially different epsilon: war-prevention coordination (low extraction, near the coordination-cost floor), consent-principle instantiation (victim set limited to coerced minorities), and oligopoly entrenchment (this file; victim set is the non-permanent majority; high extraction). Per the epsilon-invariance principle each claim is authored separately and linked here. Citation flow runs from the coordination and sovereignty framings — which supply the arrangement's public justifications — into this reading, which treats those justifications as the arrangement's cover; correspondingly, this reading exerts an influences-type downstream pressure on the sovereignty sibling by exposing Charter immutability as self-interested, changing the conditions under which the pure-consent claim persuades without logically eliminating it. The relationship to the coordination sibling is coexistence: both remain live positions held by different parties, and a holder of either can acknowledge the other's observation while denying its primacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
