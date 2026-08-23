% ============================================================================
% CONSTRAINT STORY: salic_prohibition__cognatic_reversion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__cognatic_reversion_reading, []).

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
 *   constraint_id: salic_prohibition__cognatic_reversion_reading
 *   human_readable: Salic Prohibition — Cognatic Reversion Reading (Frankish Anachronism Thesis)
 *   domain: constitutional/political-history
 *
 * SUMMARY:
 *   The Salic prohibition — the exclusion of women from royal succession —
 *   operated for five centuries in realms the original Frankish code never
 *   governed. This story instantiates the cognatic_reversion_reading of that
 *   kernel: the prohibition as a territorially bounded personal law of the
 *   Salian Franks, carried into France, Spain, and the German states by
 *   citation rather than command, and therefore never properly binding there.
 *   On this reading the standing arrangement under contest is the operated
 *   prohibition itself, and its warrant fails at the root: the code's
 *   inheritance clauses regulated private allodial land among Franks, the
 *   royal-succession use was constructed in the 1316–1328 extinction crises,
 *   and each later adoption followed a local crisis, not Frankish authority.
 *   The reading holds territorial integrity above agnatic purity and treats
 *   eldest-child primogeniture as the legitimate alternative. Constraint
 *   family: this is one of three readings of kernel salic_prohibition, linked
 *   via network.affects_constraints — the immutable_mandate_reading authors
 *   the same operated arrangement as irrevocable natural/divine law (maximal
 *   binding force), the sovereign_override_reading as revocable positive law
 *   (intermediate); this reading authors the lowest legitimate binding force
 *   and locates the arrangement's persistence entirely in enforcement and
 *   citation. Each file carries its own ε over the same referent; the ε
 *   spread across the family is the measurement the decomposition exists to
 *   take. KEY AGENTS (by structural relationship): -
 *   agnatic_reigning_dynasts: Primary beneficiary and co-administrator
 *   (institutional/identity_locked) — crowns arrive through the exclusion;
 *   they preside over its renewal - court_jurists_and_parlements:
 *   Administrator and secondary beneficiary (institutional/constrained) —
 *   hold the interpretive monopoly that certifies each settlement -
 *   cognatic_heirs: Primary internal target (powerful/trapped) — daughters'
 *   lines dispossessed despite superior blood-proximity -
 *   foreign_cognatic_claimants: External target-contestant (powerful/trapped)
 *   — neighboring crowns with no procedural standing inside the realm -
 *   succession_war_populations: Diffuse target (powerless/trapped) — bear the
 *   armed enforcement - royal_women_unconsulted: Excluded voice
 *   (powerless/trapped) — the rights-holders themselves, never seated -
 *   constitutional_historians: Analytical observer (analytical/analytical) —
 *   see the full jurisdictional structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, 0.58).
domain_priors:suppression_score(salic_prohibition__cognatic_reversion_reading, 0.45).
domain_priors:theater_ratio(salic_prohibition__cognatic_reversion_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__cognatic_reversion_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__cognatic_reversion_reading, "Salic Prohibition — Cognatic Reversion Reading (Frankish Anachronism Thesis)").
narrative_ontology:topic_domain(salic_prohibition__cognatic_reversion_reading, "constitutional/political-history").

domain_priors:requires_active_enforcement(salic_prohibition__cognatic_reversion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__cognatic_reversion_reading, 'a057490a-b6c0-49aa-8613-c8bed422b88e').
narrative_ontology:cs_kernel_codification('a057490a-b6c0-49aa-8613-c8bed422b88e', fixed_text).
narrative_ontology:cs_authority_grounding('a057490a-b6c0-49aa-8613-c8bed422b88e', lineage).
narrative_ontology:cs_interpretation_layer_present('a057490a-b6c0-49aa-8613-c8bed422b88e').
narrative_ontology:cs_reading_relation('a057490a-b6c0-49aa-8613-c8bed422b88e', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('a057490a-b6c0-49aa-8613-c8bed422b88e', salic_prohibition__sovereign_override_reading, influences).
narrative_ontology:cs_axiom('a057490a-b6c0-49aa-8613-c8bed422b88e', foundational, succession_custom_binds_only_within_its_own_polity).
narrative_ontology:cs_axiom_status(succession_custom_binds_only_within_its_own_polity, holdable).
narrative_ontology:cs_axiom_grounding('a057490a-b6c0-49aa-8613-c8bed422b88e', succession_custom_binds_only_within_its_own_polity, conventional).
narrative_ontology:cs_axiom('a057490a-b6c0-49aa-8613-c8bed422b88e', foundational, territorial_integrity_outweighs_agnatic_purity).
narrative_ontology:cs_axiom_status(territorial_integrity_outweighs_agnatic_purity, holdable).
narrative_ontology:cs_axiom_grounding('a057490a-b6c0-49aa-8613-c8bed422b88e', territorial_integrity_outweighs_agnatic_purity, instrumental).
narrative_ontology:cs_axiom('a057490a-b6c0-49aa-8613-c8bed422b88e', secondary, cognatic_primogeniture_is_valid_succession_rule).
narrative_ontology:cs_axiom_status(cognatic_primogeniture_is_valid_succession_rule, holdable).
narrative_ontology:cs_axiom_grounding('a057490a-b6c0-49aa-8613-c8bed422b88e', cognatic_primogeniture_is_valid_succession_rule, conventional).
narrative_ontology:cs_reference_frame('a057490a-b6c0-49aa-8613-c8bed422b88e', territorially_bounded_personal_law).
narrative_ontology:cs_drift_state('a057490a-b6c0-49aa-8613-c8bed422b88e', post_medieval_dynastic_expansion, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a057490a-b6c0-49aa-8613-c8bed422b88e', '').
narrative_ontology:cs_kernel_id(salic_prohibition__cognatic_reversion_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, agnatic_reigning_dynasts).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, court_jurists_and_parlements).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, cognatic_heirs).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, foreign_cognatic_claimants).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, succession_war_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collateral male-line branches — Valois, Bourbon, the Spanish and Neapolitan Bourbons, the Hanoverian Guelphs — whose crowns arrive through the exclusion of female lines. Once installed they preside over the councils, courts, and ceremonies that reaffirm the rule at each succession, and their prestige is staked on its permanence. Their title has no footing outside the agnatic framework: under seniority-by-blood other lines outrank them, so abandoning the framework dissolves their own claim.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, agnatic_reigning_dynasts, beneficiary,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__cognatic_reversion_reading, agnatic_reigning_dynasts, agenda_setter).

% Chancellors, parlementaires, legists, and court canonists who select the texts, reconcile them with canon and Roman law, and certify each settlement. Offices, consultative fees, and scholarly authority flow from holding the interpretive monopoly; public departure from the doctrine costs career and place. Across generations they built the citation chain that presents a Frankish private-inheritance clause as the immemorial law of kingdoms it never touched.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, court_jurists_and_parlements, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__cognatic_reversion_reading, court_jurists_and_parlements, beneficiary).

% Kings' daughters and their descendants — Jeanne of Navarre's line after 1316, Edward III's claim through Isabella of France, the Habsburg archduchesses before 1713 — whose proximity by blood outranks the installed branch but whose claim no tribunal inside the realm will entertain. Their routes are renunciation, marriage politics aimed at the next generation, or armed appeal from outside; within the legal order there is no procedure that restores their place.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, cognatic_heirs, payer,
    powerful, generational, trapped, continental).

% Neighboring crowns pressing descent-through-females claims — Plantagenet England after 1328, the German houses contesting the Austrian settlement of 1713–1740. They command armies, treasuries, and alliances, yet hold no procedural standing in the target realm's courts; pressing the claim means war, dropping it means extinguishing the line's pretension permanently. Generations of policy and revenue can be consumed by a grievance no chancery will adjudicate.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, foreign_cognatic_claimants, payer,
    powerful, biographical, trapped, continental).

% Peasants and townspeople of the districts where disputed successions are fought out — northern France during the Hundred Years' War, the Basque provinces and Catalonia in the Carlist wars. They choose no dynasty, inherit no claim, and bear conscription, requisition, and burning; their exit is flight, and flight forfeits land and livelihood.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, succession_war_populations, payer,
    powerless, immediate, trapped, regional).

% The princesses, queen-dowagers, and regents whose succession rights are allocated in assemblies, councils, and courts where they hold no seat. Consent is never sought; their interests are voiced by fathers, brothers, husbands, and uncles whose incentives diverge from theirs. Several — Anne of Kiev, Anne of Austria, the regent queens — wield real power in practice while remaining formally outside the very order that disposes of their rights.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, royal_women_unconsulted, excluded,
    powerless, biographical, trapped, national).

% Scholars comparing the manuscript recensions of the Frankish code, the 1316–1328 assembly records, and each kingdom's reception instruments. They can read the original inheritance clauses as regulating private allodial land among Franks, date the royal-succession use to fourteenth-century crisis management, and observe that every adoption followed a local extinction crisis rather than any Frankish command.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__cognatic_reversion_reading, agnatic_reigning_dynasts).
narrative_ontology:fixing_cost_class(salic_prohibition__cognatic_reversion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single publicly known order of precedence for royal succession, settling at each extinction crisis who reigns and forestalling simultaneous coronations, partitions, and interregna.
% TRANSFER_FUNCTION: Moves succession rights — and the crowns, domains, revenues, and marriage-alliance value attached to them — from daughters and their descendants to the nearest male-line relatives; princesses shift from transmission vectors of the crown to negotiating assets in alliance politics.
% ABSENT_VOICES: The royal daughters and dowagers themselves: no woman sat in the 1316–1328 assemblies, the Spanish councils that debated the 1713 adoption, or the cabinets that drafted pragmatic sanctions; their rights were allocated by male kin and clerks with divergent incentives. Also unrepresented: the rural populations of the war zones where disputed successions were ultimately decided by arms.
% DISAPPEARANCE_RATIONALE: Overnight removal reorders several thrones at once: in the 1328 counterfactual the French crown passes by blood-proximity rather than agnatic collaterality; Hanover remains joined to Britain after 1837; the Carlist claim never crystallizes into civil war; dynastic marriage strategy revalues princesses as heirs rather than instruments. The European state system's war map changes with them.
% FOUNDING_PROBLEM: After three consecutive Capetian kings died leaving only daughters (1316–1328), the realm needed a rule that would settle succession instantly and keep the crown out of foreign hands arriving through female descent.
% FOUNDING_PROBLEM_CORROBORATION: Assembly records and contemporary chroniclers of 1316–1328 corroborate the founding crisis itself, from outside all later beneficiary interests. Modern constitutional historians — who owe nothing to the dynasties — attest that the exclusion-specific problem dissolved with constitutional succession, while legitimist claimants alone maintain it is live; no disinterested party attests the founding problem in its original form as still live.
narrative_ontology:disappearance_verdict(salic_prohibition__cognatic_reversion_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__cognatic_reversion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__cognatic_reversion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(salic_prohibition__cognatic_reversion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__cognatic_reversion_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__cognatic_reversion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__cognatic_reversion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58 at interval end, peaking 0.74 in the 1830 crisis) tracks the gap between the rule's warrant and its operation: in every realm it governed, the warrant was borrowed from a code that never legislated there, so the dispossession it produced rested on citation rather than command. Suppression (0.45 at end, peaking 0.80 in the Hundred Years' War phase) measures the enforcement machinery — juristic orthodoxy, court certification, and twice open war — needed to hold the rule against blood-proximity alternatives that never stopped existing; suppression is authored as a raw structural property and is not scaled by power or scope. Theater (0.52 at end, rising from 0.25) grows as the antiquity claim thickens: by the Spanish adoption of 1713 the doctrine is presented as immemorial in a peninsula the Frankish code never reached, and by the 1830s two Spanish parties cite the same texts against each other. Accessibility collapse is moderate (0.45): cognatic and elective alternatives remained visibly operable in neighboring realms throughout. Resistance is high (0.68): the rule met armed challenge within living memory of each installation. All three series share one time grid. The trajectories oscillate on a crisis cycle — each dynastic extinction (1316–28, 1589, 1713–40, 1830s) spikes enforcement and extraction, and the spike itself re-legitimates the rule by deploying it: an intermittent-reinforcement pattern in which the crisis is the mechanism, not noise. Coalition note: the dispossessed seats occasionally aligned — external claimants funding internal heirs, war-wearied provinces backing pragmatic settlements — and each alignment produced a crack (the 1789 Spanish revocation, the 1830 Pragmatic Sanction) that the next crisis sealed shut again.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural data. From the installed dynasties' seat the rule is the constitution itself — the thing that made their reigns possible and orderly; from the jurists' seat it is a working interpretive monopoly; from the internal cognatic heirs' seat it is a closed door with no handle; from the external claimants' seat it is a tribunal void backed by battlefields; from the war-zone populations' seat it is pure cost with no benefit stream at any point. Two same-power seats stand in direct opposition — foreign_cognatic_claimants and agnatic_reigning_dynasts both hold institutional-grade power with opposed positions — so contention between them is derivable rather than asserted. Identity-lock runs deepest on the dynastic seat: the title and the doctrine are one possession, and a dynast who concedes the jurisdictional critique concedes his own crown; break that fusion and the beneficiary seat converts to a negotiator overnight.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the installed dynasties and the interpretive colleges sit near the subsidized end (low d), their position cheapened by identity_locked and constrained exits. The three payer groups sit near the target end (high d): internal heirs and external claimants are trapped — no in-framework procedure restores them — and trapped targets register nearer full-target than mobile ones would; war-zone populations are powerless and trapped, the extreme case. Scope amplification applies modestly: the arrangement operated at continental scale across multiple realms, where verification of the warrant's validity was weakest. No directionality overrides are declared: the beneficiary/victim data plus exit options already separate the seats cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — instantaneous arbitration of extinction crises — was real and repeatedly solved: five centuries of successions turned on the rule without simultaneous coronations. But the problem the rule was built to solve dissolved when constitutional succession displaced dynastic arbitration; what persists is citation without command — legitimist pretenders maintaining exclusions that no longer allocate anything. Declaring the mandate resolved keeps the classification from two opposite errors: reading the arrangement as pure coordination erases the dispossession of every daughter's line that paid for the determinacy; reading it as pure predation erases the genuine arbitration service rendered at each crisis. The honest structure is both-at-once: a working succession machine whose fuel was other people's inheritance, running on after the machine stopped being needed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_binding_source_location,
    'Where do the three readings of kernel salic_prohibition locate the prohibition''s binding force — divine/natural command (immutable_mandate_reading), sovereign enactment (sovereign_override_reading), or bounded Frankish custom void abroad (this reading) — and which location governs a given realm?',
    'Comparative analysis of each realm''s reception instruments — the French assembly records of 1316–1328, the Spanish auto of 1713, the Hanoverian house law — scored against each reading''s criteria for what counts as binding.',
    'Decides which sibling constraint is live for each realm. If reception was always local and improvised, this reading''s jurisdictional-void thesis holds and the immutable reading loses its object; if any realm enacted the prohibition as universal obligation, the immutable reading gains standing there.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_binding_source_location, conceptual, 'Location of the inter-reading disagreement: the source and scope of bindingness.').

omega_variable(
    original_clause_scope,
    'Did the transmitted Frankish code''s inheritance clause ever address royal succession, or is the royal-succession use a fourteenth-century juristic construction layered onto a private allodial-land rule?',
    'Philological comparison of manuscript recensions against the earliest royal-succession citations, dating the interpretive move that converted a private-inheritance clause into a crown rule.',
    'Confirms or refutes the anachronism thesis at the root. A late construction collapses the antiquity cover, raises the theater share of the operated period, and pressures reassessment of the operated arrangement toward pure extraction; a genuine early succession clause would partially restore the warrant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_clause_scope, empirical, 'Whether the royal-succession use of the Frankish code is original or constructed.').

omega_variable(
    reception_mode_spain,
    'Did Spain''s 1713 adoption constitute fresh positive-law enactment by the sovereign, or purported recognition of pre-existing binding custom?',
    'Reading the Auto acordado and the council deliberations surrounding it for whether the drafters treat the prohibition as new legislation or as received law being confirmed.',
    'If fresh enactment, the prohibition''s Iberian force rests on sovereign act rather than Frankish warrant — narrowing this reading''s disagreement with the sovereign_override_reading to chronology and strengthening the override reading''s account even within this story''s frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reception_mode_spain, empirical, 'Mode of the Spanish reception: enactment or recognition.').

omega_variable(
    coordination_equivalence_counterfactual,
    'Would cognatic primogeniture have delivered comparable succession determinacy in the Capetian, Habsburg, and Bourbon settings, or did the sex exclusion purchase determinacy that a non-discriminating rule could not?',
    'Comparative frequency of succession wars and contested accessions across realms operating cognatic or elective rules over the same period, controlling for fragmentation and external pressure.',
    'If equivalence holds, the prohibition''s coordination contribution is separable from its sex exclusion and the extraction stands nearly bare; if not, part of the measured extraction is the price of the determinacy actually purchased, and the tangled-rope reading of the operated period firms up.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_equivalence_counterfactual, empirical, 'Whether the coordination function required the exclusion or was achievable without it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__cognatic_reversion_reading, 1316, 1876).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(salic_cog_rev_tr_t1316, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1316, 0.25).
narrative_ontology:measurement(salic_cog_rev_tr_t1328, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1328, 0.35).
narrative_ontology:measurement(salic_cog_rev_tr_t1400, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1400, 0.4).
narrative_ontology:measurement(salic_cog_rev_tr_t1461, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1461, 0.48).
narrative_ontology:measurement(salic_cog_rev_tr_t1589, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1589, 0.5).
narrative_ontology:measurement(salic_cog_rev_tr_t1713, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1713, 0.55).
narrative_ontology:measurement(salic_cog_rev_tr_t1789, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1789, 0.58).
narrative_ontology:measurement(salic_cog_rev_tr_t1830, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1830, 0.6).
narrative_ontology:measurement(salic_cog_rev_tr_t1837, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1837, 0.57).
narrative_ontology:measurement(salic_cog_rev_tr_t1876, salic_prohibition__cognatic_reversion_reading, theater_ratio, 1876, 0.52).

% Extraction over time
narrative_ontology:measurement(salic_cog_rev_be_t1316, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1316, 0.45).
narrative_ontology:measurement(salic_cog_rev_be_t1328, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1328, 0.6).
narrative_ontology:measurement(salic_cog_rev_be_t1400, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1400, 0.68).
narrative_ontology:measurement(salic_cog_rev_be_t1461, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1461, 0.63).
narrative_ontology:measurement(salic_cog_rev_be_t1589, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1589, 0.66).
narrative_ontology:measurement(salic_cog_rev_be_t1713, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1713, 0.7).
narrative_ontology:measurement(salic_cog_rev_be_t1789, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1789, 0.64).
narrative_ontology:measurement(salic_cog_rev_be_t1830, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1830, 0.74).
narrative_ontology:measurement(salic_cog_rev_be_t1837, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1837, 0.71).
narrative_ontology:measurement(salic_cog_rev_be_t1876, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 1876, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(salic_cog_rev_su_t1316, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1316, 0.4).
narrative_ontology:measurement(salic_cog_rev_su_t1328, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1328, 0.55).
narrative_ontology:measurement(salic_cog_rev_su_t1400, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1400, 0.8).
narrative_ontology:measurement(salic_cog_rev_su_t1461, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1461, 0.65).
narrative_ontology:measurement(salic_cog_rev_su_t1589, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1589, 0.7).
narrative_ontology:measurement(salic_cog_rev_su_t1713, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1713, 0.6).
narrative_ontology:measurement(salic_cog_rev_su_t1789, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1789, 0.5).
narrative_ontology:measurement(salic_cog_rev_su_t1830, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1830, 0.78).
narrative_ontology:measurement(salic_cog_rev_su_t1837, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1837, 0.62).
narrative_ontology:measurement(salic_cog_rev_su_t1876, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 1876, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__cognatic_reversion_reading, resource_allocation).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__sovereign_override_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Salic Law' covers three structurally distinct claims about one operated arrangement, decomposed per the ε-invariance principle. This file (cognatic_reversion_reading) authors the lowest legitimate binding force — the prohibition as jurisdictionally void outside Frankish territory — and therefore attributes its entire operated persistence to enforcement and citation. The immutable_mandate_reading authors maximal binding force (natural/divine law); the sovereign_override_reading authors intermediate binding force (revocable positive law). All three stories assess the same referent — the prohibition as operated in non-Frankish realms — with reading-indexed ε values; the ε spread across the family is the corpus measurement. Upstream/downstream: the immutable reading's antiquity claims are the material this reading attacks; this reading's jurisdictional critique is the ammunition the override reading uses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
