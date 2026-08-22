% ============================================================================
% CONSTRAINT STORY: constitutional_text__legislative_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__legislative_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__legislative_sovereignty_reading
 *   human_readable: Legislative Supremacy over Constitutional Meaning (Notwithstanding/Override Model)
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   In Westminster-family constitutions — the UK Human Rights Act's
 *   declarations of incompatibility, the Canadian Charter's section 33
 *   notwithstanding clause, the New Zealand Bill of Rights Act's
 *   non-supremacy design — the constitutional text and its operating practice
 *   establish the elected chamber as final arbiter of constitutional meaning:
 *   courts advise, flag incompatibilities, and persuade, but the legislature
 *   holds the last word through override clauses or simple insistence. This
 *   file instantiates ONE reading of the kernel constitutional_text, namely
 *   legislative_sovereignty_reading. The sibling readings
 *   (judicial_supremacy_reading, popular_sovereignty_reading) are separate
 *   constraints with their own epsilon values, beneficiary structures, and
 *   victim sets; per the epsilon-invariance principle they are not averaged,
 *   hedged, or described inside this story. The claim/metric independence
 *   rule is honored: claimed_type tangled_rope is my structural belief (a
 *   genuine coordination function joined to asymmetric extraction under
 *   active enforcement), and the metrics are authored as descriptively true
 *   of the arrangement's actual operation — the engine computes per-seat
 *   classifications from the structural data, and any divergence between
 *   claim and computed type is the measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - - governing_parliamentary_majorities: Agenda setter (powerful/arbitrage) — sets constitutional meaning by override or insistence, collects the override power directly
 *   - - apex_advisory_judiciary: Subordinated interpreter (institutional/identity_locked) — advises but cannot bind; bears subordination while collecting deference and persuasive influence
 *   - - minority_rights_communities: Primary target (powerless/trapped) — bears the loss of the judicial backstop
 *   - - opposition_minority_legislators: Rotational bearer (moderate/constrained) — pays while out of power, inherits the gains in office
 *   - - electoral_majorities: Diffuse beneficiary (organized/mobile) — receives responsive government, carries rotational exposure to future hostile majorities
 *   - - human_rights_advocacy_networks: Excluded voice (organized/mobile) — would demand entrenched judicial review, holds no formal seat
 *   - - comparative_constitutional_scholars: Analytical observer — documents the drift between restraint conventions and override practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, 0.42).
domain_priors:suppression_score(constitutional_text__legislative_sovereignty_reading, 0.36).
domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0.36).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__legislative_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__legislative_sovereignty_reading, "Legislative Supremacy over Constitutional Meaning (Notwithstanding/Override Model)").
narrative_ontology:topic_domain(constitutional_text__legislative_sovereignty_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__legislative_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__legislative_sovereignty_reading, '45a186bc-8148-40c2-906c-c41f445b087a').
narrative_ontology:cs_kernel_codification('45a186bc-8148-40c2-906c-c41f445b087a', fixed_text).
narrative_ontology:cs_authority_grounding('45a186bc-8148-40c2-906c-c41f445b087a', lineage).
narrative_ontology:cs_interpretation_layer_present('45a186bc-8148-40c2-906c-c41f445b087a').
narrative_ontology:cs_reading_relation('45a186bc-8148-40c2-906c-c41f445b087a', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('45a186bc-8148-40c2-906c-c41f445b087a', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('45a186bc-8148-40c2-906c-c41f445b087a', foundational, elected_chamber_conclusive_interpretive_authority).
narrative_ontology:cs_axiom_status(elected_chamber_conclusive_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('45a186bc-8148-40c2-906c-c41f445b087a', elected_chamber_conclusive_interpretive_authority, conventional).
narrative_ontology:cs_axiom('45a186bc-8148-40c2-906c-c41f445b087a', foundational, accountability_beats_insulation_for_meaning).
narrative_ontology:cs_axiom_status(accountability_beats_insulation_for_meaning, holdable).
narrative_ontology:cs_axiom_grounding('45a186bc-8148-40c2-906c-c41f445b087a', accountability_beats_insulation_for_meaning, instrumental).
narrative_ontology:cs_reference_frame('45a186bc-8148-40c2-906c-c41f445b087a', westminster_accountable_chamber_supremacy).
narrative_ontology:cs_drift_state('45a186bc-8148-40c2-906c-c41f445b087a', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('45a186bc-8148-40c2-906c-c41f445b087a', '').
narrative_ontology:cs_kernel_id(constitutional_text__legislative_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, governing_parliamentary_majorities).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, electoral_majorities).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, minority_rights_communities).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, opposition_minority_legislators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, apex_advisory_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, opposition_minority_legislators).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, apex_advisory_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commands the chamber and decides when to invoke the notwithstanding clause or dismiss a declaration of incompatibility. Writes the constitutional meaning it prefers into ordinary statute, answers to no bench in doing so, and can amend the constitutional framework itself within existing amendment rules. Collects the override power directly and bears no comparable cost while in office.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, governing_parliamentary_majorities, agenda_setter,
    powerful, biographical, arbitrage, national).

% Voters whose preferences become law without judicial second-guessing; they receive responsive government and short feedback loops between preference and policy. Their exposure is rotational: a future majority they oppose inherits the same unchecked power over constitutional meaning, and their own preferred protections hold only while their side wins.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, electoral_majorities, beneficiary,
    organized, immediate, mobile, national).

% Issues advisory opinions and declarations of incompatibility that flag conflicts between statute and constitutional commitments but cannot strike legislation down. Retains interpretive influence through persuasion, precedent-writing, and the government's need for legal legitimacy. Has internalized the advisory role as professional identity — the dialogue framing and deference norms are now part of how the bench understands itself; exit would mean repudiating its own settled self-conception or leaving the jurisdiction.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, apex_advisory_judiciary, payer,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, apex_advisory_judiciary, beneficiary).

% Depend on a judicial backstop that this arrangement removes; their protections hold only while sitting majorities choose restraint. Cannot relocate cheaply, cannot access a forum that can bind the legislature, and are left with electoral persuasion, coalition-building, and public argument as their remaining levers.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, minority_rights_communities, payer,
    powerless, generational, trapped, national).

% Out-of-power legislators whose readings of the constitution lose whenever the governing majority overrides or simply proceeds. They bear the arrangement's costs while out of office and inherit its full benefits when they win — which makes leaving the chamber irrational even when the arrangement defeats them session after session.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, opposition_minority_legislators, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, opposition_minority_legislators, beneficiary).

% Transnational advocacy organizations pressing for entrenched judicial review and litigable rights instruments. The framework assigns them no formal seat: their objections register only as political pressure, litigation in foreign or supranational fora, and reputational campaigns. They operate across jurisdictions and redirect effort toward stronger-review systems when domestic channels close.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, human_rights_advocacy_networks, excluded,
    organized, generational, mobile, continental).

% Document the gap between restraint conventions and actual override practice across Westminster-family systems, classify weak-form review against strong-form alternatives, and publish the drift record. Hold no stake in outcomes beyond analytic standing.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__legislative_sovereignty_reading, governing_parliamentary_majorities).
narrative_ontology:fixing_cost_class(constitutional_text__legislative_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves disputes over constitutional meaning through the accountable chamber: whichever coalition commands a legislative majority settles what the constitution requires, keeping fundamental law revisable by election rather than fixed by an insulated bench.
% TRANSFER_FUNCTION: Moves final interpretive authority — and with it the enforceable content of rights guarantees — from courts and rights-holders to sitting legislative majorities; moves the residual risk of rights infringement onto minorities who lose the judicial backstop.
% ABSENT_VOICES: Minority rights communities and human-rights advocacy networks have no formal seat: the framework gives them only electoral leverage and public argument, and courts speak but cannot bind. Their objection — that rights protection should not hinge on majority preference — is heard in debate and carries no procedural weight anywhere in the arrangement.
% DISAPPEARANCE_RATIONALE: If legislative supremacy vanished overnight, courts would begin invalidating statutes, governments would lose an override tool several jurisdictions have recently normalized, and the balance between branches would reorganize around judicial review; minority-protection architecture would rebuild itself around litigable rights.
% FOUNDING_PROBLEM: Securing democratic self-government against unaccountable adjudicative power: after long struggles against monarchical prerogative and later against judicial veto over elected programs, the settlement placed final constitutional authority in the chamber answerable at the polls.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and comparative-law literature outside the benefiting parties attest the anti-judicial-veto genesis (post-1689 settlement scholarship, colonial experience with imperial courts); the judiciary's own conduct — accepting declarations-based advisory roles rather than forcing confrontation — corroborates that the founding problem concerned adjudicative overreach. No source outside the benefiting parties attests that the problem remains unsolved today; the live-status claim is made only by the arrangement's beneficiaries, and critics outside that set attest the opposite.
narrative_ontology:disappearance_verdict(constitutional_text__legislative_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__legislative_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__legislative_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text__legislative_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__legislative_sovereignty_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__legislative_sovereignty_reading_tests).
:- end_tests(constitutional_text__legislative_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at interval end): the standing arrangement permanently exposes minority rights protection to majority override, but invocation remains episodic in most jurisdictions, and this reading's own lights credit political safeguards — hence a reading-indexed epsilon below what a judicial-supremacist observer would author for the same referent. Suppression (0.36) is authored as a raw structural property, unscaled: the arrangement subordinates courts to advisory status and removes the judicial exit for rights-holders, but increasingly relies on internalized convention rather than active force; only extractiveness is scaled by the engine (by directionality and scope). Theater is low (0.18): the advisory function is real, with modest performative thickening as dialogue rhetoric accumulates. Accessibility_collapse (0.50): within the framework the judicial-recourse alternative collapses once supremacy is understood, but electoral, federal, and amendment alternatives persist. Resistance (0.45): sustained scholarly and advocacy resistance, episodic judicial pushback at the edges of the advisory role. The measurement series run on one shared time grid (1982, 1990, 1998, 2005, 2012, 2019, 2025) with every tracked metric authored at every point. The suppression_requirement series is authored deliberately because this story specifically tracks enforcement-capacity change: the shift from active subordination (early Charter- and HRA-era contests over the advisory boundary) to internalized convention — enforcement decays while extraction slowly accumulates, a divergence the scalar alone would hide. Trajectories are monotonic, not cyclical; no intermittent-reinforcement mechanism is claimed. Coalition note: minority rights communities sit at powerless per-seat, but coalition formation across minority groups and allied movements is the lever that would raise their effective power; the classification assumes the coalition has not formed.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat computes an arrangement it built, controls, and experiences as accountable self-government; the payer seats compute enforced exposure of their protections to majority preference. The judiciary seat diverges internally: the same institution bears subordination (its rulings can be overridden) while collecting deference, workload, and agenda-setting influence over legal meaning. Same-level lateral dynamics: governing majorities and opposition legislators hold formally identical standing in the chamber, yet agenda control differentiates their exit options (arbitrage versus constrained) and their directionalities — the constraint binds them asymmetrically depending on the electoral cycle, which is why the opposition seat carries a secondary beneficiary role.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: governing parliamentary majorities sit near the beneficiary end (they collect the override power and control the rules); electoral majorities also derive near the beneficiary end, with the caveat recorded here that their exposure is rotational — today's beneficiaries are tomorrow's potential targets. Targets: minority rights communities (trapped exit, d near the full-target end) and opposition legislators (constrained exit, mid-high d). The judiciary is the override case: its payer role and subordinated status would push a naive derivation toward the full-target end, but structurally it is near-symmetric — advisory opinions still shape outcomes, governments need the bench's imprimatur, and courts retain prestige and interpretive initiative — so a directionality override pins the institutional seat at 0.55. Scope amplification applies modestly at national scale; the engine owns that arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification guards both failure modes. Reading the arrangement as pure extraction (snare) would ignore its live coordination function: keeping fundamental law revisable by accountable chambers is the solution side of the founding problem, and the override machinery is used and consequential, not vestigial. Reading it as pure coordination (rope) would ignore the asymmetric, enforced burden on minorities whose protections hang on majority restraint — an asymmetry the beneficiaries do not bear. The founding problem is contested: the anti-judicial-veto genesis is corroborated from outside the benefiting parties, but whether that problem remains live is disputed between beneficiaries (who say live) and critics (who say the problem was solved and the arrangement now serves incumbent convenience). The mismatch consumer reads status=contested x verdict=world_rearranges, which yields no dead-mandate or zombie flag; mandatrophy is not resolved, and the piton signature fails on the cost-asymmetry test — the administrator could change the arrangement, but the arrangement is not maintained theatrically; it is exercised.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'Which reading does the constitutional text itself support — does final interpretive authority belong to the legislature (this reading), the courts (judicial_supremacy_reading), or remain with the demos (popular_sovereignty_reading)?',
    'Comparative analysis of drafting history, amendment records, and structural provisions (presence or absence of override clauses, entrenchment of judicial review) across Westminster-family constitutions.',
    'Adopting judicial_supremacy_reading inverts this arrangement''s beneficiary/victim structure (courts become agenda setters; legislative majorities become targets); adopting popular_sovereignty_reading dissolves institutional supremacy and shifts the victim set to any entrenched interpreter. Each sibling is a separate constraint file with its own epsilon; resolving the allocation question changes which file describes the operative constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Locus-of-final-authority allocation across sibling readings of the constitutional_text kernel.').

omega_variable(
    political_safeguards_sufficiency,
    'Are electoral competition, federalism, and deliberation sufficient to protect minorities without a judicial backstop, as this reading asserts?',
    'Longitudinal comparison of minority-rights outcomes under weak-form (legislative-supremacy) versus strong-form (judicial-supremacy) review regimes, controlling for wealth and political culture.',
    'If political safeguards are insufficient, the reading-indexed epsilon understates real extraction and the arrangement drifts toward snare; if sufficient, the tangled_rope profile is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_safeguards_sufficiency, empirical, 'Whether political processes substitute for judicial rights protection.').

omega_variable(
    override_normalization_ratchet,
    'Will rising notwithstanding-clause and override usage (Saskatchewan 2017, Ontario 2018, Quebec''s systematic preemption) continue ratcheting, or remain episodic?',
    'Track invocation rates and preemption patterns across Westminster jurisdictions over successive legislative sessions.',
    'A continuing ratchet drives base_extractiveness upward toward snare territory and would date a tangled_rope-to-snare transition; episodic use stabilizes the current profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_normalization_ratchet, empirical, 'Trajectory of override normalization across Westminster systems.').

omega_variable(
    advisory_dialogue_reality,
    'Do advisory judicial opinions and declarations of incompatibility materially constrain legislative outcomes, or do they function as legitimating cover?',
    'Code legislative responses to every declaration of incompatibility and advisory opinion: remedial action, partial accommodation, or dismissal.',
    'If cover, theater_ratio is understated and the arrangement drifts toward piton or snare; if constraining, the dialogue function is real coordination and the judiciary''s near-symmetric directionality is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advisory_dialogue_reality, empirical, 'Reality of the advisory dialogue function beneath the supremacy arrangement.').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel best framed as the constitutional text itself (fixed_text codification) or as the supremacy-doctrine tradition layered above the text — and does the strict parse of popular sovereignty (''neither branch is supreme'') foreclose this reading after all?',
    'Run both framings through the classification apparatus and compare outputs: text-framing versus doctrine-tradition-framing for codification and authority; strict versus level-split parse of the popular-sovereignty premise for the sibling relation.',
    'Under the doctrine-tradition framing, authority_grounding shifts from lineage toward practice; under the strict parse, the popular_sovereignty relation flips from coexists_with to forecloses, changing the kernel''s foreclosure topology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Framing under-determination in kernel codification and sibling-relation structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__legislative_sovereignty_reading, 1982, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1982, constitutional_text__legislative_sovereignty_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(cons_tr_t1990, constitutional_text__legislative_sovereignty_reading, theater_ratio, 1990, 0.11).
narrative_ontology:measurement(cons_tr_t1998, constitutional_text__legislative_sovereignty_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(cons_tr_t2005, constitutional_text__legislative_sovereignty_reading, theater_ratio, 2005, 0.13).
narrative_ontology:measurement(cons_tr_t2012, constitutional_text__legislative_sovereignty_reading, theater_ratio, 2012, 0.15).
narrative_ontology:measurement(cons_tr_t2019, constitutional_text__legislative_sovereignty_reading, theater_ratio, 2019, 0.17).
narrative_ontology:measurement(cons_tr_t2025, constitutional_text__legislative_sovereignty_reading, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(cons_be_t1982, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 1982, 0.34).
narrative_ontology:measurement(cons_be_t1990, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(cons_be_t1998, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 1998, 0.37).
narrative_ontology:measurement(cons_be_t2005, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 2005, 0.39).
narrative_ontology:measurement(cons_be_t2012, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 2012, 0.4).
narrative_ontology:measurement(cons_be_t2019, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 2019, 0.41).
narrative_ontology:measurement(cons_be_t2025, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1982, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 1982, 0.52).
narrative_ontology:measurement(cons_su_t1990, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 1990, 0.48).
narrative_ontology:measurement(cons_su_t1998, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 1998, 0.44).
narrative_ontology:measurement(cons_su_t2005, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 2005, 0.41).
narrative_ontology:measurement(cons_su_t2012, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 2012, 0.39).
narrative_ontology:measurement(cons_su_t2019, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 2019, 0.37).
narrative_ontology:measurement(cons_su_t2025, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 2025, 0.36).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__legislative_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% One kernel (constitutional_text), three readings emitting distinct constraints. This file instantiates legislative_sovereignty_reading (epsilon approximately 0.42; beneficiaries are governing majorities and electoral majorities; victims are minority rights communities and out-of-power legislators). judicial_supremacy_reading relocates final authority to the bench, shifting the victim set to legislative majorities and majoritarian programs; popular_sovereignty_reading dissolves institutional supremacy entirely, shifting the victim set to any entrenched interpreter. The colloquial label 'what the constitution says about who decides' conflates these; epsilon differs across them because the standing arrangement under assessment differs. The upstream/downstream structure runs through the foreclosure edge documented in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text__legislative_sovereignty_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
