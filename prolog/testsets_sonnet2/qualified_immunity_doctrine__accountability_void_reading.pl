% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__accountability_void_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__accountability_void_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__accountability_void_reading
 *   human_readable: Qualified Immunity as Systematic Impunity Mechanism
 *   domain: constitutional_law/civil_rights/law_enforcement_policy
 *
 * SUMMARY:
 *   This story instantiates the accountability_void reading of the qualified
 *   immunity kernel: the doctrine as it actually operates in circuit court
 *   practice, where the clearly-established-law standard functions as a
 *   near-absolute bar to recovery that has grown more restrictive over four
 *   decades of case law, systematically shielding officers and the
 *   municipalities that employ them from the financial consequences of
 *   constitutional violations while leaving injured plaintiffs without
 *   remedy. This reading treats the doctrine's stated coordination rationale
 *   (protecting good-faith judgment calls under uncertainty) as a cover story
 *   that has been decoupled from its actual operation, which is asymmetric
 *   extraction: officers and departments capture the benefit of reduced
 *   litigation exposure while victims of proven or plausible constitutional
 *   violations absorb the cost with no compensating mechanism. This is one of
 *   three linked readings of the same kernel —
 *   constitutional_fidelity_reading treats the doctrine as illegitimate on
 *   separation-of-powers and textual grounds regardless of its policy
 *   effects, and protective_scaffold_reading treats the shield as a necessary
 *   and functioning coordination device. All three share the same doctrinal
 *   kernel (qualified immunity as interpreted by federal courts under Section
 *   1983) but author sharply different ε values because they are assessing
 *   different structural claims about the same standing arrangement.
 *
 * KEY AGENTS:
 *   - law_enforcement_officers: primary beneficiary (institutional/arbitrage) — shielded from personal liability
 *   - constitutional_rights_victims: primary target (powerless/trapped) — bear the uncompensated cost of violations
 *   - municipalities_and_departments: secondary beneficiary (institutional/arbitrage) — captures reduced litigation and settlement exposure
 *   - civil_rights_plaintiffs_bar: secondary target (moderate/constrained) — economics of representation collapse under the standard
 *   - federal_appellate_courts and supreme_court: agenda-setters (institutional/analytical) — administer and could revise the standard
 *   - congress: excluded agenda-setter (institutional/analytical) — has statutory authority to abolish the doctrine but has not exercised it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, 0.88).
domain_priors:suppression_score(qualified_immunity_doctrine__accountability_void_reading, 0.79).
domain_priors:theater_ratio(qualified_immunity_doctrine__accountability_void_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__accountability_void_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__accountability_void_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__accountability_void_reading, "Qualified Immunity as Systematic Impunity Mechanism").
narrative_ontology:topic_domain(qualified_immunity_doctrine__accountability_void_reading, "constitutional_law/civil_rights/law_enforcement_policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__accountability_void_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__accountability_void_reading, '3d0ab5b6-cf55-4508-bc52-e4fa38af7f30').
narrative_ontology:cs_kernel_codification('3d0ab5b6-cf55-4508-bc52-e4fa38af7f30', formalized).
narrative_ontology:cs_authority_grounding('3d0ab5b6-cf55-4508-bc52-e4fa38af7f30', extraction).
narrative_ontology:cs_interpretation_layer_present('3d0ab5b6-cf55-4508-bc52-e4fa38af7f30').
narrative_ontology:cs_reading_relation('3d0ab5b6-cf55-4508-bc52-e4fa38af7f30', qualified_immunity_doctrine__protective_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d0ab5b6-cf55-4508-bc52-e4fa38af7f30', qualified_immunity_doctrine__constitutional_fidelity_reading, influences).
narrative_ontology:cs_axiom('3d0ab5b6-cf55-4508-bc52-e4fa38af7f30', foundational, remedy_denial_constitutes_independent_harm).
narrative_ontology:cs_axiom_status(remedy_denial_constitutes_independent_harm, holdable).
narrative_ontology:cs_axiom_grounding('3d0ab5b6-cf55-4508-bc52-e4fa38af7f30', remedy_denial_constitutes_independent_harm, empirically_contingent).
narrative_ontology:cs_axiom('3d0ab5b6-cf55-4508-bc52-e4fa38af7f30', foundational, clearly_established_standard_has_decoupled_from_good_faith_rationale).
narrative_ontology:cs_axiom_status(clearly_established_standard_has_decoupled_from_good_faith_rationale, holdable).
narrative_ontology:cs_axiom_grounding('3d0ab5b6-cf55-4508-bc52-e4fa38af7f30', clearly_established_standard_has_decoupled_from_good_faith_rationale, empirically_contingent).
narrative_ontology:cs_reference_frame('3d0ab5b6-cf55-4508-bc52-e4fa38af7f30', harlow_subjective_good_faith_standard).
narrative_ontology:cs_drift_state('3d0ab5b6-cf55-4508-bc52-e4fa38af7f30', post_pearson_contemporary_circuit_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3d0ab5b6-cf55-4508-bc52-e4fa38af7f30', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, municipal_liability_insurers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, police_unions).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, constitutional_rights_victims).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, civil_rights_plaintiffs_bar).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__accountability_void_reading, communities_subject_to_policing).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__accountability_void_reading, municipalities_and_departments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Act under color of law and are shielded from personal civil liability for constitutional violations unless the specific right violated was already 'clearly established' by a prior case with materially identical facts. Because courts routinely dismiss cases at the clearly-established step without ever ruling on whether a violation occurred, the standard rarely gets to update — leaving officers protected from suit in nearly every novel fact pattern, indefinitely.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, law_enforcement_officers, beneficiary,
    institutional, biographical, arbitrage, national).

% Have suffered excessive force, wrongful searches, or other constitutional violations at the hands of officers, but find their federal civil rights claims dismissed before discovery or trial because no prior case matched their exact facts closely enough. They bear the physical, psychological, and financial cost of the violation with no compensatory remedy and no judicial finding that a violation even occurred.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, constitutional_rights_victims, payer,
    powerless, biographical, trapped, local).

% Budget for far lower litigation exposure and settlement costs than they would absorb under a standard negligence or strict liability regime, since most claims against individual officers are dismissed on immunity grounds before reaching the merits. Departments have no structural incentive to reform training or discipline practices because the financial signal that would normally drive such reform — successful litigation and judgments — rarely arrives.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, municipalities_and_departments, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__accountability_void_reading, municipalities_and_departments, agenda_setter).

% Attorneys who would litigate constitutional tort claims on contingency face a doctrine that makes claims extraordinarily difficult to win, driving down the expected value of taking cases and reducing the supply of representation available to victims, especially in circuits with narrow clearly-established-law precedent. Many meritorious cases are never filed because the economics do not support the litigation risk.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, civil_rights_plaintiffs_bar, payer,
    moderate, biographical, constrained, national).

% Administer and continually reformulate the clearly-established-law standard through published opinions, and can choose whether to reach the merits of a constitutional question before resolving immunity. Their choice to frequently skip the merits question (post-Pearson v. Callahan, permissively) is the mechanism that prevents the body of clearly-established law from growing, perpetuating the doctrine's protective scope.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, federal_appellate_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Originated and has repeatedly reaffirmed and expanded the qualified immunity doctrine through case law rather than statute, and could narrow, discipline, or abolish it through a single ruling. Multiple justices across the ideological spectrum have publicly questioned the doctrine's textual basis, yet the Court has repeatedly declined to grant certiorari on cases squarely presenting the question.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Could amend 42 U.S.C. Section 1983 to abolish or narrow qualified immunity by statute, and reform bills have been introduced repeatedly, but none have passed given the political costs of appearing anti-police. Congress's silence functions as tacit ratification of a doctrine the Court itself acknowledges it invented, without Congress ever having affirmatively voted for the standard now in force.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__accountability_void_reading, congress, excluded,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__accountability_void_reading, municipalities_and_departments).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__accountability_void_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its stated form, the doctrine coordinates the willingness of officers to make split-second judgment calls under uncertainty without fear of personal financial ruin from every close call later found unconstitutional in hindsight, and it coordinates predictable municipal budgeting for litigation exposure.
% TRANSFER_FUNCTION: Moves the cost of constitutional violations from the individual officers and departments who commit them onto the victims who absorb the injury without compensation, and secondarily onto the public which loses the deterrent and information-forcing effects that damages litigation would otherwise generate.
% ABSENT_VOICES: Victims of unconstitutional police conduct whose cases were dismissed at the pleading stage never had their facts adjudicated on the merits at all — courts frequently resolve immunity without ever deciding whether a constitutional violation occurred, so there is no public record and no voice for what happened to them within the very proceeding meant to vindicate their rights.
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, officers and municipalities would face ordinary Section 1983 liability exposure calibrated to actual constitutional harm; insurance markets, department training budgets, use-of-force policy, and settlement practices would reorganize rapidly around the restored deterrent signal, and a substantial volume of previously dismissed claims would become viable litigation.
% FOUNDING_PROBLEM: The doctrine was originally justified in Harlow v. Fitzgerald (1982) as solving a genuine problem: without some shield, officials would face debilitating litigation and discovery burdens for good-faith judgment calls made under uncertainty, chilling vigorous public service.
% FOUNDING_PROBLEM_CORROBORATION: Sitting and former federal judges across the ideological spectrum (including Justice Thomas in dissent, and Judges Willett and Sutton on the circuit courts) have written opinions stating the doctrine has drifted from its original good-faith rationale into a near-absolute bar untethered from the statute's text or history; empirical studies (e.g., Schwartz 2014 UCLA Law Review analysis of indemnification and settlement data) corroborate from outside the officer/municipality beneficiary set that the doctrine's practical operation diverges sharply from its stated purpose.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__accountability_void_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__accountability_void_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__accountability_void_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__accountability_void_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__accountability_void_reading, 0.88, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__accountability_void_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__accountability_void_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.88) because the clearly-established-law standard, as it has evolved through circuit court practice, functions less as a good-faith-judgment shield and more as a near-categorical bar requiring near-identical prior precedent — a standard that structurally guarantees most novel violations go uncompensated regardless of severity. Suppression is authored high (0.79) because courts frequently decline to reach the merits question at all (post-Pearson), which is an active mechanism suppressing the growth of clearly-established law that would otherwise narrow the shield over time — this is not passive neglect but an affirmatively maintained gap. Theater ratio is moderate (0.42): there is a genuine, non-trivial coordination function (protecting officers from truly novel, good-faith split-second decisions) but the theater has grown as courts increasingly invoke the standard to dispose of cases involving conduct that violates settled constitutional norms in substance even where no factually identical precedent exists. Accessibility collapse is high (0.81): once a plaintiff's bar understands the doctrine's actual operation, viable claim volume for anything short of near-identical precedent collapses almost completely. Resistance is substantial (0.62) reflecting active, sustained pushback from civil rights litigators, some federal judges, and reform advocates — this is a contested doctrine, not a quietly accepted one.
 *
 * PERSPECTIVAL GAP:
 *   Officers and municipalities experience the doctrine as a stable, well-functioning protective structure they rely on for operational and budgetary predictability. Victims and their counsel experience the identical doctrinal structure as an opaque, near-impenetrable bar that prevents their claims from ever being heard on the merits. The engine computes these divergent seat classifications from the same structural data — this story does not resolve the divergence but documents it as the object of measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Officers and municipalities are the structural beneficiaries: officers face dramatically reduced personal liability exposure (d near the beneficiary end) and departments capture reduced settlement and litigation costs, with the additional benefit of avoiding the reform pressure that adverse judgments would otherwise generate. Victims are the structural targets: trapped by the injury already suffered, powerless relative to the institutional defendants, and facing near-total accessibility collapse of the remedy Section 1983 was designed to provide. The plaintiffs' bar sits as a secondary target whose exit option (declining to take cases) does not help the victims who need representation — it merely reflects the doctrine's success at suppressing litigation supply. Federal courts and the Supreme Court hold analytical/institutional positions with the standing to revise the doctrine but have structural reasons (docket management, deference to law enforcement, incremental jurisprudence) not to.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting officers from ruinous litigation over genuinely uncertain, good-faith judgment calls) may still be partially live, but the doctrine's actual operation — a standard requiring near-identical prior precedent, applied by courts that frequently decline to reach the merits — has drifted well past that founding justification into a broader shield covering conduct that violates settled constitutional norms in substance. The founding_problem_status is authored as contested rather than dead because reasonable disagreement exists about whether any residual good-faith-protection function remains live; but this reading holds that whatever coordination function persists is now substantially outweighed by the asymmetric extraction the doctrine performs on victims who have no remedy. Classifying this as snare (rather than tangled_rope) reflects the judgment that the coordination story, in its accountability_void form, functions primarily as cover — the beneficiary/victim asymmetry is severe, exit is essentially unavailable to victims, and disappearance would visibly rearrange settlement practices and litigation volume, which argues against pure mountain/natural-fact framing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_versus_extraction_ratio,
    'What fraction of qualified immunity''s actual dismissals protect genuinely novel, good-faith judgment calls versus conduct that violates settled constitutional norms in substance but lacks factually identical precedent?',
    'Systematic empirical coding of circuit court qualified immunity dismissals (as in Schwartz''s and Reinhart''s empirical studies) categorizing dismissed claims by whether the underlying conduct would likely have been found unconstitutional had the merits been reached.',
    'A high genuinely-novel-conduct fraction would support the protective_scaffold_reading''s characterization; a high settled-violation fraction corroborates this reading''s characterization of the doctrine as extraction dressed as coordination. Current empirical literature leans toward the latter but is not dispositive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_versus_extraction_ratio, empirical, 'How much of immunity''s operation is genuine coordination versus extraction dressed as coordination.').

omega_variable(
    kernel_framing_selection,
    'Is qualified immunity best understood as a single doctrine subject to competing normative readings (this framing), or as a genealogically split doctrine where the 1982 subjective-good-faith standard and the modern clearly-established-law standard are structurally different constraints that happen to share a name?',
    'Doctrinal history analysis distinguishing Harlow v. Fitzgerald''s original subjective standard from the objective clearly-established-law standard that displaced it, tracking whether the shift itself constitutes a new constraint rather than evolution of the same one.',
    'If the modern standard is genealogically distinct from the founding doctrine, this story''s ε and beneficiary structure describe only the modern instantiation, and the founding_problem''s corroboration weakens further, strengthening the accountability_void reading''s claim that the coordination rationale no longer attaches to the operative standard.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_selection, conceptual, 'Whether the doctrine''s historical drift constitutes evolution of one constraint or a genealogical split into two.').

omega_variable(
    reform_tractability,
    'Given that both the Supreme Court and Congress hold the institutional power to abolish or substantially narrow the doctrine, why has neither acted despite persistent, cross-ideological criticism from sitting judges?',
    'Political economy analysis of legislative reform failure (tracking bill introductions, floor votes, and stated objections) combined with certiorari-denial pattern analysis at the Supreme Court to identify whether institutional inertia, political risk aversion, or genuine substantive disagreement best explains non-action.',
    'If institutional inertia/political risk aversion dominates, this supports classifying the persistence mechanism as extraction-adjacent (a piton-like inertial component layered on the snare); if genuine substantive disagreement about optimal policy dominates, the persistence is better read as contested policy equilibrium rather than captured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_tractability, conceptual, 'Whether the doctrine''s persistence despite reform pressure reflects capture, inertia, or genuine unresolved policy disagreement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__accountability_void_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t0, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(qual_tr_t8, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(qual_tr_t16, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(qual_tr_t24, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(qual_tr_t32, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(qual_tr_t40, qualified_immunity_doctrine__accountability_void_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(qual_be_t0, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(qual_be_t8, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(qual_be_t16, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 16, 0.71).
narrative_ontology:measurement(qual_be_t24, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement(qual_be_t32, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 32, 0.85).
narrative_ontology:measurement(qual_be_t40, qualified_immunity_doctrine__accountability_void_reading, base_extractiveness, 40, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t0, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(qual_su_t8, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(qual_su_t16, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(qual_su_t24, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(qual_su_t32, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 32, 0.74).
narrative_ontology:measurement(qual_su_t40, qualified_immunity_doctrine__accountability_void_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__accountability_void_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(qualified_immunity_doctrine__accountability_void_reading, 0.1).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine__protective_scaffold_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, qualified_immunity_doctrine__constitutional_fidelity_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, section_1983_civil_rights_liability_framework).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__accountability_void_reading, police_union_collective_bargaining_discipline_shield).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'qualified immunity' per the epsilon-invariance principle: accountability_void_reading (this file, ε=0.88, snare), protective_scaffold_reading (a separate file, expected low-moderate ε, rope or tangled_rope), and constitutional_fidelity_reading (a separate file assessing textual/institutional legitimacy independent of cost-benefit, likely tangled_rope or snare on different grounds — judicial usurpation of a legislative function). All three share the doctrinal kernel qualified_immunity_doctrine but author different ε values because they measure different structural claims about the same standing arrangement, not the same claim from different angles. Linked via affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__accountability_void_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
