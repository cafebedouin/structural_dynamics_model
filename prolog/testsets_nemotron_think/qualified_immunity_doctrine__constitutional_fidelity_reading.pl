% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__constitutional_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__constitutional_fidelity_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__constitutional_fidelity_reading
 *   human_readable: Qualified Immunity Doctrine (Constitutional Fidelity Reading)
 *   domain: constitutional_law/civil_rights/law_enforcement_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the constitutional_fidelity_reading of
 *   the qualified_immunity_doctrine kernel. The reading holds that qualified
 *   immunity is a judicially fabricated doctrine with no basis in the
 *   constitutional text, the enacting statute (42 U.S.C. § 1983), or the
 *   original understanding of 1871. The doctrine was created by the Supreme
 *   Court in Pierson v. Ray (1967) and expanded in Harlow v. Fitzgerald
 *   (1982) to become an objective, clearly-established-law test that
 *   functionally immunizes officers unless a prior case has held the exact
 *   conduct unconstitutional. From this reading's perspective, the doctrine
 *   is illegitimate regardless of its policy outcomes — it is a usurpation of
 *   legislative authority by the judiciary that expands judicial
 *   institutional power at the expense of both law enforcement officers (who
 *   are denied a clear, democratically enacted framework for their conduct)
 *   and victims of constitutional violations (who are denied remedies). The
 *   constraint is the standing arrangement of qualified immunity doctrine as
 *   it operates today, assessed by this reading's lights: high extraction
 *   (judiciary captures interpretive monopoly), high suppression (stare
 *   decisis and the clearly-established test block alternatives), moderate
 *   theater (the 'good faith' and 'clearly established' framings perform
 *   legitimacy), high accessibility collapse (lower courts cannot deviate;
 *   Congress has not overridden), and moderate resistance (academic critique,
 *   some circuit splits, but no structural challenge). The claimed_type is
 *   snare: pure extraction masquerading as coordination, with the judiciary
 *   as sole beneficiary.
 *
 * KEY AGENTS:
 *   - federal_judiciary: Primary beneficiary (institutional/generational/analytical) — authors and enforces doctrine, expands interpretive monopoly
 *   - law_enforcement_officers: Victim (organized/biographical/constrained) — denied legitimate statutory framework, face unpredictable liability standards
 *   - constitutional_violation_victims: Victim (powerless/immediate/trapped) — denied remedies for rights violations unless prior case matches exactly
 *   - congress: Excluded (institutional/generational/analytical) — stripped of authority to define immunity scope, acquiesces through inaction
 *   - legal_academy: Observer (analytical/generational/analytical) — critiques doctrine but lacks institutional power to change it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.78).
domain_priors:suppression_score(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.82).
domain_priors:theater_ratio(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__constitutional_fidelity_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__constitutional_fidelity_reading, "Qualified Immunity Doctrine (Constitutional Fidelity Reading)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__constitutional_fidelity_reading, "constitutional_law/civil_rights/law_enforcement_policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__constitutional_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__constitutional_fidelity_reading, 'f176a58b-bc4e-4fb2-94d6-2973a411d806').
narrative_ontology:cs_kernel_codification('f176a58b-bc4e-4fb2-94d6-2973a411d806', fixed_text).
narrative_ontology:cs_authority_grounding('f176a58b-bc4e-4fb2-94d6-2973a411d806', lineage).
narrative_ontology:cs_interpretation_layer_present('f176a58b-bc4e-4fb2-94d6-2973a411d806').
narrative_ontology:cs_reading_relation('f176a58b-bc4e-4fb2-94d6-2973a411d806', qualified_immunity_doctrine__protective_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('f176a58b-bc4e-4fb2-94d6-2973a411d806', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_axiom('f176a58b-bc4e-4fb2-94d6-2973a411d806', foundational, judicial_fabrication_without_authorization).
narrative_ontology:cs_axiom_status(judicial_fabrication_without_authorization, holdable).
narrative_ontology:cs_axiom_grounding('f176a58b-bc4e-4fb2-94d6-2973a411d806', judicial_fabrication_without_authorization, deontological).
narrative_ontology:cs_axiom('f176a58b-bc4e-4fb2-94d6-2973a411d806', foundational, doctrine_illegitimate_regardless_of_outcomes).
narrative_ontology:cs_axiom_status(doctrine_illegitimate_regardless_of_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('f176a58b-bc4e-4fb2-94d6-2973a411d806', doctrine_illegitimate_regardless_of_outcomes, deontological).
narrative_ontology:cs_reference_frame('f176a58b-bc4e-4fb2-94d6-2973a411d806', constitutional_text_and_original_understanding).
narrative_ontology:cs_drift_state('f176a58b-bc4e-4fb2-94d6-2973a411d806', contemporary_doctrine_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('f176a58b-bc4e-4fb2-94d6-2973a411d806', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_violation_victims).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__constitutional_fidelity_reading, judicial_supremacy_in_constitutional_interpretation).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__constitutional_fidelity_reading, stare_decisis_as_institutional_preservation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors, maintains, and expands the qualified immunity doctrine through precedent. The doctrine grants the judiciary a monopoly on defining the scope of constitutional accountability for state actors, transferring that authority from Congress (statutory text) and juries (fact-finding) to appellate courts (interpretive judgment). The judiciary collects institutional power — control over the constitutional tort docket, supremacy in §1983 interpretation, and insulation from legislative correction — without bearing the costs of constitutional violations or the operational uncertainty officers face.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary, beneficiary).

% Subject to a liability standard that changes case-by-case based on whether a prior appellate decision has held the exact conduct unconstitutional. Officers lack a clear, democratically enacted statutory framework defining their immunity; instead they face judicially created standards that are unpredictable and retroactively applied. Their organizations (police unions, FRATERNAL ORDER) publicly defend the doctrine, but this reflects path dependence and institutional inertia — officers themselves are denied the legislative clarity that would come from congressional action. Exit is constrained: they cannot opt out of §1983 liability, and the doctrine's uncertainty is a structural feature of their legal environment.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_officers, payer,
    organized, biographical, constrained, national).

% Individuals whose constitutional rights have been violated by state actors. Under the clearly-established-law test, they can recover damages only if a prior case has held the exact same conduct unconstitutional in the same jurisdiction. This requires a factually identical precedent — a near-impossible standard for novel or context-specific violations. They bear the full cost of the doctrine's extraction: lost remedies, no deterrence for future violations, and no legislative recourse (Congress has not acted). Exit is effectively trapped: they cannot access state courts for federal claims (exhaustion not required but state courts follow federal precedent), and the Supreme Court has rejected arguments that the doctrine violates due process or the statute's text.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_violation_victims, payer,
    powerless, immediate, trapped, national).

% The legislative body that enacted §1983 in 1871 with no immunity provision. The Supreme Court has held that qualified immunity is grounded in 'historical' immunities at common law, not in the statute's text, and that Congress can override it — but any legislative fix faces the Court's constitutional review power and the institutional weight of 50+ years of precedent. Congress is structurally excluded from the immunity policy space: it has analytical exit (it could legislate) but is trapped by the judiciary's claim of constitutional authority over the doctrine's core. No major immunity reform legislation has passed since the doctrine's creation.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, congress, excluded,
    institutional, generational, analytical, national).

% Scholars, law professors, and legal commentators who near-unanimously criticize qualified immunity as lacking textual, historical, or doctrinal basis. They produce the empirical and theoretical analysis showing the doctrine's fabrication, but they hold no institutional power to change it. Their exit is analytical: they can observe, critique, and propose alternatives, but they neither collect rents from the doctrine nor bear its costs directly. Their situation is the perch from which the structural asymmetry is most visible.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, legal_academy, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The doctrine claims to coordinate by protecting officers from the burden of defending against frivolous lawsuits and giving fair notice of prohibited conduct. This reading holds that this coordination function is pretextual: the clearly-established test does not give fair notice (it requires factually identical precedent), and the 'frivolous suit' rationale is belied by the doctrine's application to meritorious claims. The real function is judicial control over the §1983 docket.
% TRANSFER_FUNCTION: Moves institutional power and interpretive monopoly to the federal judiciary; moves the cost of constitutional violations from the state (via officer liability) to individual victims; moves legal clarity from officers (who get case-by-case standards instead of statutory rules) to the judiciary (which retains discretionary control). The transfer is not monetary but structural: authority over constitutional accountability shifts from democratic branches to courts.
% ABSENT_VOICES: Congress (stripped of authority to define immunity, acquiesces through inaction); state legislatures (could create parallel state-law remedies but most have not); civil society organizations representing communities most affected by police violence (excluded from the judicial discourse that shapes the clearly-established test); originalist and textualist judges who have not applied their methodology to this doctrine (present in the judiciary but silenced by stare decisis).
% DISAPPEARANCE_RATIONALE: If qualified immunity vanished overnight, §1983 would operate on its statutory text: 'every person' who under color of law deprives rights 'shall be liable.' Officers would face liability for constitutional violations without the clearly-established filter; Congress would be forced to legislate a clear immunity framework (or not); victims would recover for violations without needing a factually identical precedent; the judiciary would lose its interpretive monopoly over the scope of constitutional accountability. The mobile software economy analogy: the mobile software economy would reorganize around open payment routing.
% FOUNDING_PROBLEM: Judicial management of the §1983 litigation docket and protection of judicial supremacy in constitutional interpretation. The Court in Pierson v. Ray (1967) imported a 'good faith' defense from common law; in Harlow v. Fitzgerald (1982) it converted this to an objective 'clearly established law' test, explicitly citing the need to avoid 'broad-ranging discovery' and 'insubstantial claims' burdening officials. The founding problem was docket control and judicial efficiency, not officer protection per se.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (William Baude, 'Is Qualified Immunity Unlawful?' 2018; Joanna Schwartz, 'How Qualified Immunity Fails' 2020) document that the 1871 Congress enacted no immunity and the common law provided no such defense for constitutional torts. Originalist scholars (Justice Thomas's Ziglar v. Abbasi concurrence; Professor Baude) attest the doctrine lacks originalist foundation. Congressional records show no debate on immunity in 1871. The judiciary's own prior cases (Monroe v. Pape, 1961) held §1983 provides a remedy 'against the statute' with no immunity. No corroborating source outside the benefiting judiciary supports the claim that the founding problem is live.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__constitutional_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__constitutional_fidelity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__constitutional_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) reflects the judiciary's capture of the immunity policy space: the doctrine transfers the power to define the scope of constitutional accountability from Congress (text) and juries (fact) to courts (interpretive monopoly). Suppression (0.82) is high because the clearly-established-law test structurally suppresses alternatives — lower courts cannot recognize new violations, plaintiffs cannot prevail without a factually identical precedent, and legislative overrides are treated as unnecessary because the Court claims constitutional authority. Theater ratio (0.45) captures the performative 'good faith' and 'clearly established' language that masks the doctrine's true function: the test is outcome-determinative in favor of immunity in the vast majority of cases. Accessibility collapse (0.75) is high because the judicial monopoly on constitutional interpretation means no alternative framework can gain traction — Congress could legislate but faces institutional inertia and judicial review; state courts follow federal precedent. Resistance (0.42) is moderate: academic criticism is near-unanimous against the doctrine's legitimacy, some judges dissent, but the institutional structure produces no effective challenge.
 *
 * PERSPECTIVAL GAP:
 *   The protective_scaffold_reading sees officers as beneficiaries (coordination: protection from frivolous suits) and the doctrine as rope/tangled_rope. The accountability_void_reading sees victims as primary targets and the doctrine as snare. This reading sees BOTH officers and victims as victims of judicial fabrication, with the judiciary as sole beneficiary. The engine will compute three different per-seat type profiles from the same structural data — the divergence IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary is the structural beneficiary (d ~ 0.1): it gains institutional power, interpretive control, and docket management authority. Law enforcement officers are victims in this reading (d ~ 0.7): they face unpredictable, case-by-case liability standards instead of clear statutory rules, and their organizations' support for the doctrine reflects path dependence, not structural benefit. Constitutional violation victims are full targets (d ~ 0.95): they bear the full cost of the doctrine's extraction (lost remedies, no deterrence). Congress is excluded (d not computed): it has analytical exit but structural traps (judicial review of any legislative fix). The legal academy is analytical observer (d = 0.5): it sees the full structure but collects no rents and pays no costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (judicial management of §1983 litigation docket / protection of judicial supremacy) is dead: the docket-management rationale cannot justify a doctrine that lacks any textual or historical authorization. The doctrine persists because it serves the judiciary's institutional interest in interpretive monopoly, not because it solves a live coordination problem. This is mandatrophy: the constraint's mandate (if any) has been entirely displaced by institutional self-preservation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does this constraint represent one reading of the contested qualified_immunity_doctrine kernel, and which structural elements distinguish it from sibling readings?',
    'Comparative analysis of the three declared readings (constitutional_fidelity_reading, protective_scaffold_reading, accountability_void_reading) on beneficiary/victim sets, claimed_type, and epsilon referent. The kernel_id and reading_id are fixed by the committer frame; this omega records the committer structure that the schema has no dedicated field for.',
    'If the reading identity is not tracked, the corpus conflates distinct constraints sharing a colloquial label, violating ε-invariance (DP-001). Each reading must instantiate its own constraint with independent metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Committee frame identity: this constraint is the constitutional_fidelity_reading of kernel qualified_immunity_doctrine; siblings are protective_scaffold_reading and accountability_void_reading.').

omega_variable(
    officer_victim_status_ambiguity,
    'In this reading, are law enforcement officers properly classified as victims (denied legitimate framework) or do they remain net beneficiaries of the doctrine''s operational shield?',
    'Empirical analysis of officer litigation outcomes, qualified immunity grant rates, and officer organizational positions on the doctrine. If officers overwhelmingly defend the doctrine and win dismissal, the victim classification may reflect the reading''s normative frame rather than structural position.',
    'If officers are net beneficiaries, the victim set shrinks to constitutional_violation_victims only, altering the extraction asymmetry and potentially shifting classification from snare toward tangled_rope (coordination for officers + extraction from victims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(officer_victim_status_ambiguity, empirical, 'Whether the reading''s claim that officers are denied legitimate framework matches their structural position as net extractees.').

omega_variable(
    legislative_abdication_vs_judicial_usurpation,
    'Is the absence of statutory immunity a result of legislative abdication (Congress chose not to act) or judicial usurpation (courts preempted the field)?',
    'Historical analysis of congressional intent in 1871 (Ku Klux Klan Act), 1961 (Monroe v. Pape), and subsequent legislative history. Did Congress implicitly delegate immunity policy to courts, or did courts invent doctrine contrary to statutory scheme?',
    'If legislative abdication, the doctrine fills a vacuum Congress left — weakening the ''fabrication'' claim. If judicial usurpation, the fabrication claim is structurally robust and the beneficiary (judiciary) is the active usurper.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_abdication_vs_judicial_usurpation, conceptual, 'Origin of the statutory gap: congressional silence vs. judicial preemption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__constitutional_fidelity_reading, 0, 57).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qi_cf_tr_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(qi_cf_tr_t15, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(qi_cf_tr_t30, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(qi_cf_tr_t45, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 45, 0.42).
narrative_ontology:measurement(qi_cf_tr_t57, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 57, 0.45).

% Extraction over time
narrative_ontology:measurement(qi_cf_be_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qi_cf_be_t15, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(qi_cf_be_t30, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(qi_cf_be_t45, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 45, 0.75).
narrative_ontology:measurement(qi_cf_be_t57, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 57, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(qi_cf_su_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(qi_cf_su_t15, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(qi_cf_su_t30, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(qi_cf_su_t45, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 45, 0.78).
narrative_ontology:measurement(qi_cf_su_t57, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 57, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__constitutional_fidelity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.12).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, section_1983_litigation_structure).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, judicial_review_of_police_conduct).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, congressional_civil_rights_enforcement_authority).

% DUAL FORMULATION NOTE:
% Part of qualified_immunity_doctrine kernel family. This reading (constitutional_fidelity) and protective_scaffold_reading are mutual foreclosures; both coexist_with accountability_void_reading. The ε values differ widely: this reading sees high extraction (fabrication as usurpation), protective_scaffold sees low extraction (coordination function), accountability_void sees very high extraction (impunity mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__constitutional_fidelity_reading, organized, 0.68).
constraint_indexing:directionality_override(qualified_immunity_doctrine__constitutional_fidelity_reading, powerless, 0.95).
constraint_indexing:directionality_override(qualified_immunity_doctrine__constitutional_fidelity_reading, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
