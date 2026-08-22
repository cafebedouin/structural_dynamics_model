% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_originalist, []).

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
 *   constraint_id: us_constitution_meaning__originalist_reading
 *   human_readable: Constitutional Originalism: Fixed Meaning at Ratification
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   Constitutional originalism claims that the U.S. Constitution's meaning
 *   was fixed at the moment of ratification (or at each amendment's
 *   ratification), and that judges are bound to interpret the document
 *   according to its public meaning at that historical moment. Originalism
 *   frames itself as a constraint on judicial discretion: judges may not read
 *   contemporary values into the text, may not adapt meaning to changed
 *   circumstances, and may not invent unenumerated rights based on evolving
 *   social understanding. This reading of the Constitution operates as a
 *   tangled rope: it coordinates around a stable, predictable rule
 *   (historical meaning determines outcomes), but does so asymmetrically—it
 *   benefits institutional actors committed to constraint-based legitimacy
 *   and those whose preferred outcomes align with 18th-century
 *   understandings, while it victimizes contemporary rights claimants whose
 *   claims lack historical support. The extraction is substantial because
 *   originalism's gatekeeping effect prevents entire categories of
 *   constitutional arguments (dignity-based rights, evolving equal protection
 *   principles) from gaining purchase in originalist-dominated courts, and
 *   that suppression is actively enforced through doctrine.
 *
 * KEY AGENTS:
 *   - originalist_judiciary: federal judges (especially Supreme Court appointees) who have embraced originalist methodology and enforce it through precedent and doctrinal gatekeeping
 *   - counter_majoritarian_constraint_advocates: scholars, judges, and institutional actors who value judicial restraint and believe originalism best constrains judges to law rather than policy
 *   - conservative_political_coalition: political interests whose policy preferences (limited welfare state, strong property rights, deference to executive on national security) historically aligned with originalist outcomes
 *   - contemporary_rights_claimants_without_18th_century_support: individuals and groups seeking constitutional recognition for dignity, autonomy, equality, and privacy interests that were not explicitly protected in 1788 or the amending periods
 *   - marginalized_groups_seeking_unenumerated_protections: LGBTQ persons, racial and religious minorities, people with disabilities seeking constitutional protection for identities and practices not contemplated in historical constitutional text
 *   - living_constitutionalist_judiciary: judges and scholars who read the Constitution as a living document whose principles endure but apply contextually
 *   - legal_academia_originalist_wing: law professors and scholars who generate originalist arguments and legitimating theory
 *   - state_legislatures: occasional beneficiaries when originalism restrains federal constitutional claims against state action
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, 0.68).
domain_priors:suppression_score(us_constitution_meaning__originalist_reading, 0.72).
domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__originalist_reading, "Constitutional Originalism: Fixed Meaning at Ratification").
narrative_ontology:topic_domain(us_constitution_meaning__originalist_reading, "constitutional/legal/political").

domain_priors:requires_active_enforcement(us_constitution_meaning__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__originalist_reading, 'd3db3e92-987f-468d-845a-561339a7a20b').
narrative_ontology:cs_kernel_codification('d3db3e92-987f-468d-845a-561339a7a20b', fixed_text).
narrative_ontology:cs_authority_grounding('d3db3e92-987f-468d-845a-561339a7a20b', lineage).
narrative_ontology:cs_interpretation_layer_present('d3db3e92-987f-468d-845a-561339a7a20b').
narrative_ontology:cs_reading_relation('d3db3e92-987f-468d-845a-561339a7a20b', us_constitution_meaning__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d3db3e92-987f-468d-845a-561339a7a20b', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('d3db3e92-987f-468d-845a-561339a7a20b', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('d3db3e92-987f-468d-845a-561339a7a20b', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('d3db3e92-987f-468d-845a-561339a7a20b', foundational, judges_bound_by_historical_public_meaning).
narrative_ontology:cs_axiom_status(judges_bound_by_historical_public_meaning, holdable).
narrative_ontology:cs_axiom_grounding('d3db3e92-987f-468d-845a-561339a7a20b', judges_bound_by_historical_public_meaning, deontological).
narrative_ontology:cs_axiom('d3db3e92-987f-468d-845a-561339a7a20b', secondary, contemporary_circumstances_irrelevant_to_meaning).
narrative_ontology:cs_axiom_status(contemporary_circumstances_irrelevant_to_meaning, holdable).
narrative_ontology:cs_axiom_grounding('d3db3e92-987f-468d-845a-561339a7a20b', contemporary_circumstances_irrelevant_to_meaning, deontological).
narrative_ontology:cs_reference_frame('d3db3e92-987f-468d-845a-561339a7a20b', framers_original_public_meaning_at_ratification).
narrative_ontology:cs_drift_state('d3db3e92-987f-468d-845a-561339a7a20b', contemporary_post_2010, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d3db3e92-987f-468d-845a-561339a7a20b', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(us_constitution_meaning__originalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, originalist_judiciary).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, contemporary_rights_claimants_without_18th_century_support).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, marginalized_groups_seeking_unenumerated_protections).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, conservative_political_coalition).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, legal_academia_originalist_wing).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, state_legislatures).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, judicial_restraint_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, rule_of_law_fixity).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, separation_of_powers_rigidity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal judges (primarily Supreme Court, appellate courts, and originalist-oriented district judges) who have adopted originalist methodology and enforce it through precedent, doctrine, and opinion writing. They set the agenda by determining what counts as a legitimate constitutional argument (historical public meaning only) and gatekeeping which arguments reach judicial decision. They believe themselves constrained by history and law, not by policy preference. They can exit by retiring or by shifting methodology, but they maintain the constraint through continuous doctrinal enforcement.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, generational, mobile, national).

% Scholars, judges, and institutional actors (primarily originalist law professors, Federalist Society members, constitutional scholars committed to originalism) who benefit from a judicial methodology that appears to constrain judges and restrain the expansion of unenumerated rights. They collect the benefit of judicial restraint and rule-of-law predictability. They have significant arbitrage options: they can publish, teach, influence judicial appointments, and shape constitutional discourse. They are genuinely invested in the belief that originalism constrains, not that it disguises discretion.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates, beneficiary,
    institutional, generational, arbitrage, national).

% Conservative political interests, think tanks, religious organizations, and right-leaning political parties benefit from originalist outcomes because originalism's fixation on 18th-century meaning systematically produces outcomes favoring limited welfare state, strong property rights, religious liberty, and deference to executive power on national security. They have substantial arbitrage: they can influence judicial appointments, fund originalist scholars, shape public discourse, and retreat to state-level constitutionalism. They have no identity-lock to originalism—they would adopt a different methodology if it served their interests better.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, conservative_political_coalition, beneficiary,
    powerful, generational, arbitrage, national).

% Individuals and groups seeking constitutional recognition for rights and protections that were not explicitly provided in 1788 or at major amendment periods: primarily LGBTQ persons seeking marriage rights and identity protection, immigrants seeking equal protection, people with disabilities seeking accommodation rights, and contemporary workers seeking dignified labor conditions and privacy protections. They bear the cost of originalism by being foreclosed from constitutional argument in originalist-dominated courts. Their exit options are highly constrained: they cannot simply move to a different jurisdiction (constitutional meaning is national), they cannot opt out of the Constitution (it applies to them regardless), and seeking rights is often constitutive of their identity (making exit identity-locked for many). The cost they bear is the categorical foreclosure of their arguments from the judicial arena.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, contemporary_rights_claimants_without_18th_century_support, payer,
    moderate, biographical, constrained, national).

% Racial and religious minorities, non-citizens, persons living in poverty, and other marginalized groups seeking constitutional expansion of equal protection, dignity, and autonomy beyond what the Framers explicitly recognized. They bear costs because originalism systematically denies the legitimacy of claims grounded in contemporary understanding of human dignity and equality. They are also partially excluded from the constitutional conversation: their lived experience of inequality is not considered relevant to determining constitutional meaning (what matters is what the public thought in 1788, not what people think today). Their exit is identity-locked: they cannot leave their race, religion, citizenship status, or marginalized position, and seeking dignity recognition is often constitutive of their identity as persons. The cost is both direct suppression and epistemic exclusion from the conversation about what rights matter.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, marginalized_groups_seeking_unenumerated_protections, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__originalist_reading, marginalized_groups_seeking_unenumerated_protections, excluded).

% Federal judges and appellate justices who embrace living-constitutionalist methodology (meaning evolves with social understanding) are structurally excluded from the contemporary constitutional conversation when originalists dominate the Supreme Court and appellate benches. Their constitutional arguments are accessible but are systematically dispreferred, their opinions are written in dissent rather than majority, and their judicial appointments are politically contested. They would object to originalism's suppression of evolved understanding and its foreclosure of dignity-based rights, but they lack the institutional power to prevail. Their exit is constrained: they can retire or shift methodology, but changing the Supreme Court's orientation requires political processes outside their control (presidential appointments, Senate confirmation, death or retirement of originalist justices).
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, living_constitutionalist_judiciary, excluded,
    institutional, generational, constrained, national).

% Law professors, legal scholars, and think-tank intellectuals who have built careers and institutional prestige on originalist constitutional theory. They benefit from originalism's institutional dominance: their scholarship gets cited in Supreme Court opinions, their students populate originalist law firms and judiciaries, their conferences attract funding, and their methodological framework is treated as the serious player at the constitutional table. They have substantial arbitrage: they can publish, teach, influence discourse, and build alternative intellectual frameworks. They are not trapped by originalism; they are invested in it.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, legal_academia_originalist_wing, beneficiary,
    organized, generational, arbitrage, national).

% State legislatures occasionally benefit from originalism's restraint on federal constitutional claims against state action, particularly on issues where state legislatures want to preserve traditional authority (education regulation, family law, commerce regulation within state borders). They have arbitrage: they can lobby federal appointments, support originalist candidates, or draft state constitutional amendments to override federal limitations. They are not systematically targeted by originalism; they are situational beneficiaries.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, state_legislatures, beneficiary,
    organized, generational, arbitrage, regional).

% Scholars and analysts who observe the constitutional system from outside, studying how institutional procedures, appointment politics, and methodological choices shape constitutional outcomes. They neither benefit nor pay from originalism; they analyze its operation and its effects. They can exit completely (their work is not dependent on constitutional outcomes) and can shift focus as the system changes.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, proceduralist_institutional_reformers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__originalist_reading, originalist_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_meaning__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originalism coordinates the federal judiciary around a single standard for constitutional interpretation: historical public meaning at ratification. This solves the coordination problem of judicial discretion and interpretation chaos—if all judges are bound to historical meaning, there is a common reference point that, in theory, reduces arbitrary outcome variation and provides predictability. Without originalism (or another unified interpretive methodology), judges would each bring their own substantive constitutional vision, and constitutional meaning would shift with bench composition. Originalism offers a focal point for convergence.
% TRANSFER_FUNCTION: Originalism transfers legitimacy from democratic processes to historical recovery, and it transfers interpretive authority to judges (including judges in the originalist tradition) who control what counts as legitimate historical evidence and methodology. It also transfers the opportunity to expand constitutional protections from contemporary democratic movements and courts sympathetic to evolving understandings, to historical evidence repositories and originalist scholars who reconstruct 18th-century public understanding. In effect, it moves decision-making power from the contemporary democratic arena into the historical-textual-interpretive arena, and it concentrates that power among institutional actors (originalist judges and scholars) who control the interpretive methodology.
% ABSENT_VOICES: Living-constitutionalist judges and scholars, whose voice is excluded when originalists dominate courts. Contemporary rights claimants—LGBTQ persons, racial minorities, immigrants, people with disabilities—whose lived experience and contemporary understanding of rights are explicitly deemed irrelevant to constitutional meaning. Democratic majorities seeking to expand rights, whose preferences are excluded by the constraint that only historical meaning counts. Descendants and voices of the enslaved, indigenous peoples, and those excluded from the 18th-century political community—whose understanding of what rights should mean would challenge originalism's historical referent, but they are doubly excluded (not present in 1788, and their contemporary voices are ruled out as irrelevant to historical public meaning).
% DISAPPEARANCE_RATIONALE: If originalist constraint disappeared overnight, the constitutional landscape would substantially rearrange. Without originalism's gatekeeping, courts would have latitude to recognize unenumerated rights (privacy, dignity, identity), to expand equal protection beyond the Framers' conception, and to develop constitutional protections for harms not contemplated in 1788. Some contemporary rights claimants would gain access to constitutional argument. But contestation exists: originalists would argue that removing the historical constraint would lead to judicial overreach and unmoored constitutional invention (returning to the Warren Court problem they believe they solved). Living constitutionalists and rights advocates would argue that removing originalism would enable courts to catch up with evolving understanding of equality and human dignity. The question of whether the world would rearrange is precisely the foundational disagreement—originalists believe that without historical constraint, courts invent rights and the system becomes unstable; critics believe that without evolution, the Constitution ossifies and becomes unresponsive to changed circumstances and newly recognized harms.
% FOUNDING_PROBLEM: In the 1970s and 1980s, originalists identified the founding problem as judicial overreach: the Warren Court and its successors had been inventing unenumerated rights (privacy, sexual autonomy) and expanding equal protection beyond what the constitutional text explicitly supported, and they did so without grounding their claims in the Constitution's original public meaning. The problem was that judges were imposing their own values and contemporary policy preferences under the guise of constitutional interpretation. Originalism was created to solve this by tying judges to something external—historical meaning—that would constrain their discretion and prevent them from turning the Constitution into a vehicle for whatever contemporary rights advocates wanted.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (judicial overreach of the Warren and Burger eras) is attested to by originalist judges and scholars who lived through it and who cite specific decisions (Roe v. Wade, Griswold v. Connecticut) as paradigm examples. But living-constitutionalists and rights advocates outside the originalist camp dispute both the diagnosis and the remedy: they argue that the Warren Court's decisions were sound constitutional development, not overreach; that the problem originalism identified was not genuine judicial overreach but rather judicial recognition of rights those advocates opposed; and that originalism has not solved a real institutional problem but has instead replaced one form of judicial discretion (Warren Court value-imposition) with another (originalist historical-narrative selection). Non-originalist scholars attest that originalism has simply shifted whose discretion counts and whose outcomes prevail, not eliminated discretion from the system. The testimony about the founding problem comes primarily from originalist advocates; external corroboration from non-originalist institutional voices is mixed or opposite.
narrative_ontology:disappearance_verdict(us_constitution_meaning__originalist_reading, contested).
narrative_ontology:founding_problem_status(us_constitution_meaning__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_meaning__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__originalist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness measurement (0.68 final) reflects originalism's substantial asymmetry: it coordinates around the principle of historical constraint, but that constraint systematically favors some policy outcomes (those consistent with 18th-century understandings) over others (contemporary human rights and dignity claims). The constraint is not neutral across all parties—it benefits those whose substantive preferences happen to align with historical meaning, and it harms those seeking to expand rights beyond what the Framers would have recognized. Suppression is high (0.72) because originalism's enforcement mechanism is doctrinal: non-originalist constitutional arguments (particularly those grounded in evolving conceptions of liberty, equality, and human dignity) are systematically ruled out of order in originalist-dominated courts, not because they lack logical coherence but because they violate the methodological constraint. Theater ratio (0.41) captures the intermediate state: the historical meaning genuinely constrains outcomes in many cases (judges do appear bound by text and history), but the measurement series shows rising theater over time, suggesting that as historical meaning becomes more contested and requires deeper historical investigation, the appearance of constraint increasingly masks discretionary judgment calls about what 'the public' understood in 1788. The measurement trajectory from t=0 to t=40 shows extractiveness, suppression, and theater all rising, indicating that as originalism has consolidated institutional power, both its active enforcement and its theatrical maintenance have intensified.
 *
 * PERSPECTIVAL GAP:
 *   The originalist judge and the counter-majoritarian constraint advocate sit on the beneficiary side: they experience originalism as a genuine constraint that restrains their own policy preferences when historical meaning diverges from contemporary convictions—they believe they are bound. From this seat, the constraint appears as a mountain or genuine rope (fixed meaning genuinely constrains). But from the seat of a contemporary rights claimant seeking to expand constitutional protection (e.g., a same-sex couple in the 2010s seeking marriage rights, or a transgender person seeking dignity protection), originalism appears as active suppression: the historical evidence is weaponized to foreclose arguments that have no historical pedigree, making the judge's invocation of historical constraint feel like post-hoc justification for a predetermined outcome. The engine will compute these seats differently based on the power/exit/directionality data: originalist judges sit at high power with mobile exit (they can leave the bench, can retire, can vote their way out), while rights claimants sit at lower power with constrained or identity-locked exit (they cannot simply leave the jurisdiction, and seeking rights is constitutive of their identity). The directionality derivation will place originalist judges near the beneficiary end (low d, low effective extraction) and rights claimants near the target end (high d, high effective extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary identification is dual: (1) counter-majoritarian constraint advocates benefit from a judicial methodology that appears to bind judges to something external (history) rather than internal (judges' own policy preferences); (2) conservative political interests benefit because originalism's fixation on 18th-century meaning systematically produces outcomes that align with conservative property rights, religious liberty, and limited welfare-state positions. The victim identification is also dual: (1) contemporary rights claimants whose claims lack historical support (LGBTQ persons seeking marriage rights, transgender persons seeking identity recognition, immigrants seeking equal protection, etc.) are foreclosed from constitutional argument in originalist courts; (2) marginalized groups seeking to expand constitutional protection for newly recognized harms are systematically disadvantaged. The directionality for originalist judges should compute as low-to-moderate (they sit at high power, can exit, and believe themselves constrained—all reducing d), while directionality for constrained rights claimants should compute as high (they sit at lower power, have identity-locked exit, and face active suppression of their arguments—all increasing d). The asymmetry is built into the structural data: beneficiaries set and enforce the rule, while victims must accept forelosure of their arguments.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem for originalism is real but contested: in the 1970s-1980s originalists argued that the Warren Court had wandered into unmoored policy-making, inventing constitutional rights not supported by text and history. The founding problem was judicial overreach. But by 2026, that founding problem is substantially dead—the Rehnquist Court and contemporary Supreme Court have been majority-originalist or originalist-sympathetic for decades, and originalism's suppression of non-originalist constitutional development is complete enough to be visible. The problem originalism was created to solve (runaway judicial invention of rights) has either been solved or has transformed: contemporary originalist courts are not inventing rights, but they are also not being constrained by originalism in the direction advocates hoped—instead, originalist methodology is being deployed to foreclose rights expansion in ways that align with conservative policy, suggesting originalism serves as a tool for suppression rather than as a neutral constraint. This is the mandatrophy signal: the founding problem (judicial overreach) is no longer the live threat, but originalism persists, now functioning as suppression machinery rather than as neutral methodological restraint. The constraint should be marked as mandatrophy-candidate: the living founding problem is not judicial invention of rights (that era has passed), but rather the question of whether constitutional meaning should evolve with social understanding—and on that problem, originalism takes an aggressive position (meaning is fixed), but that position is enforced through institutional power, not through the neutral logic of textual constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_meaning_determination_ambiguity,
    'What constitutes ''public meaning at ratification'' when historical sources conflict, are sparse, or were stratified by literacy and participation? Whose public? Which historical moment—the drafting, the ratifying conventions, the state ratifications?',
    'Systematic comparison of originalist outcomes across different ''public meaning'' interpretation methodologies (original public meaning vs. original intent vs. framers'' specific conception); longitudinal study of how originalist judges resolve meaning ambiguities when evidence is genuinely underdetermined.',
    'If historical meaning is genuinely underdetermined at key constitutional moments, originalism reduces to judges selecting from multiple defensible readings that fit the historical constraint—making it functionally a discretionary system that preserves the appearance of constraint while delivering substantial outcome latitude. Classification would shift toward snare (the historical constraint is cover for discretion).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_meaning_determination_ambiguity, empirical, 'Whether historical public meaning is determinate or merely constrains judges within a discretionary range.').

omega_variable(
    meaning_versus_application_boundary,
    'Is the originalist distinction between ''fixed meaning'' and ''evolving application'' coherent, or does application necessarily reshape meaning? Can judges truly apply the Second Amendment''s ''well regulated militia'' clause to contemporary semiautomatic weapons without changing what the clause means?',
    'Detailed analysis of originalist judicial decisions: do judges who claim to hold meaning constant while applying it differently actually change the meaning? Comparison with non-originalist frameworks on identical cases to assess whether the ''application'' framing is merely descriptive cover.',
    'If application necessarily reshapes meaning, the originalist constraint is substantially theatrical—meaning appears fixed while actually drifting. Theater_ratio should be substantially higher (0.6+). The constraint would function more as a snare (preservation of a natural law framing that masks ongoing discretionary development).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meaning_versus_application_boundary, conceptual, 'Whether meaning/application boundary is structurally stable or theater.').

omega_variable(
    suppression_of_alternative_readings_mechanism,
    'How is the suppression of non-originalist constitutional readings maintained? Is it through doctrine (originalist methodology gatekeeping), through institutional power (originalist judges controlling precedent), through epistemic isolation (legal education narrowing exposure to alternatives), or through some combination?',
    'Audit of law school constitutional curricula; analysis of appellate citation patterns; study of how living-constitutionalist and positivist arguments fare when presented within originalist-dominated courts vs. originalist-sparse venues.',
    'If suppression is primarily doctrinal (the methodology itself rules out alternatives), it is a structural feature of the constraint. If it is institutional (power concentration among originalist judges), it is contingent and potentially reversible. A contingent suppression mechanism weakens the ''fixed meaning'' framing and suggests the constraint is actively maintained extraction rather than natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternative_readings_mechanism, empirical, 'Whether suppression of alternatives is structural to originalism or contingent on institutional power distribution.').

omega_variable(
    beneficiary_identity_and_self_interest,
    'Do originalist advocates genuinely believe in the methodology''s historical constraint, or do they adopt it strategically because it protects preferred outcomes (strong property rights, limited welfare provisions, etc.)? Is originalism a reading of the historical record, or is it a post-hoc justification for outcomes that political conservatism prefers?',
    'Longitudinal study of originalist advocates'' positions: if originalism is genuinely constrained by history, originalists should occasionally arrive at outcomes that contradict their preferred policy positions. Measure frequency of such outcomes. Compare with living-constitutionalist outcomes on the same questions.',
    'If originalists rarely reach outcomes that contradict conservative preferences, the constraint is primarily a snare—the historical meaning is selectively discovered to vindicate predetermined outcomes. The beneficiary identification would shift from ''counter-majoritarian constraint advocates'' to ''conservative political interests'' and extraction would be recognized as substantially higher (0.85+).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_and_self_interest, empirical, 'Whether originalism constrains outcomes or whether it is outcome-driven selection of historical narratives.').

omega_variable(
    reading_foreclosure_sibling_relationship,
    'Does originalism''s core axiom (meaning fixed at ratification by historical public meaning) logically foreclose living constitutionalism (meaning endures but application evolves), or do they coexist as genuinely different normative commitments held by different institutional factions?',
    'Logical analysis: can a single adjudication framework hold both that meaning is determined by 18th-century public evidence AND that meaning evolves with contemporary understanding? Or are these incompatible commitments that any coherent framework must choose between?',
    'If originalism forecloses living constitutionalism (a framework cannot hold both), the relation is ''forecloses'' and the kernel exhibits genuine incompatibility structure. If both can coexist within the same framework (different judges, different cases, cohabiting institutional structure), the relation is ''coexists_with''. This determines whether the constitutional system is a unified framework with an internal contradiction, or a genuinely plural system hosting incompatible readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_sibling_relationship, conceptual, 'Whether originalism and living constitutionalism are logically foreclosing or coexisting readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__originalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__originalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_meaning__originalist_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement_basis(us_c_tr_t10, observed).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_meaning__originalist_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(us_c_tr_t20, observed).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_meaning__originalist_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement_basis(us_c_tr_t30, observed).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_meaning__originalist_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(us_c_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__originalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t10, us_constitution_meaning__originalist_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(us_c_be_t10, observed).
narrative_ontology:measurement(us_c_be_t20, us_constitution_meaning__originalist_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(us_c_be_t20, observed).
narrative_ontology:measurement(us_c_be_t30, us_constitution_meaning__originalist_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement_basis(us_c_be_t30, observed).
narrative_ontology:measurement(us_c_be_t40, us_constitution_meaning__originalist_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(us_c_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__originalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t10, us_constitution_meaning__originalist_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(us_c_su_t10, observed).
narrative_ontology:measurement(us_c_su_t20, us_constitution_meaning__originalist_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement_basis(us_c_su_t20, observed).
narrative_ontology:measurement(us_c_su_t30, us_constitution_meaning__originalist_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(us_c_su_t30, observed).
narrative_ontology:measurement(us_c_su_t40, us_constitution_meaning__originalist_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(us_c_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_meaning__originalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__positivist_reading).

% DUAL FORMULATION NOTE:
% The contested kernel 'us_constitution_meaning' decomposes into three constraint stories corresponding to three major readings: originalism (this constraint), living constitutionalism, and positivism. Each reading instantiates a different constraint because each has a different ε (originalism high at 0.68 because it systematically forecloses rights claims; living constitutionalism lower because it preserves argument space), different beneficiary structures, and different suppression profiles. The three constraints are linked via network.affects_constraints to show the family relationship and the fact that originalist institutional dominance directly constrains the other readings' feasibility in federal courts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_meaning__originalist_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
