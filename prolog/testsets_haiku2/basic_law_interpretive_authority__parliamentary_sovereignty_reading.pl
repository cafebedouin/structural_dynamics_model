% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__parliamentary_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty Constitutional Interpretation Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint instantiates the parliamentary sovereignty reading of the
 *   contested kernel 'basic_law_interpretive_authority.' Under this reading,
 *   the elected legislature holds final interpretive authority over
 *   constitutional meaning by virtue of its democratic mandate and
 *   representative accountability. Courts are positioned as interpreters who
 *   apply law made by the legislature, not as co-equal arbiters of
 *   constitutional meaning. When the legislature and courts disagree on
 *   constitutional interpretation, the legislature's reading prevails through
 *   legislative override, appointment of compliant judges,
 *   jurisdiction-stripping, or constitutional amendment. The reading treats
 *   democratic election as the primary source of constitutional legitimacy
 *   and judicial independence as a subordinate good. Sibling readings
 *   (judicial supremacy, popular constitutionalism) contest this framing: one
 *   locates authority in courts; another locates it in ongoing democratic
 *   contestation outside institutional terminals. This story authors the
 *   parliamentary sovereignty reading as a clean, ε-invariant constraint with
 *   its own beneficiaries, victims, and institutional dynamics.
 *
 * KEY AGENTS:
 *   - elected_legislature: Primary beneficiary and agenda-setter (holds final interpretive authority through override and appointment power)
 *   - judicial_independence: Primary victim (structurally subordinated to legislative will; retains only authority legislature permits)
 *   - rights_minorities: Primary victim (depend on judicial protection that can be overridden by legislature; electoral voice is weak)
 *   - separated_powers_doctrine: Subordinated principle (treated as procedural guideline, not co-equal constitutional constraint)
 *   - electoral_constituency: Secondary beneficiary (government directly responsive to electoral preference without judicial check)
 *   - international_rights_bodies: Excluded from interpretive authority (no standing within the frame)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.68).
domain_priors:suppression_score(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.71).
domain_priors:theater_ratio(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "Parliamentary Sovereignty Constitutional Interpretation Reading").
narrative_ontology:topic_domain(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "constitutional/political").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'f47b978a-2731-481a-bb8e-a3afb4a76c2b').
narrative_ontology:cs_kernel_codification('f47b978a-2731-481a-bb8e-a3afb4a76c2b', fixed_text).
narrative_ontology:cs_authority_grounding('f47b978a-2731-481a-bb8e-a3afb4a76c2b', extraction).
narrative_ontology:cs_interpretation_layer_present('f47b978a-2731-481a-bb8e-a3afb4a76c2b').
narrative_ontology:cs_reading_relation('f47b978a-2731-481a-bb8e-a3afb4a76c2b', basic_law_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('f47b978a-2731-481a-bb8e-a3afb4a76c2b', basic_law_interpretive_authority__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('f47b978a-2731-481a-bb8e-a3afb4a76c2b', foundational, electoral_legitimacy_constitutes_authority).
narrative_ontology:cs_axiom_status(electoral_legitimacy_constitutes_authority, holdable).
narrative_ontology:cs_axiom_grounding('f47b978a-2731-481a-bb8e-a3afb4a76c2b', electoral_legitimacy_constitutes_authority, deontological).
narrative_ontology:cs_axiom('f47b978a-2731-481a-bb8e-a3afb4a76c2b', secondary, majority_rule_primacy).
narrative_ontology:cs_axiom_status(majority_rule_primacy, holdable).
narrative_ontology:cs_axiom_grounding('f47b978a-2731-481a-bb8e-a3afb4a76c2b', majority_rule_primacy, instrumental).
narrative_ontology:cs_reference_frame('f47b978a-2731-481a-bb8e-a3afb4a76c2b', legislative_constitutional_supremacy).
narrative_ontology:cs_drift_state('f47b978a-2731-481a-bb8e-a3afb4a76c2b', contemporary_judicial_assertiveness_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f47b978a-2731-481a-bb8e-a3afb4a76c2b', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, executive_accountable_to_legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_independence).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, separated_powers_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_review_institution).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, electoral_constituency).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_review_institution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the interpretive frame through legislation, judicial appointment, budget control, and (in many systems) ability to amend the constitution or change the court structure. Enforces parliamentary sovereignty by overriding adverse judicial decisions and reshaping the judiciary's composition and jurisdiction. Benefits directly from the constraint's operation: unambiguous authority, no need to negotiate with courts over constitutional meaning, consolidated power. Faces minimal cost; the cost of maintaining the fiction that legislatures are interpreting, not making, the constitution is low.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% The institutional and structural capacity of courts to reach decisions independently of legislative or executive pressure. Under the parliamentary sovereignty reading, judicial independence is systematically subordinated: courts understand their role as interpretation (applying laws the legislature made) rather than arbitration (checking the legislature). When legislatures override decisions, threaten jurisdiction-stripping, or signal that certain judges will not be retained, judicial independence erodes. The constraint treats judicial independence as a permission grant from the legislature, not as a structural requirement. Independence is subordinated to electoral legitimacy.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_independence, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_independence).

% Groups whose interests are systematically disadvantaged by majoritarian legislation (religious minorities, ethnic minorities, unpopular groups, those with disability requiring accommodation that majorities prefer not to fund). Under the parliamentary sovereignty reading, their protection rests on legislatures enacting protective legislation—the same institutions that enacted the disadvantaging laws. Their recourse against legislative majorities is limited to electoral pressure (they are minorities, so electoral leverage is weak) or to seeking judicial protection (which is structurally subordinated to legislative override). Exit is impossible. Voice is weak by structural design of majoritarian systems.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities, payer,
    powerless, biographical, trapped, national).

% The constitutional principle that legislative, executive, and judicial powers should be dispersed and checked by each other—a co-equal constraint principle in many constitutional theories. Under the parliamentary sovereignty reading, this principle is treated as subordinate to majoritarian legitimacy. The legislature checks the courts, but the courts lack reciprocal check against the legislature (or their check is exercised only with legislative tolerance). The separation-of-powers principle is not abandoned but reinterpreted as a procedural guideline rather than a structural requirement.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, separated_powers_doctrine, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(basic_law_interpretive_authority__parliamentary_sovereignty_reading, separated_powers_doctrine).

% Benefits from having a clear institutional mandate (interpret law, apply it, do not make it) and from legislative validation of its role. Does not compete with legislatures for supremacy; has a defined sphere. Pays the cost of subordination: cannot develop independent constitutional reasoning; must accept legislative override; faces appointment and discipline from legislatures. The benefit is institutional survival and clarity; the cost is subordinate status.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_review_institution, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_review_institution, payer).

% The voters who elect legislatures benefit from government directly accountable to electoral pressure. Legislatures pass laws the constituency votes for; courts enforce those laws, not some other constitutional doctrine that majorities voted against. The benefit is democratic responsiveness: government follows the will of the (voting) people. The cost (not borne equally by all constituencies) falls on those whose interests the majority votes to disadvantage—diffuse for the electoral majority, concentrated for minorities.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, electoral_constituency, beneficiary,
    organized, biographical, mobile, national).

% In parliamentary systems, the executive depends on legislative confidence; in presidential systems, the legislature controls the executive's budget and can impeach. Under parliamentary sovereignty reading, the executive benefits from judicial restraint—courts cannot restrain executive action that the legislature permits or enacts. The executive's accountability is to the legislature, not to courts. Benefits from consolidated authority; pays the cost of political dependence on the legislature.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, executive_accountable_to_legislature, beneficiary,
    institutional, generational, constrained, national).

% Study the origins and evolution of constitutional arrangements. Witness competing narratives about the founding: whether judicial usurpation (the parliamentary sovereignty founding story) or judicial UNDERenforcement (the judicial supremacy founding story) was the actual historical problem. Do not stake institutional survival on either reading but analyze how each reading selects and interprets historical evidence to legitimize itself.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_historians, observer,
    moderate, generational, mobile, national).

% International bodies (UN human rights committees, European Court of Human Rights, regional courts) monitor compliance with international human rights law. Within the bounded frame of the parliamentary sovereignty reading (national legislative supremacy), these bodies have no enforcement power: a legislature can violate international human rights commitments and override international court rulings through domestic legislation. International monitors are excluded from the interpretive conversation by the territorial and institutional scope of the reading. They can remonstrate but cannot override.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, international_human_rights_regime, excluded,
    powerful, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides unambiguous institutional location for constitutional interpretation: the legislature. Solves coordination problem of 'where does final authority reside' by answering: the elected branch. Eliminates gridlock from competing institutional claims to constitutional supremacy (no court-versus-legislature deadlock). Ensures constitutional meaning can be changed through the same democratic process that changes ordinary law—responsive to electoral pressure.
% TRANSFER_FUNCTION: Transfers the power to interpret and reshape constitutional meaning from courts (who claim expertise-based authority) to legislatures (who claim electoral authority). Transfers also the power to override adverse constitutional decisions, discipline courts, reshape the judiciary's composition and jurisdiction. Transfers the burden of minority rights protection from guaranteed judicial review to the electoral process, where minorities have structurally weak voice.
% ABSENT_VOICES: Judicial independence advocates (courts themselves, separation-of-powers theorists, judicial independence organizations) would reject this reading if they had equal voice: they argue that unchecked legislatures become tyrannical majorities and judicial independence is essential to constitutional fidelity. Rights-minority advocates would argue that electoral protection is insufficient and that stable, judicially enforced rights are necessary to prevent majoritarian oppression. International human rights bodies would argue that national legislative supremacy enables human rights violations without external accountability. These voices are structurally excluded by the frame that privileges electoral democracy as the sole legitimacy source.
% DISAPPEARANCE_RATIONALE: If parliamentary sovereignty as the interpretive principle disappeared (replaced by judicial supremacy or popular constitutionalism), the constitutional and institutional arrangement would reorganize: courts would gain independent authority to declare laws unconstitutional and refuse to apply them without legislative override. Rights-minorities would gain access to stable protection outside electoral cycles. Legislatures would lose unilateral authority to reshape constitutional meaning. The legislative-executive axis would face new judicial constraints. Institutional authority would disperse.
% FOUNDING_PROBLEM: Judicial overreach and usurpation: courts interpreting vague constitutional language (fundamental rights, due process, equal protection) as license to impose judge-made law and frustrate legitimate legislative will. The founding problem as this reading frames it is that courts lacking democratic accountability can frustrate the will of the people as expressed through their elected representatives, creating gridlock and imposing substantive law under the guise of interpretation.
% FOUNDING_PROBLEM_CORROBORATION: Legislatures and executive branches defending their authority attest the problem is live: courts interpret broadly, constrain legislation, and frustrate electoral mandates. Judicial independence advocates and constitutional scholars attest the problem is either overstated or misdescribed: courts actually exercise great restraint; the real problem is judicial UNDERenforcement of rights (minorities do not get protection they need). Comparative constitutional historians attest that different democracies have experienced different pathologies—some genuinely suffered judicial overreach, others suffered legislative supremacy enabling atrocities. Democratic theory scholars attest the contest is real: there is no consensus on which problem (judicial overreach or legislative supremacy) is the genuine threat. No corroboration from outside the competing institutional seats.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) reflects the transfer of interpretive authority from courts to legislature: legislatures extract the power to say what the constitution means without independent judicial check. The transfer is asymmetric—legislatures gain unilateral override capability while courts lose interpretive autonomy. Suppression (0.71) is high because the constraint's persistence depends on actively suppressing judicial independence (through composition pressure, jurisdiction-stripping, and delegitimizing language about 'judge-made law'). Theater (0.42, moderate) reflects that legislatures do maintain formal respect for constitutional language and judicial roles—they do not openly claim unlimited power—but an increasing share of activity is devoted to managing courts' willingness to defer rather than engaging genuine constitutional interpretation. The measurement series shows extractiveness rising to asymptote at mid-interval (legislative authority settles around ≈0.67-0.68 after early consolidation), suggesting the constraint reaches a stable equilibrium where legislatures have successfully established override capability and courts have internalized subordination. Theater remains flat and moderate—neither rising toward piton (where performance would dominate) nor falling (where courts would regain visible independence). Suppression plateaus at ≈0.71, consistent with stable-state maintenance of judicial deference. The constrained (not volatile) trajectory indicates this reading, once institutionalized, produces a stable equilibrium rather than ongoing contestation.
 *
 * PERSPECTIVAL GAP:
 *   The elected legislature and the judicial system compute radically different types from the same structural facts. From the legislature's seat, the constraint is genuine coordination: unambiguous authority, democratic legitimacy, clear lines of accountability. Courts and rights-minority seats perceive the same constraint as extractive subordination: they have lost autonomy without gaining offsetting benefit. The separated powers doctrine seat perceives it as a constitutional reading that privileges one principle (majority rule through election) over a co-equal principle (institutional checks). The engine computes per-seat directionality from the beneficiary/victim declarations and power differentials: the legislature (institutional power, arbitrage-level exit) sits at low d; rights-minorities (powerless, trapped exit) sit at high d. This divergence is the core measurement the framework exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected legislature benefits from unambiguous interpretive supremacy (no need to negotiate with courts, direct enforcement through override). Its directionality is low (near full beneficiary, d ≈ 0.1-0.2): it controls the rule, defines what the constitution means, can reshape the judiciary at will, faces no judicial check. Judicial independence and rights-minorities are targets: they lose structural autonomy without compensating benefit. Judicial independence is trapped (cannot exit the constitutional frame) and its subordination is structural; d is high (≈0.75-0.85). Rights-minorities are powerless (no institutional seat) and trapped (cannot exit; voice limited to electoral cycles that do not prioritize them); their d is highest (≈0.85-0.95). The electoral constituency sits near symmetric (d ≈ 0.5): they get responsive government but bear diffuse cost in rights erosion. Separated powers doctrine is trapped and its subordination is permanent within this reading; it functions as a victim despite being non-agent. The directionality spread (legislature near beneficiary pole, minorities near target pole) is the structural engine of effective extraction even though base ε (0.68) is moderate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (judicial usurpation, gridlock) would have been 'live' at the moment of constitutional founding if the judicial institution existed and was indeed overreaching. The parliamentary sovereignty reading resolves that problem by subordinating courts to legislatures: courts no longer gridlock legislative action; they enforce what legislatures say the constitution means. However, the very mechanism that solves the judicial-overreach problem creates a new problem: rights-protection through courts becomes unreliable. The constraint is a tangled_rope (genuine coordination problem solved + asymmetric extraction realized through the same mechanism). The theater ratio plateaus at moderate levels (0.42) because legislatures and courts maintain elaborate justifications for the arrangement (constitutional fidelity, interpretive restraint) that are not entirely performative—legislatures do pay some cost in maintaining constitutional language—but increasingly divorced from courts' actual decision-making autonomy. Mandatrophy potential is modest: the constraint is actively maintained by legislatures with direct incentive to enforce it. Were the founding problem genuinely solved and legislatures no longer needed the override mechanism, theater_ratio would rise (maintenance would become pure performance). Currently, the moderate-flat trajectory suggests the constraint is still substantially functional for its beneficiaries; it is not yet a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_empirical_status,
    'Was judicial overreach (the founding problem as this reading frames it) a genuine, widespread historical phenomenon, or a selective narrative used to justify legislative supremacy?',
    'Empirical analysis of judicial decisions in the interval: what percentage of legislative statutes did courts actually strike down? Did those strikes cluster in particular domains? Did legislatures perceive itself as constrained in practice, or primarily in theoretical rhetoric? Comparative constitutional history across democracies.',
    'If judicial overreach was rare or concentrated in unrepresentative cases, the founding problem is overstated, and the constraint''s framing as responding to a genuine coordination failure is weakened. The constraint recasts as pure legislative power-consolidation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_empirical_status, empirical, 'Whether judicial review actually posed the threat this reading claims it did.').

omega_variable(
    rights_protection_substitutability,
    'Can rights-minorities access substantive protection through the electoral process (legislation, constitutional amendment by majority vote) as effectively as through judicial enforcement, or is electoral protection fundamentally weaker?',
    'Comparative case studies: jurisdictions where minorities have attempted rights-protection through legislative supermajority requirements vs. through judicial enforcement. Track outcome success rates, implementation fidelity, and persistence over time.',
    'If electoral protection is as effective as judicial protection, the constraint imposes no net cost on minorities; rights-victims are mislabeled. If electoral protection is substantially weaker (minorities are systematically out-voted), the constraint is extractive from minorities regardless of legislative benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rights_protection_substitutability, empirical, 'Whether majoritarian democracy protects minorities as well as constitutionalism with judicial enforcement.').

omega_variable(
    reading_contest_structure,
    'Do the three readings of this kernel (parliamentary sovereignty, judicial supremacy, popular constitutionalism) actually coexist as live positions held by different democratic constituencies, or does one reading functionally dominate and the others serve only as theoretical alternatives?',
    'Political-institutional mapping: in contemporary democracies claiming all three readings as intelligible, which reading does the actual institutional structure instantiate? How stable is the arrangement? When readings collide (e.g., elected legislature overrides judicial constitutional decision), which reading''s authority actually prevails in practice?',
    'If one reading dominates in practice, the contest is not structurally live; the kernel is not genuinely contested. The parliamentary sovereignty reading would then be the instantiated constraint, and the others would be aspirational alternatives rather than coexisting readings. This changes the omega classification from conceptual (different readings) to empirical (which arrangement actually operates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_structure, conceptual, 'Whether the kernel admits genuinely coexisting readings or resolves to one dominant reading in practice.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured judicial deference (suppression = 0.71) structural (judges fear retaliation: appointment pressure, jurisdiction-stripping, budgetary cuts) or internalized (judges have internalized the norm that legislatures should be supreme and enforce it voluntarily)?',
    'Jurisdictional natural experiments: when jurisdiction-stripping threats are removed (e.g., through reform or change of legislative party) do judges immediately reassert independent reasoning, or does deference persist? Survey judicial cognition: do judges describe their behavior as constrained externally or as reflecting genuine constitutional belief in legislative supremacy?',
    'If suppression is purely structural, courts could regain independence rapidly if legislatures changed course. If suppression is internalized, courts have incorporated the reading into their judicial philosophy; independence would take generational change. This affects the constraint''s reversibility and the costs of its removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether judicial subordination to legislatures is enforced through external pressure or internalized belief.').

omega_variable(
    sibling_reading_logical_status,
    'Can a single institutional framework coherently hold both parliamentary sovereignty and judicial supremacy as co-equal principles, or do they logically foreclose each other?',
    'Formal constitutional analysis: attempt to construct a coherent institutional mechanism where both courts and legislatures hold ''final'' authority. If the mechanism requires adjudicating conflicts between the two authorities, which authority decides? Does the framework implicitly defer to one over the other, effectively collapsing into a single reading?',
    'If the readings logically foreclose each other, the cs_structure.reading_relations should list ''forecloses'' (rare). If they can coexist (one reading controls in one domain, another in another), they ''coexist_with.'' If the parliamentary sovereignty reading creates structural pressure that makes judicial supremacy harder to maintain (but not impossible), the relation is ''influences.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_logical_status, conceptual, 'Whether the two readings of who holds final authority are logically compatible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(basi_tr_t5, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 5, 0.39).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement(basi_tr_t15, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(basi_tr_t25, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(basi_be_t5, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(basi_be_t15, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(basi_be_t25, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement(basi_su_t5, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 5, 0.67).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(basi_su_t15, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(basi_su_t25, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, legislative_override_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_appointment_political_influence).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, minority_rights_protection_through_courts).

% DUAL FORMULATION NOTE:
% This constraint is ONE reading of the kernel 'basic_law_interpretive_authority.' The sibling readings are authored as separate constraint stories: judicial_supremacy_reading (ε ≈ 0.50, mountains judges as primary beneficiary) and popular_constitutionalism_reading (ε ≈ 0.45, diffuse institutional authority). All three readings share the same kernel (the contested question of interpretive authority) but produce different ε values, different beneficiary/victim sets, and different types because they make different empirical and normative claims about what the basic law requires. The ε-invariance principle (DP-001) requires separate stories; linking via network.affects_constraints models that these readings influence each other's operation (e.g., if parliamentary sovereignty becomes institutionalized, it creates pressure on courts that affects the viability of the judicial supremacy reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_authority__parliamentary_sovereignty_reading, institutional, 0.15).
constraint_indexing:directionality_override(basic_law_interpretive_authority__parliamentary_sovereignty_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
