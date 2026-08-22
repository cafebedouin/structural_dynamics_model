% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__coordinate_construction_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__coordinate_construction_reading
 *   human_readable: Constitutional Authority Distributed Across Branches (Coordinate Construction Reading)
 *   domain: constitutional law / political theory
 *
 * SUMMARY:
 *   This constraint instantiates the COORDINATE CONSTRUCTION READING of
 *   constitutional interpretive authority — the claim that no single branch
 *   possesses final say over constitutional meaning, and that the
 *   constitution's actual meaning emerges from inter-branch dialogue,
 *   political contestation, legislative override, appointment cycles, and the
 *   amendment process. Under this reading, constitutional authority is
 *   intentionally distributed; interpretive disputes are resolved through
 *   politics (budgets, appointments, jurisdiction stripping, amendment)
 *   rather than through a supreme adjudicator. The coordinate model tolerates
 *   higher interpretive instability as the price of preventing unilateral
 *   power concentration. This constraint is ONE of three competing readings
 *   of the same kernel (constitutional interpretive authority); the sibling
 *   readings are judicial supremacy (courts have final say) and parliamentary
 *   supremacy (legislatures have final say). This story models the coordinate
 *   reading as a distinct, internally coherent constraint with its own
 *   extraction profile, beneficiaries, and victims.
 *
 * KEY AGENTS:
 *   - judiciary_institutional_seat: One branch; interprets via adjudication but subject to override
 *   - legislature_institutional_seat: One branch; enacts law, controls amendment and appointments
 *   - executive_institutional_seat: One branch; enforces but lacks independent voice
 *   - stable_precedent_dependent_actors: Powerful interests harmed by interpretive instability
 *   - minority_rights_holders_without_supermajority: Powerless groups whose rights depend on political will
 *   - political_contestation_space: The domain of inter-branch debate itself (non-agent beneficiary)
 *   - constitutional_amendment_coalition: Organized coalitions mobilized by interpretive disputes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__coordinate_construction_reading, 0.58).
domain_priors:suppression_score(constitutional_interpretive_authority__coordinate_construction_reading, 0.42).
domain_priors:theater_ratio(constitutional_interpretive_authority__coordinate_construction_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__coordinate_construction_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__coordinate_construction_reading, "Constitutional Authority Distributed Across Branches (Coordinate Construction Reading)").
narrative_ontology:topic_domain(constitutional_interpretive_authority__coordinate_construction_reading, "constitutional law / political theory").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__coordinate_construction_reading, 'ad4d266e-7aa0-46bc-9f83-4dc993dd0b83').
narrative_ontology:cs_kernel_codification('ad4d266e-7aa0-46bc-9f83-4dc993dd0b83', distributed).
narrative_ontology:cs_authority_grounding('ad4d266e-7aa0-46bc-9f83-4dc993dd0b83', distributed).
narrative_ontology:cs_reading_relation('ad4d266e-7aa0-46bc-9f83-4dc993dd0b83', constitutional_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('ad4d266e-7aa0-46bc-9f83-4dc993dd0b83', constitutional_interpretive_authority__parliamentary_supremacy_reading, coexists_with).
narrative_ontology:cs_axiom('ad4d266e-7aa0-46bc-9f83-4dc993dd0b83', foundational, no_final_interpretive_authority).
narrative_ontology:cs_axiom_status(no_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('ad4d266e-7aa0-46bc-9f83-4dc993dd0b83', no_final_interpretive_authority, deontological).
narrative_ontology:cs_axiom('ad4d266e-7aa0-46bc-9f83-4dc993dd0b83', foundational, amendment_as_live_remedy).
narrative_ontology:cs_axiom_status(amendment_as_live_remedy, holdable).
narrative_ontology:cs_axiom_grounding('ad4d266e-7aa0-46bc-9f83-4dc993dd0b83', amendment_as_live_remedy, instrumental).
narrative_ontology:cs_axiom('ad4d266e-7aa0-46bc-9f83-4dc993dd0b83', secondary, inter_branch_dialogue_constitutes_authority).
narrative_ontology:cs_axiom_status(inter_branch_dialogue_constitutes_authority, holdable).
narrative_ontology:cs_axiom_grounding('ad4d266e-7aa0-46bc-9f83-4dc993dd0b83', inter_branch_dialogue_constitutes_authority, conventional).
narrative_ontology:cs_reference_frame('ad4d266e-7aa0-46bc-9f83-4dc993dd0b83', separated_powers_mutual_constraint).
narrative_ontology:cs_drift_state('ad4d266e-7aa0-46bc-9f83-4dc993dd0b83', contemporary_polarization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ad4d266e-7aa0-46bc-9f83-4dc993dd0b83', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, political_contestation_space).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_amendment_coalition).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, stable_precedent_dependent_actors).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, minority_rights_holders_without_supermajority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, judiciary_institutional_seat).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, legislature_institutional_seat).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, executive_institutional_seat).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitutional text in concrete cases; decisions bind the parties before it and influence future lower-court rulings. But the judiciary's authority is constrained: it lacks enforcement power (depends on executive compliance), can be overridden by legislative amendment or jurisdiction-stripping, subject to appointment turnover that reshapes doctrine, and depends on legislative budget appropriation. Acts as authoritative interpreter within its domain while being subject to political check.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, judiciary_institutional_seat, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, judiciary_institutional_seat, payer).

% Enacts statutes that courts review and can nullify; can amend the constitution (with supermajority) to override court rulings; controls the judiciary's budget and can strip jurisdiction; can shape the judiciary through appointments during vacancies. Coordinates policy-making while constrained by constitutional supremacy doctrine and judicial veto.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, legislature_institutional_seat, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, legislature_institutional_seat, payer).

% Enforces court orders and legislative enactments; proposes nominees for judicial vacancies; takes executive actions later reviewed by courts; negotiates with legislature on constitutional interpretation in specific policy domains. Has the least independent interpretive voice of the three branches and faces constraints from both.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, executive_institutional_seat, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, executive_institutional_seat, payer).

% Corporations, financial institutions, states, and long-term investors that have organized infrastructure, contracts, and plans around settled judicial interpretations. Property law, commercial law, regulatory compliance, and interstate commerce all depend on consistent precedent. When constitutional authority is dispersed across branches, precedent faces cyclical destabilization: legislatures override through amendment or jurisdiction-stripping, appointment-driven doctrinal shifts reverse settled law, and political pressure can reinterpret established meaning. Exit is constrained: moving infrastructure to other jurisdictions is costly and may not escape the US constitutional system's reach.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, stable_precedent_dependent_actors, payer,
    powerful, generational, constrained, national).

% Groups whose fundamental rights (freedom of speech, conscience, equal protection, voting rights) depend on constitutional protection but lack the legislative or amendment supermajority to embed those protections securely. Under coordinate construction, their rights are subject to ongoing political contestation: legislatures can pass laws restricting those rights, courts interpret those rights narrowly or broadly depending on appointments, and amendment campaigns can narrow constitutional protections. Exit is identity-locked: leaving the national polity is not a realistic option for most groups; they are bound by citizenship or residency.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, minority_rights_holders_without_supermajority, payer,
    powerless, biographical, identity_locked, national).

% The domain of constitutional debate itself — the space where constitutional meaning is negotiated and renegotiated across branches, in legislatures, through appointment politics, and via amendment campaigns. Dispersed interpretive authority means this space is OPEN and continuously activated: no single institutional seat can close interpretation against political pressure. The constraint benefits the existence and vibrancy of this contestation space.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, political_contestation_space, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(constitutional_interpretive_authority__coordinate_construction_reading, political_contestation_space).

% Ad-hoc coalitions mobilized around constitutional amendment campaigns when unhappy with judicial or legislative interpretation. They benefit from a system in which amendment is a live remedy for interpretive disputes, not preempted by a supreme interpreter claiming closure and discouraging amendment efforts. Coalition membership is mobile: individuals enter when motivated by a specific amendment campaign and exit once the campaign succeeds or fails.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_amendment_coalition, beneficiary,
    organized, generational, mobile, national).

% Actors, doctrines, and institutional designs that benefit from stable, closure-granting constitutional authority — a single voice that says 'this is what the constitution means, finally.' They advocate for judicial supremacy or parliamentary supremacy because both offer interpretive closure and stable precedent. Under coordinate construction, they are structurally excluded from being beneficiaries because the model itself denies the legitimacy of their core preference: final interpretive authority. They argue (from outside this reading) that the coordinate model is unstable and illegitimate.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, interpretive_stability_constituencies, excluded,
    powerful, generational, constrained, national).

% Foreign governments and international institutions that rely on stable US constitutional commitments (human rights treaties, property protections, trade obligations). Dispersed US constitutional authority creates interpretive risk: today's judicial reading may be tomorrow's legislative override, rendering long-term reliance hazardous. They cannot exit (treaties bind through international law) and have no voice in US constitutional debates.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, international_coordination_dependents, excluded,
    powerful, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__coordinate_construction_reading, political_contestation_space).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__coordinate_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes constitutional interpretation across three institutional seats (judiciary, legislature, executive) to prevent any single seat from claiming unilateral interpretive authority. Coordinates dispute resolution through inter-branch dialogue, amendment, appointment, and political override rather than through supreme adjudication.
% TRANSFER_FUNCTION: Transfers interpretive stability and closure from actors dependent on settled law (stable-precedent actors, minority rights holders) to the political contestation space, where constitutional meaning is continuously renegotiated through appointment politics, legislative override, and amendment campaigns.
% ABSENT_VOICES: Interpretive stability advocates who would prefer judicial or parliamentary supremacy are structurally excluded from the coordinate model because the model itself denies their core claim: that constitutional meaning should be finally settled by a single authoritative voice. They are excluded not by fiat but by logical incompatibility with the reading's foundational axiom (no_final_interpretive_authority). International coordination dependents are also excluded: they would benefit from a single voice that could bind the US to stable commitments, but coordinate construction means those commitments are always politically contestable.
% DISAPPEARANCE_RATIONALE: If the coordinate construction constraint vanished — if one branch claimed and secured final interpretive authority — the landscape would reorganize around that single seat's rulings. Either courts would issue final interpretations (amendment would become remedy only for explicit text change), legislatures would claim supremacy (courts would lack veto power), or executives would claim interpretive voice (constitutional law would merge with administrative decision-making). The political contestation space over meaning would shrink; interpretation would close.
% FOUNDING_PROBLEM: Preventing tyranny of a single interpretive authority. The founding problem was structural: how to prevent any one branch (especially the executive) from claiming unilateral power to say what the constitution means and enforce that meaning without check by the other branches.
% FOUNDING_PROBLEM_CORROBORATION: The coordinate reading attests the founding problem is live: contemporary debates over judicial activism, executive overreach, and legislative nullification show ongoing concern about unilateral interpretive power. However, the judicial-supremacy reading attests that the problem HAS BEEN SOLVED: courts, by assuming the role of guardian of fundamental rights and constitutional text, remove the risk of tyranny from the executive and legislature, and establish predictable meaning. The parliamentary-supremacy reading attests a different solution: the elected legislature, accountable to voters, is the appropriate seat to resolve constitutional meaning; courts usurping this role creates a different tyranny (rule by unelected judges). Outside the benefiting parties: historians and political scientists note that the founding problem was PARTIALLY solved by the Constitution's structure; none of the three branches achieved unilateral power in practice. Contemporary conflicts (appointment politics, legislative override, amendment campaigns) suggest the problem has NOT disappeared but has been redirected into political contestation. International law scholars note that other democracies have avoided the same problem by explicitly granting final interpretive authority to constitutional courts (thereby accepting 'rule by judges' as preferable to constant contestation).
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__coordinate_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__coordinate_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__coordinate_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__coordinate_construction_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__coordinate_construction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is MODERATE-HIGH (0.58 at interval end) because the constraint extracts stable interpretive authority from those who depend on judicial closure, transferring that security to the political contestation space. Minority rights holders pay a particularly high cost: their fundamental protections are subject to legislative override and political cycles rather than to a supreme guardian. Suppression is MODERATE (0.42): the constraint does not require coercive machinery to enforce (unlike a snare or authoritarian system); it persists because the branches mutually constrain each other through constitutional design and because political actors benefit from the contestation space it opens. Theater ratio is MODERATE (0.48): some activity is genuine inter-branch dialogue and constitutional reasoning (real coordination function), but a growing share is ritualized positioning and rhetorical theater as actors perform their roles in the amendment/appointment/override game. The measurement series show SLIGHT OSCILLATION: extractiveness and theater rise through mid-interval (t=0 to t=24) as political contestation intensifies and appointment politics become more polarized, then SLIGHTLY DECLINE (t=24 to t=40) as the system stabilizes around new equilibrium (recent appointments have settled, amendment becomes less salient). This oscillation pattern reflects the cyclical nature of constitutional politics: periods of high contestation followed by relative stability, then renewal. The oscillation itself is NOT extraction (it is not systematic wealth transfer) but INSTABILITY (the alternating reassurance and uncertainty that comes from distributed authority). Suppression requirement tracks theater ratio loosely: as the theatrical component rises, slightly more enforcement is needed to keep the system stable (enforcement = the constitutional norms that prevent any branch from simply overriding the others without formal process).
 *
 * PERSPECTIVAL GAP:
 *   JUDICIARY SEAT: Experiences this constraint as a limit on its authority — it can interpret authoritatively within its domain, but that authority is not final; political branches can override through amendment, jurisdiction stripping, and appointment. The cost is instability; the benefit is that courts avoid the accusation of tyranny. LEGISLATURE SEAT: Experiences it as both benefit (can override courts through ordinary legislation, amendment, appointment) and constraint (cannot simply enact whatever it wants; courts will review and nullify). The constrained exit reflects constitutional supremacy doctrine. STABLE PRECEDENT ACTORS: Experience it as pure extraction — they have organized around settled law, and the coordinate model means that law is always politically contestable. They would prefer judicial supremacy (courts as final interpreter, amendment as rare remedy). MINORITY RIGHTS HOLDERS: Experience it as extraction — their rights are only as secure as the political coalitions defending them. They would prefer judicial supremacy (courts as guardian against majoritarian override). AMENDMENT COALITIONS: Experience it as benefit — the constraint opens the possibility of constitutional contestation and amendment campaigns; without distributed authority, amendment becomes a dead letter. The engine should compute DIFFERENT types from different seats: judiciary and legislature likely compute as TANGLED ROPE (both coordinate and extract, both constrained); stable precedent actors compute closer to SNARE (pure extraction, trapped exit); amendment coalitions compute closer to ROPE (genuine coordination, exit available through mobilization).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: 'political_contestation_space' (non-agent — the abstract domain of debate) and 'constitutional_amendment_coalition' (organized actors mobilized by interpretive disputes) benefit from a system that keeps constitutional meaning open. Amendment coalitions derive d ≈ 0.25 (beneficiary end): they mobilize in response to disputes, exit is mobile (they can exit the coalition after amendment or settle), and they collect something (influence over constitutional meaning). Victims: 'stable_precedent_dependent_actors' and 'minority_rights_holders_without_supermajority' bear costs. Precedent actors have constrained exit (they have built infrastructure around settled law) and derive d ≈ 0.75 (near target). Minority rights holders have identity-locked exit (leaving the polity is not a realistic option) and derive d ≈ 0.85 (near full target). The institutional seats (judiciary, legislature, executive) are DUAL: they both coordinate (resolve disputes, make policy, enforce law) and extract (constrain each other, impose costs). Judiciary derives d ≈ 0.50 (symmetric): it authoritatively interprets, but its authority is not final. Legislature derives d ≈ 0.50: it can override courts and control appointments, but it too is checked. Executive derives d ≈ 0.55 (slightly toward target): it has the fewest independent interpretive tools and depends on the other branches' authorization to act.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY QUESTION: Is this constraint's founding mandate (avoiding tyranny of a single interpretive authority) still live, or has it become a zombie? The coordinate reading says the mandate is LIVE: contemporary debates over judicial activism and executive overreach show ongoing concern about unilateral power. However, there is a case for PARTIAL MANDATROPHY: the constraint now primarily serves to KEEP CONSTITUTIONAL MEANING OPEN TO POLITICAL CONTESTATION rather than to prevent TYRANNY per se. If the true mandate was 'prevent one branch from claiming final authority,' that mandate has arguably succeeded — none of the three branches unilaterally claims final authority anymore. But the constraint has become INSTITUTIONALIZED as a system that keeps amendment possible, keeps appointment politics relevant, and prevents doctrinal closure. The persistence mechanism is no longer 'we need to prevent tyranny' but 'we benefit from keeping this contestation space open.' This is a transition from ROPE (coordination around a solved problem) to TANGLED ROPE (coordination carrying asymmetric extraction). The mandatrophy analysis points to omega variables that resolve this: What is the actual persistence mechanism? Is it still about tyranny prevention (live mandate) or has it become about keeping the contestation space open for certain political actors (extracted function)? The theater ratio measurement helps here: as theater rises (currently at 0.48, rising toward 0.50), the constraint shifts toward theatrical maintenance of a political benefit rather than real protection against tyranny.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tyranny_prevention_vs_contestation_opening,
    'Is the coordinate construction model''s persistence driven by the founding mandate (preventing tyranny of a single authority) or by a different mechanism (keeping constitutional meaning open to political contestation and amendment)?',
    'Historical analysis of when the constraint emerged and what actors have defended it over time. If the constraint persists even when tyranny risk is low (as arguably it is in contemporary stable democracies with separated powers), then the persistence mechanism is not tyranny prevention but contestation opening.',
    'If the constraint persists due to contestation opening rather than tyranny prevention, it transitions from ROPE (solving the founding problem) to TANGLED ROPE (carrying asymmetric extraction for beneficiaries of the contestation space). This would confirm the mandatrophy signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tyranny_prevention_vs_contestation_opening, empirical, 'Whether the coordinate construction model is still solving its founding problem or has become institutionalized for a different reason.').

omega_variable(
    judicial_supremacy_vs_coordinate_logics,
    'Can both judicial supremacy and coordinate construction coexist as live readings of the same constitutional text, or does the judicial-supremacy claim (courts have final authority) logically foreclose the coordinate claim (no seat has final authority)?',
    'Analytical framing test: can a single legal system consistently hold that courts issue final rulings AND that those rulings are subject to legislative override, amendment, and appointment-driven reversal? Or does one claim logically exclude the other?',
    'If the claims foreclose each other (court rulings cannot be both ''final'' and ''subject to override''), then the relation is FORECLOSES not COEXISTS_WITH. If they can coexist (courts are final within their domain but the domain itself is politically contestable), then COEXISTS_WITH is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_supremacy_vs_coordinate_logics, conceptual, 'Whether judicial supremacy and coordinate construction are logically incompatible or can be held simultaneously by different parties.').

omega_variable(
    minority_rights_protection_under_coordination,
    'Does the coordinate construction model adequately protect minority rights, or does distributing interpretive authority leave minorities vulnerable to majoritarian override?',
    'Comparative constitutional law: do polities with truly coordinate systems (no supreme interpreter) protect minority rights better or worse than polities with judicial supremacy? Do judicial-supremacy systems show better minority-rights outcomes because courts shield minorities from majoritarian politics?',
    'If minorities are systematically worse protected under coordinate models, the constraint carries high extraction cost for minority rights holders (d ≈ 0.90), and the snare classification for that seat strengthens. If outcomes are similar, the constraint''s extraction is defensible as the price of preventing tyranny.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_rights_protection_under_coordination, empirical, 'Whether coordinate construction adequately protects minority constitutional rights.').

omega_variable(
    amendment_mechanism_closure,
    'Can the amendment process itself become closed (high supermajority requirement means effective entrenchment despite formal amendment possibility)? If so, does coordinate construction lose its benefit when amendment is no longer a live remedy?',
    'Analysis of amendment frequency and supermajority coalition dynamics. If amendments become effectively impossible (as some scholars argue happened to the US Constitution in the 20th century), does the coordinate model collapse into de facto judicial supremacy?',
    'If amendment closes, the benefit of coordinate construction (keeping meaning open to political contestation and amendment) evaporates. The constraint would become pure extraction by the institutional seats that have adapted to the current interpretation (judiciary + legislature in current configuration) against those frozen out.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_mechanism_closure, empirical, 'Whether the amendment process remains a live remedy or has become effectively closed.').

omega_variable(
    this_reading_versus_siblings_kernel,
    'This constraint is one reading of the kernel CONSTITUTIONAL_INTERPRETIVE_AUTHORITY. The sibling readings (judicial supremacy and parliamentary supremacy) are expressed as separate constraints. Are the ε values (extractiveness measures) for those siblings consistent with this reading''s ε, or do they diverge significantly?',
    'Cross-reading comparison: when the three sibling readings are authored separately and compiled, examine whether their base_extractiveness and structure-derived χ values indicate stable equilibrium or contestation. If judicial-supremacy ε is much lower (courts as protectors), coordinate ε is moderate (contested balance), and parliamentary ε is high (legislatures extracting from minorities), the readings form a contestation triplet.',
    'If the three readings have significantly different ε values and extraction profiles, it confirms that they are genuinely distinct constraints (ε-invariance principle: different observables, different constraints). If they have identical ε values, the readings are observables of a single constraint — a false kernel decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(this_reading_versus_siblings_kernel, empirical, 'Whether the three sibling readings of the constitutional_interpretive_authority kernel are distinct constraints or observables of one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__coordinate_construction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cons_tr_t8, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 8, 0.4).
narrative_ontology:measurement(cons_tr_t16, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 16, 0.45).
narrative_ontology:measurement(cons_tr_t24, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 24, 0.5).
narrative_ontology:measurement(cons_tr_t32, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 32, 0.49).
narrative_ontology:measurement(cons_tr_t40, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cons_be_t8, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(cons_be_t16, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(cons_be_t24, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(cons_be_t32, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 32, 0.59).
narrative_ontology:measurement(cons_be_t40, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(cons_su_t8, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 8, 0.39).
narrative_ontology:measurement(cons_su_t16, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(cons_su_t24, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 24, 0.43).
narrative_ontology:measurement(cons_su_t32, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 32, 0.42).
narrative_ontology:measurement(cons_su_t40, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_interpretive_authority__coordinate_construction_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority__parliamentary_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, appointment_politics_as_constitutional_amendment).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel constitutional_interpretive_authority. The kernel concerns which institutional seat (if any) possesses final interpretive authority over the constitution. Three readings decompose the kernel: (1) COORDINATE_CONSTRUCTION_READING (this file): no single seat has final authority; meaning emerges from inter-branch dialogue and political contestation. (2) JUDICIAL_SUPREMACY_READING: courts have final authority via constitutional guardianship; legislative acts subject to judicial nullification. (3) PARLIAMENTARY_SUPREMACY_READING: elected legislature has final authority; no judicial authority to void parliamentary acts. The three readings have different ε values (extractiveness measures), different beneficiary/victim structures, and different persistence mechanisms. They are NOT observables of one constraint; they are distinct constraints with shared kernel. The network links show dependency: the coordinate reading influences both supremacy readings (if no single seat has final authority, both the judicial and parliamentary supremacy claims are aspirational rather than descriptive). The three readings should be authored separately with distinct constraint_ids, each carrying reading_relations and axioms in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
