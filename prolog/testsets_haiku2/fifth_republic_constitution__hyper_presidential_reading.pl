% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__hyper_presidential_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__hyper_presidential_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: fifth_republic_constitution__hyper_presidential_reading
 *   human_readable: Fifth Republic Constitution: Hyper-Presidential Reading
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   The Fifth Republic constitution (1958) contains text that supports
 *   multiple readings of executive-legislative relations. Under the
 *   hyper-presidential reading, the president is the primary repository of
 *   national sovereignty, elected directly by the people with a mandate to
 *   embody the unified national will. The legislature's authority is
 *   reinterpreted as subordinate: Article 49.3 (government confidence)
 *   becomes a tool for the president to override legislative obstruction,
 *   Article 16 emergency powers are invoked with minimal oversight, and
 *   legislative dissolution (Article 12) becomes a threat that disciplines
 *   inconvenient majorities. This reading elevates presidential prerogative
 *   and interprets constitutional safeguards as advisory. The founding
 *   problem (Fourth Republic gridlock) is presented as ongoing justification,
 *   though historical evidence suggests it was substantially solved by the
 *   Fifth Republic's structural reforms independent of hyper-presidential
 *   readings. This is ONE reading of the contested Fifth Republic kernel;
 *   sibling readings (parliamentary_constraint_reading,
 *   cohabitation_equilibrium_reading) interpret the same text as requiring
 *   legislative co-authorization.
 *
 * KEY AGENTS:
 *   - incumbent_president: Embodies and claims direct national sovereignty via electoral mandate; sets policy agenda with minimal legislative authorization; invokes Article 16 and dissolution threats to enforce compliance.
 *   - national_legislature: Elected body subordinated to presidential will under this reading; retains formal legislative power but effectively overridden via executive action, veto, or dissolution threat.
 *   - presidency_institution: Office itself accumulates power and discretion; successive presidents inherit the broad prerogatives this reading licenses.
 *   - constitutional_safeguards: Separation of powers and legislative authorization requirements reinterpreted as advisory rather than binding.
 *   - citizen_electoral_mandate: Citizens' ability to constrain executive power via mid-term legislative elections is extracted away; overridden by the president's five-year mandate.
 *   - constitutional_court: Observes and occasionally constrains operation but does not interpret the constitution as prioritizing parliamentary constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, 0.78).
domain_priors:suppression_score(fifth_republic_constitution__hyper_presidential_reading, 0.71).
domain_priors:theater_ratio(fifth_republic_constitution__hyper_presidential_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(fifth_republic_constitution__hyper_presidential_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__hyper_presidential_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__hyper_presidential_reading, "Fifth Republic Constitution: Hyper-Presidential Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__hyper_presidential_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__hyper_presidential_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__hyper_presidential_reading, '5c06709b-1eae-4650-87b1-346732b9fa0b').
narrative_ontology:cs_kernel_codification('5c06709b-1eae-4650-87b1-346732b9fa0b', fixed_text).
narrative_ontology:cs_authority_grounding('5c06709b-1eae-4650-87b1-346732b9fa0b', lineage).
narrative_ontology:cs_interpretation_layer_present('5c06709b-1eae-4650-87b1-346732b9fa0b').
narrative_ontology:cs_reading_relation('5c06709b-1eae-4650-87b1-346732b9fa0b', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c06709b-1eae-4650-87b1-346732b9fa0b', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('5c06709b-1eae-4650-87b1-346732b9fa0b', foundational, president_embodies_direct_national_sovereignty).
narrative_ontology:cs_axiom_status(president_embodies_direct_national_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('5c06709b-1eae-4650-87b1-346732b9fa0b', president_embodies_direct_national_sovereignty, deontological).
narrative_ontology:cs_axiom('5c06709b-1eae-4650-87b1-346732b9fa0b', foundational, legislative_authorization_subordinate_to_electoral_mandate).
narrative_ontology:cs_axiom_status(legislative_authorization_subordinate_to_electoral_mandate, holdable).
narrative_ontology:cs_axiom_grounding('5c06709b-1eae-4650-87b1-346732b9fa0b', legislative_authorization_subordinate_to_electoral_mandate, conventional).
narrative_ontology:cs_reference_frame('5c06709b-1eae-4650-87b1-346732b9fa0b', presidential_unitary_sovereignty).
narrative_ontology:cs_drift_state('5c06709b-1eae-4650-87b1-346732b9fa0b', post_cohabitation_reassertion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5c06709b-1eae-4650-87b1-346732b9fa0b', '2026-06-11T14:22:00Z').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, presidency_institution).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, national_legislature).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, constitutional_safeguards).
narrative_ontology:constraint_victim(fifth_republic_constitution__hyper_presidential_reading, citizen_electoral_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds executive power justified as direct embodiment of the national will via electoral mandate. Under this reading, invokes Article 16 emergency powers with minimal legislative oversight, dissolves National Assembly when necessary (Article 12), blocks legislation via executive veto or non-implementation. The incumbent president extracts broad personal authority from the constitution's text and uses institutional power to shield that extraction from legislative challenge.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, incumbent_president, agenda_setter,
    institutional, biographical, constrained, national).

% Elected with its own popular mandate but finds that mandate subordinated to the president's claimed direct sovereignty. Legislature can withhold confidence via Article 49.1, but the president can dissolve it (Article 12), forcing new elections where the president's veto power and media advantage usually restore a compliant majority. Over successive cycles, the legislature's constraint-setting power erodes as cohabitation becomes rarer and presidents gain confidence they can survive or escape legislative challenge.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, national_legislature, payer,
    powerful, generational, constrained, national).

% The office itself accumulates power and prestige under this reading. Successive presidents, once elected, inherit the broad executive authority this interpretation licenses. The institution grows in reach and discretion; the reading's persistence benefits the presidency regardless of the individual holder. Constitutional amendments that strengthen presidential prerogatives or successive court decisions upholding executive action reinforce the institutional benefit.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, presidency_institution, beneficiary,
    institutional, generational, arbitrage, national).

% The separation of powers, checks and balances, and legislative authorization requirements written into the Fifth Republic text are the entities this reading subordinates. They are not defeated in formal law but are reinterpreted as advisory rather than binding: Article 49.3 (government confidence) becomes a tool the president uses to override legislature, not a legislative power over the government.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, constitutional_safeguards, payer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(fifth_republic_constitution__hyper_presidential_reading, constitutional_safeguards).

% Citizens who vote for legislative majorities opposed to the president discover their mandate is overridden by the presidential mandate the same electorate gave five years prior. The interpretation that one electoral mandate (the president's) is structurally superior to another (the legislature's) places the citizen's ability to constrain executive power through mid-term legislative elections in the victim role. Exit: voting out the president requires waiting for the next presidential election (5 years) while the president dissolves an inconvenient legislature.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, citizen_electoral_mandate, payer,
    powerless, biographical, trapped, national).

% Political parties and coalitions that wish to govern via legislative policy-setting rather than presidential decree find themselves excluded from the decision-making structure once a hyper-presidential president is in office. They can win legislative elections but cannot implement their program if it contradicts the president's agenda. Their exclusion is what the constraint enforces.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, rival_executive_visions, excluded,
    powerful, generational, constrained, national).

% The Constitutional Council (or equivalent review body) observes and occasionally constrains the reading's operation. It can strike down executive orders as unconstitutional but does not interpret the constitution itself to have prioritized parliamentary constraint over presidential sovereignty — it merely enforces the bare constitutional text. Under the hyper-presidential reading, the court's role is narrow: constitutional compliance checking, not mandate allocation.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__hyper_presidential_reading, constitutional_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__hyper_presidential_reading, incumbent_president).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__hyper_presidential_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes executive decision-making and policy implementation into a single hierarchical structure justified as representing the unified national will via presidential election. Solves the coordination problem of multiple competing executive voices by subordinating all others (prime minister, cabinet, legislature) to the president's policy direction.
% TRANSFER_FUNCTION: Transfers legislative constraint-setting power from the National Assembly to the presidency. The legislature retains formal authority to pass laws and withhold confidence, but the president claims the right to override via executive action, constitutional reinterpretation, or dissolution. Transfers effective veto power from the legislative majority to the incumbent president.
% ABSENT_VOICES: Parliamentary majorities that win electoral legitimacy but lose policy-setting power to the president are structurally excluded from the conversation once the president adopts this reading. Alternative constitutional visions (parliamentary constraint, cohabitation equilibrium) are present in legal discourse but marginalized in the sitting president's invocation of the text. Judges, constitutional scholars, and opposing parties can articulate competing readings but lack the institutional power to impose them while the president controls the executive enforcement machinery.
% DISAPPEARANCE_RATIONALE: If this reading disappeared (replaced by a parliamentary or cohabitation reading), legislative majorities would regain effective veto power, cabinet selections would require legislative confidence, Article 16 emergency powers would be scrutinized and limited, and the separation of powers would function as a structural constraint rather than as advisory text. Policy-setting would require negotiation between president and legislature rather than presidential decree. The state's operational structure would reorganize around legislative authorization rather than presidential will.
% FOUNDING_PROBLEM: The Fourth Republic suffered from legislative gridlock, weak executives, and unstable coalitions that prevented coherent national policy. De Gaulle and the Fifth Republic's architects sought to create a strong executive capable of decisive action and embodying national unity above factional parliamentary debate.
% FOUNDING_PROBLEM_CORROBORATION: The founding generation and pro-presidential legal scholars attest the problem remains live and this reading is the legitimate solution. Parliamentary scholars, cohabitation advocates, and comparative constitutional analysts attest the founding problem is substantially solved and the reading now persists as an extracted benefit (executive power unconstrained by legislative authorization) rather than as a response to gridlock. The historical record from 1958-2000 shows legislative gridlock largely resolved even during cohabitation periods, suggesting the founding problem's reduction independent of hyper-presidential readings. See Duverger, Debré and constitutional court rulings acknowledging parliamentary co-authority.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__hyper_presidential_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__hyper_presidential_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__hyper_presidential_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fifth_republic_constitution__hyper_presidential_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__hyper_presidential_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__hyper_presidential_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__hyper_presidential_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the reading transfers effective legislative veto power to the presidency and subordinates democratic authorization mechanisms to presidential will. The extraction is not hidden extraction (the reading claims the president embodies legitimate national will), but it is extraction from the legislature and from the electoral power of legislative majorities. Suppression (0.71) is substantial because the reading's persistence depends on actively suppressing legislative assertion of co-equal authority: the threat of dissolution, the invocation of Article 16 with minimal legislative consultation, the reinterpretation of legislative budget authority as subject to presidential override. Theater ratio (0.42, moderate) reflects that genuine executive coordination and policy coherence are real functions the reading serves, but an increasing share of enforcement activity (especially in later decades as cohabitation becomes rarer) defends the presidential prerogative itself rather than serving the founding coordination problem. The measurement series shows extraction rising and plateauing as successive presidents establish precedent for hyper-presidential invocations, suppression hardening as institutional norms calcify, and theater ratio rising (the coordination function remains, but presidential power-maintenance becomes the salient enforcement driver). The coercion grid shows structural-level suppression rising (institutional pressure on legislative function) while individual-level resistance to the reading persists (citizens continue attempting to assert electoral power via mid-term legislative elections). Organizational-level resistance from the legislature itself declines (successive legislatures grow habituated to presidential dominance, especially when dissolution threats restore presidential majorities after inconvenient elections).
 *
 * PERSPECTIVAL GAP:
 *   From the presidential seat, the reading is coordination: unified executive decision-making embodying the national will prevents gridlock and incoherence. From the legislative seat, the reading is extraction: elected representatives find their authority subordinated and their constituents' electoral power overridden by the president's five-year term. From the citizen seat, the reading is partial extraction: the voter can constrain the legislature in mid-term elections but cannot check a president who claims direct sovereignty and can dissolve an inconvenient legislature. The engine computes these divergences from the structural data (power atoms, exit options, beneficiary/victim declarations); the authored claim does not adjudicate which seat's perception is correct. The measured divergence IS the classification signal: a constraint claimed as coordination but computed as extractive from most seats is a red flag for regulatory review or constitutional amendment.
 *
 * DIRECTIONALITY LOGIC:
 *   The incumbent president sits at d ≈ 0.1 (full beneficiary): the reading vests power in the office-holder and shields it from legislative override. The national legislature sits at d ≈ 0.85 (near-full target): its constraint-setting power is extracted away, its electoral mandate is subordinated, its veto is threatened with dissolution. The presidency as an institution sits at d ≈ 0.05 (full beneficiary): successive occupants inherit broader discretion than the Fourth Republic presidency held. Constitutional safeguards (non-agent) sit effectively at d ≈ 0.9 (victim of reinterpretation). The citizen electoral mandate sits at d ≈ 0.78 (victim: the citizen's power to constrain via mid-term legislative elections is overridden). These directionality values feed the engine's effective-extraction computation: for the beneficiary seats (president, presidency institution), effective extraction is damped or inverted into subsidy; for the victim seats (legislature, constitutional safeguards, citizen mandate), effective extraction is amplified. The constitutional court is positioned as observer (d ≈ 0.5) because it can constrain the reading at the margins but does not interpret the constitution itself to subordinate presidential to legislative authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Fourth Republic gridlock) was solved by the Fifth Republic's structural reforms (two-ballot electoral system, stronger executive, direct presidential election). The hyper-presidential reading presents ongoing prevention of gridlock as justification for maintaining broad presidential prerogative decades after the gridlock problem itself substantially resolved. The founding_problem_status is contested: the reading claims it remains live; competing scholarship and the cohabitation experience (1986-1988, 1993-1995, 1997-2002) show governance functioned with parliamentary constraint and legislatively imposed prime ministers, suggesting the founding problem is substantially dead while the reading persists. This is a mandatrophy case: the founding problem's death is attested by the cohabitation periods themselves, which produced stable government without hyper-presidential invocation; yet the reading persists due to institutional inertia (successive presidents inherit the precedent, constitutional scholars respect the lineage, the reading suits the presidential office's institutional interest). The constraint is not a pure piton (it retains real coordination function in policy coherence) but shows mandatrophy markers: rising theater ratio, asymmetric enforcement (suppression hardens while resistance to the reading persists in legislative and electoral venues), and the founding problem's status shifting from live to dead across the interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem (Fourth Republic legislative gridlock) still live, or has it been substantially solved and the hyper-presidential reading now persists as extracted power unconstrained by its original justification?',
    'Comparative historical analysis of governance quality and legislative deadlock during cohabitation periods (1986-1988, 1993-1995, 1997-2002) versus mono-executive periods; analysis of legislative productivity and policy coherence across both regimes.',
    'If the founding problem is dead (as cohabitation evidence suggests), the reading should be reclassified to piton or snare: power extraction without founding coordination function. If live (as the reading claims), it remains tangled rope: coordination justified extraction. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges is the diagnostic kernel for this omega.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the hyper-presidential reading''s stated founding problem justification remains valid or has become a mandatrophy cover story.').

omega_variable(
    democratic_sovereignty_allocation,
    'Can two separately-elected bodies (president and legislature) each claim direct embodiment of ''the national will'', or does one necessarily take priority when they conflict?',
    'Democratic theory and constitutional design comparison: do all functioning democracies with separately-elected executive and legislature resolve conflicts via one supreme body, or do some sustain genuine equipoise? Empirical test: does the cohabitation reading (dual authority, negotiated allocation) produce more or less democratic constraint than the hyper-presidential reading?',
    'If one body must take priority, the hyper-presidential reading''s claim to embody national will is justified; if equipoise is sustainable, the reading is a choice to subordinate one elected mandate to another elected mandate (more transparently extractive). This is the conceptual crux where readings coexist: the same text supports both, but different democratic theories (presidential-supremacy vs. legislative co-authority) generate different readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_sovereignty_allocation, conceptual, 'Whether the reading''s core claim (president as primary sovereign) is a constitutional necessity or a chosen allocation of democratic authority.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the legislature''s acceptance of subordinate status structural (external threat of dissolution) or internalized (legislators believe the presidential reading is constitutionally and normatively correct)?',
    'Post-cohabitation behavior: when cohabitation redistributed power to the legislature, did legislators'' resistance to presidential override strengthen (suggesting internalized suppression), or did they exercise co-authority comfortably (suggesting mainly structural suppression)? Qualitative analysis of parliamentary debates and reform proposals during and after cohabitation.',
    'If suppression is mainly structural, removing the dissolution threat would restore legislative constraint. If internalized, the reading''s persistence depends on cultural acceptance of presidential supremacy, and would require constitutional amendment plus civic re-education to reverse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether legislative suppression operates via external threats or via internalized acceptance of subordinate role.').

omega_variable(
    reading_kernel_contest,
    'This constraint is one reading of the fifth_republic_constitution kernel. Does the kernel''s text support hyper-presidential reading as the intended or natural reading, or is it one of multiple structurally defensible readings?',
    'Textual analysis of constitutional text and legislative history: what was the drafting intent regarding executive-legislative balance? Do constitutional scholars across ideological spectra agree the text has a single natural reading, or is the reading contest itself a property of the text''s ambiguity? Evidence: the cohabitation periods show the same text accommodated parliamentary-constraint readings without amendment.',
    'If the text is genuinely ambiguous (as cohabitation evidence suggests), the reading is a choice among multiple defensible interpretations, and the choice benefits the presidency. If the text naturally supports hyper-presidential reading, the constraint is a legitimate constitutional arrangement rather than a chosen extraction. This is THE kernel-reading ambiguity omega: it names what the committer-frame structure is contesting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Whether the Fifth Republic constitution kernel is inherently hyper-presidential or admits multiple readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__hyper_presidential_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t0, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(fift_tr_t0, observed).
narrative_ontology:measurement(fift_tr_t5, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(fift_tr_t5, observed).
narrative_ontology:measurement(fift_tr_t10, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(fift_tr_t10, observed).
narrative_ontology:measurement(fift_tr_t15, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(fift_tr_t15, observed).
narrative_ontology:measurement(fift_tr_t20, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(fift_tr_t20, observed).
narrative_ontology:measurement(fift_tr_t25, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(fift_tr_t25, observed).
narrative_ontology:measurement(fift_tr_t30, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(fift_tr_t30, observed).
narrative_ontology:measurement(fift_tr_t40, fifth_republic_constitution__hyper_presidential_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(fift_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(fift_be_t0, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(fift_be_t0, observed).
narrative_ontology:measurement(fift_be_t5, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement_basis(fift_be_t5, observed).
narrative_ontology:measurement(fift_be_t10, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 10, 0.69).
narrative_ontology:measurement_basis(fift_be_t10, observed).
narrative_ontology:measurement(fift_be_t15, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement_basis(fift_be_t15, observed).
narrative_ontology:measurement(fift_be_t20, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement_basis(fift_be_t20, observed).
narrative_ontology:measurement(fift_be_t25, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement_basis(fift_be_t25, observed).
narrative_ontology:measurement(fift_be_t30, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement_basis(fift_be_t30, observed).
narrative_ontology:measurement(fift_be_t40, fifth_republic_constitution__hyper_presidential_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(fift_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t0, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(fift_su_t0, observed).
narrative_ontology:measurement(fift_su_t5, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(fift_su_t5, observed).
narrative_ontology:measurement(fift_su_t10, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(fift_su_t10, observed).
narrative_ontology:measurement(fift_su_t15, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(fift_su_t15, observed).
narrative_ontology:measurement(fift_su_t20, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(fift_su_t20, observed).
narrative_ontology:measurement(fift_su_t25, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(fift_su_t25, observed).
narrative_ontology:measurement(fift_su_t30, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(fift_su_t30, observed).
narrative_ontology:measurement(fift_su_t40, fifth_republic_constitution__hyper_presidential_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(fift_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(fift_grid_01, fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse(class), 0, 0.58).
narrative_ontology:measurement(fift_grid_02, fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse(class), 40, 0.62).
narrative_ontology:measurement(fift_grid_03, fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse(individual), 0, 0.55).
narrative_ontology:measurement(fift_grid_04, fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse(individual), 40, 0.59).
narrative_ontology:measurement(fift_grid_05, fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse(organizational), 0, 0.65).
narrative_ontology:measurement(fift_grid_06, fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse(organizational), 40, 0.68).
narrative_ontology:measurement(fift_grid_07, fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse(structural), 0, 0.72).
narrative_ontology:measurement(fift_grid_08, fifth_republic_constitution__hyper_presidential_reading, accessibility_collapse(structural), 40, 0.75).
narrative_ontology:measurement(fift_grid_09, fifth_republic_constitution__hyper_presidential_reading, resistance(class), 0, 0.68).
narrative_ontology:measurement(fift_grid_10, fifth_republic_constitution__hyper_presidential_reading, resistance(class), 40, 0.65).
narrative_ontology:measurement(fift_grid_11, fifth_republic_constitution__hyper_presidential_reading, resistance(individual), 0, 0.62).
narrative_ontology:measurement(fift_grid_12, fifth_republic_constitution__hyper_presidential_reading, resistance(individual), 40, 0.59).
narrative_ontology:measurement(fift_grid_13, fifth_republic_constitution__hyper_presidential_reading, resistance(organizational), 0, 0.75).
narrative_ontology:measurement(fift_grid_14, fifth_republic_constitution__hyper_presidential_reading, resistance(organizational), 40, 0.72).
narrative_ontology:measurement(fift_grid_15, fifth_republic_constitution__hyper_presidential_reading, resistance(structural), 0, 0.71).
narrative_ontology:measurement(fift_grid_16, fifth_republic_constitution__hyper_presidential_reading, resistance(structural), 40, 0.68).
narrative_ontology:measurement(fift_grid_17, fifth_republic_constitution__hyper_presidential_reading, stakes_inflation(class), 0, 0.54).
narrative_ontology:measurement(fift_grid_18, fifth_republic_constitution__hyper_presidential_reading, stakes_inflation(class), 40, 0.58).
narrative_ontology:measurement(fift_grid_19, fifth_republic_constitution__hyper_presidential_reading, stakes_inflation(individual), 0, 0.48).
narrative_ontology:measurement(fift_grid_20, fifth_republic_constitution__hyper_presidential_reading, stakes_inflation(individual), 40, 0.52).
narrative_ontology:measurement(fift_grid_21, fifth_republic_constitution__hyper_presidential_reading, stakes_inflation(organizational), 0, 0.62).
narrative_ontology:measurement(fift_grid_22, fifth_republic_constitution__hyper_presidential_reading, stakes_inflation(organizational), 40, 0.66).
narrative_ontology:measurement(fift_grid_23, fifth_republic_constitution__hyper_presidential_reading, stakes_inflation(structural), 0, 0.68).
narrative_ontology:measurement(fift_grid_24, fifth_republic_constitution__hyper_presidential_reading, stakes_inflation(structural), 40, 0.71).
narrative_ontology:measurement(fift_grid_25, fifth_republic_constitution__hyper_presidential_reading, suppression(class), 0, 0.51).
narrative_ontology:measurement(fift_grid_26, fifth_republic_constitution__hyper_presidential_reading, suppression(class), 40, 0.55).
narrative_ontology:measurement(fift_grid_27, fifth_republic_constitution__hyper_presidential_reading, suppression(individual), 0, 0.45).
narrative_ontology:measurement(fift_grid_28, fifth_republic_constitution__hyper_presidential_reading, suppression(individual), 40, 0.48).
narrative_ontology:measurement(fift_grid_29, fifth_republic_constitution__hyper_presidential_reading, suppression(organizational), 0, 0.58).
narrative_ontology:measurement(fift_grid_30, fifth_republic_constitution__hyper_presidential_reading, suppression(organizational), 40, 0.62).
narrative_ontology:measurement(fift_grid_31, fifth_republic_constitution__hyper_presidential_reading, suppression(structural), 0, 0.64).
narrative_ontology:measurement(fift_grid_32, fifth_republic_constitution__hyper_presidential_reading, suppression(structural), 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__hyper_presidential_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fifth_republic_constitution__hyper_presidential_reading, 0.12).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__parliamentary_constraint_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the Fifth Republic constitutional kernel family. The kernel (the 1958 constitution, formalized fixed text grounded in Gaullist lineage authority) is read differently by three constraint stories: hyper_presidential_reading (this file, extractive, president as sovereign), parliamentary_constraint_reading (legislature as co-authority), and cohabitation_equilibrium_reading (negotiated dual executive). Each reading generates a different constraint structure with different beneficiaries, victims, and extractiveness profiles. The readings are not alternative measurements of the same constraint; they are different constraints instantiated by different readings of the same contested kernel. All three must be generated and linked via network.affects_constraints to model the kernel contest accurately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__hyper_presidential_reading, powerful, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
