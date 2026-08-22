% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [SUPERSEDED_FEDERALLY_BY_SFFA_2023]
% ============================================================================

:- module(constraint_equal_protection_kernel__remedial_reading, []).

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
 *   constraint_id: equal_protection_kernel__remedial_reading
 *   human_readable: Equal Protection Clause — Remedial Reading (Race-Conscious Admissions Permission Under Narrow Tailoring)
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the equal-protection kernel: the
 *   remedial reading, under which the Fourteenth Amendment permits
 *   race-conscious state action when narrowly tailored to remedy documented
 *   historical exclusion or serve a compelling diversity interest. The
 *   constraint modeled is the standing arrangement that reading creates —
 *   race-conscious admissions at selective public institutions, operated
 *   under a judicially administered documentation-and-tailoring regime. Its
 *   epsilon referent is that standing arrangement, assessed by the reading's
 *   own lights: not the colorblind arrangement the sibling reading would
 *   install, and not the broader antisubordination arrangement. Historically
 *   excluded groups gain admission probability; applicants who would have
 *   been admitted under race-blind review bear the offsetting loss;
 *   universities administer the programs and collect their compositional and
 *   educational returns; the federal judiciary supplies the enforcement
 *   architecture. The sibling readings (colorblind_reading,
 *   antisubordination_reading) are separate constraint stories linked through
 *   network.affects_constraints; their structural deltas are recorded in the
 *   kernel omega. Operationally the reading ran 1978-2023 (Bakke through
 *   SFFA), and the measurement series traces its life: justification drift
 *   from documented remediation toward generic diversity, tightening
 *   enforcement, rising performative compliance, and terminal formal
 *   repudiation. KEY AGENTS (by structural relationship): -
 *   federal_judiciary: agenda-setting enforcer (institutional / analytical
 *   exit) — administers strict scrutiny, sets the documentation standard -
 *   selective_public_universities: administering beneficiary (institutional /
 *   constrained) — runs the programs, collects the returns, bears compliance
 *   costs - historically_excluded_minority_applicants: primary beneficiary
 *   (moderate / constrained) — receives admission probability the race-blind
 *   counterfactual would deny - unpreferred_rejected_applicants: primary
 *   target (moderate / constrained) — bears the lost-seat cost -
 *   civil_rights_advocacy_organizations: secondary beneficiary (organized /
 *   identity_locked) — collects docket, standing, and mission continuity -
 *   constitutional_law_scholars: analytical observer — supplies competing
 *   doctrinal accounts, collects nothing
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda-setting enforcer (institutional / analytical exit) — administers strict scrutiny, sets the documentation standard, strikes non-compliant programs
 *   - selective_public_universities: administering beneficiary (institutional / constrained) — designs and runs the programs, collects compositional and educational returns, bears compliance and litigation costs
 *   - historically_excluded_minority_applicants: primary beneficiary (moderate / constrained) — receives admission probability the race-blind counterfactual would deny
 *   - unpreferred_rejected_applicants: primary target (moderate / constrained) — bears the lost-seat cost of race-conscious review
 *   - civil_rights_advocacy_organizations: secondary beneficiary (organized / identity_locked) — collects docket, standing, and mission continuity from the programs' survival
 *   - constitutional_law_scholars: analytical observer (analytical / analytical) — supplies competing doctrinal accounts, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, 0.6).
domain_priors:suppression_score(equal_protection_kernel__remedial_reading, 0.7).
domain_priors:theater_ratio(equal_protection_kernel__remedial_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__remedial_reading, "Equal Protection Clause — Remedial Reading (Race-Conscious Admissions Permission Under Narrow Tailoring)").
narrative_ontology:topic_domain(equal_protection_kernel__remedial_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__remedial_reading, 'a4dbdcaa-2929-46ef-9a2b-73568851340e').
narrative_ontology:cs_kernel_codification('a4dbdcaa-2929-46ef-9a2b-73568851340e', fixed_text).
narrative_ontology:cs_authority_grounding('a4dbdcaa-2929-46ef-9a2b-73568851340e', lineage).
narrative_ontology:cs_interpretation_layer_present('a4dbdcaa-2929-46ef-9a2b-73568851340e').
narrative_ontology:cs_reading_relation('a4dbdcaa-2929-46ef-9a2b-73568851340e', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('a4dbdcaa-2929-46ef-9a2b-73568851340e', equal_protection_kernel__antisubordination_reading, coexists_with).
narrative_ontology:cs_axiom('a4dbdcaa-2929-46ef-9a2b-73568851340e', foundational, documented_exclusion_justifies_race_conscious_remedy).
narrative_ontology:cs_axiom_status(documented_exclusion_justifies_race_conscious_remedy, holdable).
narrative_ontology:cs_axiom_grounding('a4dbdcaa-2929-46ef-9a2b-73568851340e', documented_exclusion_justifies_race_conscious_remedy, empirically_contingent).
narrative_ontology:cs_axiom('a4dbdcaa-2929-46ef-9a2b-73568851340e', foundational, educational_diversity_is_compelling_state_interest).
narrative_ontology:cs_axiom_status(educational_diversity_is_compelling_state_interest, overridden).
narrative_ontology:cs_axiom_grounding('a4dbdcaa-2929-46ef-9a2b-73568851340e', educational_diversity_is_compelling_state_interest, instrumental).
narrative_ontology:cs_reference_frame('a4dbdcaa-2929-46ef-9a2b-73568851340e', equal_protection_permits_narrowly_tailored_remediation).
narrative_ontology:cs_drift_state('a4dbdcaa-2929-46ef-9a2b-73568851340e', sffa_repudiation_2023, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('a4dbdcaa-2929-46ef-9a2b-73568851340e', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__remedial_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, historically_excluded_minority_applicants).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, selective_public_universities).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, civil_rights_advocacy_organizations).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, unpreferred_rejected_applicants).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, narrow_tailoring_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, compelling_interest_documentation_requirement).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, educational_diversity_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reviews every race-conscious admissions program under strict scrutiny: demands a documented compelling-interest showing, audits whether the program is narrowly tailored, and periodically revisits whether the interest remains compelling. Sets the documentation standard universities must satisfy and strikes programs that fail it. Its position is fixed by office — it cannot decline the question, and it collects no admission slots and bears no enrollment consequences.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Design and operate the admissions programs: define the interest (remediation or diversity), build file-review processes in which race may tip close calls, and assemble the evidentiary record defending tailoring. They receive the compositional and educational outcomes the programs produce and bear the compliance and litigation costs of defending them. Leaving means unilaterally abandoning race-conscious review — available, but at a cost to stated mission commitments and political positioning.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, selective_public_universities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__remedial_reading, selective_public_universities, beneficiary).

% Applicants from groups historically barred or severely underrepresented at the institution. The program raises their admission probability at selective campuses; they receive seats and enrollment they would statistically not have obtained under race-blind review. Individually they hold little leverage over the process; their interests are carried by advocacy organizations and occasional intervenors in litigation.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, historically_excluded_minority_applicants, beneficiary,
    moderate, biographical, constrained, national).

% Applicants denied admission in cycles where race is considered who would have been admitted had review been race-blind. They bear the program's costs directly — a lost seat at a specific campus — and typically learn of the counterfactual only through litigation or statistical disclosure. Recourse is individual (other campuses, later cycles) or collective (organized litigation, which is how their position finally reached the enforcing court).
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, unpreferred_rejected_applicants, payer,
    moderate, biographical, constrained, national).

% Organizations founded to dismantle Jim Crow exclusion. The programs sustain their litigation dockets, amicus standing, membership relevance, and fundraising narratives; they defend the programs in court and mobilize politically for their retention. Their organizational identity is constituted through the anti-exclusion project, and pivoting to enforce race-neutral rules is close to unthinkable from inside.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, civil_rights_advocacy_organizations, beneficiary,
    organized, generational, identity_locked, national).

% Analyze the doctrine's coherence, publish competing accounts of what the clause requires, and supply the arguments both sides deploy in litigation. They collect nothing from the programs' operation and bear none of its costs.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__remedial_reading, historically_excluded_minority_applicants).
narrative_ontology:fixing_cost_class(equal_protection_kernel__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, judicially policed standard under which universities seeking to integrate institutions built under exclusion can act: a uniform compelling-interest-plus-documentation-plus-narrow-tailoring template that programs, applicants, and reviewing courts can all coordinate on, replacing ad hoc and politically exposed preference schemes.
% TRANSFER_FUNCTION: Moves admission probability at selective public institutions from applicants who would prevail under race-blind review toward applicants from historically excluded groups; secondarily moves interpretive authority over racial classification from universities and legislatures to the reviewing courts, via the documentation and tailoring obligations.
% ABSENT_VOICES: Displaced applicants had no seat in the doctrine's formation — the standard was negotiated among universities, civil-rights organizations, and reviewing courts; rejected applicants appeared only as individual litigants after the fact. Applicants to less selective campuses, who absorb cascading crowding effects as displaced applicants move down the selectivity ladder, were never represented at all.
% DISAPPEARANCE_RATIONALE: When the permission vanished (realized in 2023), admissions offices rewrote processes within one cycle, class composition shifted measurably at previously race-conscious campuses, advocacy litigation inverted to defend race-neutral proxies, and the documentation apparatus lost its object — the arrangements built around the permission demonstrably depended on it.
% FOUNDING_PROBLEM: Flagship public institutions — professional schools especially — operated under explicit racial exclusion (statutory segregation, outright refusal to admit Black applicants) into the mid-twentieth century; the problem was how such institutions, and the professions they fed, were to be integrated once formal exclusion ended.
% FOUNDING_PROBLEM_CORROBORATION: The founding exclusion is corroborated entirely outside the benefiting parties: judicial findings recording the refusal of state law schools to admit Black applicants, state segregation statutes, and professional-historical consensus. Persistence of the problem is disputed: social-science literatures tracing present wealth and schooling gaps to exclusionary policy support continued remedial need, while the 2023 Supreme Court majority and colorblind advocates deny that current applicants stand in the shoes of the excluded. Corroboration for persistence comes from demographers and historians outside the beneficiary set, not from the programs themselves.
narrative_ontology:disappearance_verdict(equal_protection_kernel__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_kernel__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__remedial_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_kernel__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.60: per-person harm is bounded by the plus-factor design, but aggregate displacement is material and rose steadily as the operative justification shifted from documented remediation (tied to identifiable injured communities) to generic diversity (open-ended, untied to documented injury) — broadening the beneficiary set diluted the remedial warrant while the cost stayed concentrated on displaced applicants. Suppression is authored at 0.70 as a RAW structural property — the engine scales only extractiveness by directionality and scope — reflecting an enforcement ratchet: deferential Bakke-era review hardened through Croson, Grutter's real strict scrutiny, Fisher's no-deference and workable-alternative demands, ending at maximal compliance burden. Theater ratio 0.44: a growing share of program activity became litigation-proofing (undefined critical mass, coached essays, post-hoc rationales) rather than remedial function, peaking when discovery in the 2023 litigation exposed the gap. Accessibility_collapse 0.52: race-neutral substitutes (percentage plans, socioeconomic preferences) are partially workable but imperfect, so alternatives persist rather than collapsing. Resistance 0.72: sustained constitutional litigation, statewide bans in multiple states, and recurring ballot fights. All three series run on ONE shared eight-point grid (1978-2023) so every metric is authored at every examined time point. The trajectory shows mild litigation-cycle oscillation (Hopwood-era turbulence, Grutter-era stabilization, Fisher-era plateau) driven by external doctrinal shocks, not by intermittent reinforcement — the underlying drift is monotonic.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural inputs. From the enforcing bench the arrangement is a manageable doctrinal template; from the administering university it is simultaneously mission fulfillment and a compliance burden; from the preferred applicant's seat it is opportunity and recognition; from the displaced applicant's seat it is an uncompensated, usually invisible loss. Same-level lateral divergence is sharp here: the two applicant classes hold the SAME power atom (moderate) and similar constrained exits, yet sit at opposite directional poles — differentiated purely by constraint-specific position (receipt versus bearing of the preference), not by global standing. The advocacy-organization seat adds institutional identity fusion: the organization has become its function, so its nominal mobility overstates its real exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: historically_excluded_minority_applicants and civil_rights_advocacy_organizations sit near the beneficiary pole (low d), with the advocacy seat's damping made durable by identity-locked exit. selective_public_universities are dual-positioned — collecting outcomes (beneficiary-side pressure) while bearing documentation and litigation costs (mild target-side pressure) — landing low-to-mid d, which the secondary_role declaration captures without an override. unpreferred_rejected_applicants sit near the full-target pole (high d), amplified by constrained exit: the lost seat cannot be recovered at the same campus. federal_judiciary carries no beneficiary or victim declaration and derives to roughly symmetric d — it administers without collecting. No directionality_overrides are used: the beneficiary/victim declarations plus exit atoms already separate every seat, and an override keyed on the shared institutional power atom would wrongly drag the universities toward the judiciary's neutral position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (integrating institutions built under explicit exclusion) is genuinely contested rather than dead: the historical exclusion is corroborated from outside the beneficiary set, but persistence of a present-day remedial need is disputed by the enforcing court's final majority. The mismatch consumer reads status=contested x verdict=world_rearranges — no dead-mandate flag fires, correctly, because the arrangement demonstrably organizes the world even while half its warrant eroded. The erosion itself is visible in the data: the reading survived its original warrant's weakening by substituting justifications (documented remediation to generic diversity), which is metric substitution in the Goodhart sense and is tracked by the rising theater_ratio series. The tangled_rope classification prevents symmetrical mislabeling: reading the arrangement as pure coordination erases the displaced-applicant class that pays for it; reading it as pure extraction erases the accountability architecture (documentation duties, tailoring review) that distinguished it from unbounded preference and that the enforcement series shows was real and intensifying.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the equal_protection_kernel; how would the sibling readings restructure the beneficiary and victim sets, and which reading controls?',
    'Doctrinal resolution by the enforcing court, not data: colorblind_reading empties the beneficiary set entirely (no permitted race-conscious action; every racially classified applicant becomes a potential victim of the classification) and antisubordination_reading broadens beneficiaries to all caste-subordinated groups while relaxing narrow tailoring. Cross-reading comparison of chi or classification is invalid by construction.',
    'Whichever reading controls flips the entire structural surface: beneficiary set, victim set, enforcement shape, and therefore every seat''s computed type. This story''s values are valid only within the remedial reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one kernel, three readings, mutually incompatible structural surfaces.').

omega_variable(
    remedial_to_diversity_justification_drift,
    'Did the operative justification of actual programs remain tied to documented historical exclusion, or did it drift to generic diversity untied to documented injury?',
    'Compare program records, legislative findings, and university justifications across the interval: early programs cited documented exclusion histories; later filings leaned on educational-diversity benefits with undefined critical mass.',
    'If the drift is real, late-period operation violates the reading''s own legitimacy condition — extraction loses its remedial warrant, supporting higher effective chi for the payer seat and snare-leaning per-seat classifications in the final third of the interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_to_diversity_justification_drift, empirical, 'Whether the reading''s own warrant was abandoned by its own operation.').

omega_variable(
    counterfactual_admit_identifiability,
    'Who exactly is a victim — can the race-blind counterfactual admit be identified for any individual rejected applicant under holistic review?',
    'Audit experiments and simulation on admissions data: rerun cycles with the race term ablated and measure the displaced set''s size and composition.',
    'Imprecise counterfactual identification spreads the cost across a wider, less identifiable class — lowering measured harm per identified victim while widening the affected pool; the payer seat''s chi depends on which framing the data supports.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_admit_identifiability, empirical, 'Precision of the victim set under holistic review.').

omega_variable(
    grutter_quasi_sunset_status,
    'Was the reading self-understood as transitional — Grutter''s stated expectation that racial preferences would not need strict scrutiny within 25 years — or as a steady-state doctrine?',
    'Doctrinal history: weigh Grutter''s sunset expectation against the absence of any operative sunset mechanism and the subsequent reaffirmation-and-tightening pattern through Fisher.',
    'If the reading was self-consciously transitional, its classification leans scaffold-like (justification is the transition); if steady-state, the tangled_rope reading holds. The absence of a formal sunset clause kept it out of the scaffold gate regardless.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grutter_quasi_sunset_status, conceptual, 'Whether the arrangement carried an undeclared transitional self-understanding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__remedial_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epk_remedial_tr_t1978, equal_protection_kernel__remedial_reading, theater_ratio, 1978, 0.18).
narrative_ontology:measurement_basis(epk_remedial_tr_t1978, observed).
narrative_ontology:measurement(epk_remedial_tr_t1985, equal_protection_kernel__remedial_reading, theater_ratio, 1985, 0.21).
narrative_ontology:measurement_basis(epk_remedial_tr_t1985, observed).
narrative_ontology:measurement(epk_remedial_tr_t1992, equal_protection_kernel__remedial_reading, theater_ratio, 1992, 0.25).
narrative_ontology:measurement_basis(epk_remedial_tr_t1992, observed).
narrative_ontology:measurement(epk_remedial_tr_t1999, equal_protection_kernel__remedial_reading, theater_ratio, 1999, 0.28).
narrative_ontology:measurement_basis(epk_remedial_tr_t1999, observed).
narrative_ontology:measurement(epk_remedial_tr_t2003, equal_protection_kernel__remedial_reading, theater_ratio, 2003, 0.33).
narrative_ontology:measurement_basis(epk_remedial_tr_t2003, observed).
narrative_ontology:measurement(epk_remedial_tr_t2008, equal_protection_kernel__remedial_reading, theater_ratio, 2008, 0.36).
narrative_ontology:measurement_basis(epk_remedial_tr_t2008, observed).
narrative_ontology:measurement(epk_remedial_tr_t2016, equal_protection_kernel__remedial_reading, theater_ratio, 2016, 0.39).
narrative_ontology:measurement_basis(epk_remedial_tr_t2016, observed).
narrative_ontology:measurement(epk_remedial_tr_t2023, equal_protection_kernel__remedial_reading, theater_ratio, 2023, 0.44).
narrative_ontology:measurement_basis(epk_remedial_tr_t2023, observed).

% Extraction over time
narrative_ontology:measurement(epk_remedial_be_t1978, equal_protection_kernel__remedial_reading, base_extractiveness, 1978, 0.38).
narrative_ontology:measurement_basis(epk_remedial_be_t1978, observed).
narrative_ontology:measurement(epk_remedial_be_t1985, equal_protection_kernel__remedial_reading, base_extractiveness, 1985, 0.41).
narrative_ontology:measurement_basis(epk_remedial_be_t1985, observed).
narrative_ontology:measurement(epk_remedial_be_t1992, equal_protection_kernel__remedial_reading, base_extractiveness, 1992, 0.44).
narrative_ontology:measurement_basis(epk_remedial_be_t1992, observed).
narrative_ontology:measurement(epk_remedial_be_t1999, equal_protection_kernel__remedial_reading, base_extractiveness, 1999, 0.47).
narrative_ontology:measurement_basis(epk_remedial_be_t1999, observed).
narrative_ontology:measurement(epk_remedial_be_t2003, equal_protection_kernel__remedial_reading, base_extractiveness, 2003, 0.51).
narrative_ontology:measurement_basis(epk_remedial_be_t2003, observed).
narrative_ontology:measurement(epk_remedial_be_t2008, equal_protection_kernel__remedial_reading, base_extractiveness, 2008, 0.54).
narrative_ontology:measurement_basis(epk_remedial_be_t2008, observed).
narrative_ontology:measurement(epk_remedial_be_t2016, equal_protection_kernel__remedial_reading, base_extractiveness, 2016, 0.57).
narrative_ontology:measurement_basis(epk_remedial_be_t2016, observed).
narrative_ontology:measurement(epk_remedial_be_t2023, equal_protection_kernel__remedial_reading, base_extractiveness, 2023, 0.6).
narrative_ontology:measurement_basis(epk_remedial_be_t2023, observed).

% Suppression requirement over time
narrative_ontology:measurement(epk_remedial_su_t1978, equal_protection_kernel__remedial_reading, suppression_requirement, 1978, 0.36).
narrative_ontology:measurement_basis(epk_remedial_su_t1978, observed).
narrative_ontology:measurement(epk_remedial_su_t1985, equal_protection_kernel__remedial_reading, suppression_requirement, 1985, 0.4).
narrative_ontology:measurement_basis(epk_remedial_su_t1985, observed).
narrative_ontology:measurement(epk_remedial_su_t1992, equal_protection_kernel__remedial_reading, suppression_requirement, 1992, 0.46).
narrative_ontology:measurement_basis(epk_remedial_su_t1992, observed).
narrative_ontology:measurement(epk_remedial_su_t1999, equal_protection_kernel__remedial_reading, suppression_requirement, 1999, 0.5).
narrative_ontology:measurement_basis(epk_remedial_su_t1999, observed).
narrative_ontology:measurement(epk_remedial_su_t2003, equal_protection_kernel__remedial_reading, suppression_requirement, 2003, 0.56).
narrative_ontology:measurement_basis(epk_remedial_su_t2003, observed).
narrative_ontology:measurement(epk_remedial_su_t2008, equal_protection_kernel__remedial_reading, suppression_requirement, 2008, 0.61).
narrative_ontology:measurement_basis(epk_remedial_su_t2008, observed).
narrative_ontology:measurement(epk_remedial_su_t2016, equal_protection_kernel__remedial_reading, suppression_requirement, 2016, 0.66).
narrative_ontology:measurement_basis(epk_remedial_su_t2016, observed).
narrative_ontology:measurement(epk_remedial_su_t2023, equal_protection_kernel__remedial_reading, suppression_requirement, 2023, 0.7).
narrative_ontology:measurement_basis(epk_remedial_su_t2023, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__remedial_reading, resource_allocation).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the equal-protection kernel decomposes into three readings with different epsilon values and different beneficiary/victim structures — colorblind_reading (no permitted race-conscious action; no beneficiary set; all racially classified applicants protected), remedial_reading (this file: permitted under documentation and tailoring; historically excluded groups benefit, displaced applicants pay), and antisubordination_reading (permission keyed to dismantling caste-like hierarchy; broadest beneficiary set, weakest tailoring demand). The upstream colorblind reading and the downstream remedial reading are linked because each is argued against the other's premises; the remedial and antisubordination readings are linked because remedial narrow-tailoring discipline is the main doctrinal pressure on antisubordination breadth. Each member links to the others via network.affects_constraints; no member averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
