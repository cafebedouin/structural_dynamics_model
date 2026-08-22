% ============================================================================
% CONSTRAINT STORY: tenure_contract__institutional_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__institutional_extraction_reading, []).

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
 *   constraint_id: tenure_contract__institutional_extraction_reading
 *   human_readable: Tenure as Permanent Rent Capture by Early Winners (Institutional Extraction Reading)
 *   domain: economic/institutional/labor
 *
 * SUMMARY:
 *   Under this reading, the tenure system converts an early-career victory —
 *   clearing the tenure gate — into a permanent, unconditional claim on
 *   institutional resources. Salary lines, course releases, and research
 *   support attach to the holder for life, insulated from enrollment shifts,
 *   administrative preference, and performance variation. Because the
 *   permanent tier cannot be resized, every budget shock lands on the
 *   uncommitted tier: institutions staff growth and volatility with
 *   per-course contingent instructors who carry the flexibility the permanent
 *   tier is shielded from, and finance the whole structure through tuition.
 *   The arrangement still performs a coordination function — it commits
 *   institutions to long-horizon work and staggers the labor market through a
 *   probationary tournament — but its incidence has inverted: security flows
 *   to those who no longer face annual evaluation, while risk concentrates on
 *   those who teach most of the courses. Enforcement is active and layered:
 *   peer-review gatekeeping controls entry to the permanent tier, contractual
 *   and statutory provisions protect the acquired claims, and hiring
 *   practices maintain the pipeline that replenishes it. KEY AGENTS (by
 *   structural relationship): - tenured_faculty: Primary beneficiary
 *   (organized/identity_locked) — holds the permanent claim and administers
 *   peer review - contingent_faculty: Primary target (powerless/trapped) —
 *   bears schedule flexibility and piece-rate instructional labor -
 *   students_and_families: Secondary target (moderate/constrained) — finances
 *   both tiers through tuition - university_administrations: Agenda-setting
 *   manager (institutional/mobile) — converts lines, expands the contingent
 *   tier, captures the flexibility - qualified_nontraditional_applicants:
 *   Excluded entrant (moderate/arbitrage) — locked out of lines that cannot
 *   be reallocated - higher_ed_labor_economists: Analytical observer
 *   (analytical/analytical) — sees the full two-tier structure across
 *   institutions
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, 0.76).
domain_priors:suppression_score(tenure_contract__institutional_extraction_reading, 0.66).
domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__institutional_extraction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__institutional_extraction_reading, "Tenure as Permanent Rent Capture by Early Winners (Institutional Extraction Reading)").
narrative_ontology:topic_domain(tenure_contract__institutional_extraction_reading, "economic/institutional/labor").

domain_priors:requires_active_enforcement(tenure_contract__institutional_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__institutional_extraction_reading, '85677896-9e98-409d-965f-4bed5e728f68').
narrative_ontology:cs_kernel_codification('85677896-9e98-409d-965f-4bed5e728f68', formalized).
narrative_ontology:cs_authority_grounding('85677896-9e98-409d-965f-4bed5e728f68', practice).
narrative_ontology:cs_interpretation_layer_present('85677896-9e98-409d-965f-4bed5e728f68').
narrative_ontology:cs_reading_relation('85677896-9e98-409d-965f-4bed5e728f68', tenure_contract__academic_freedom_reading, influences).
narrative_ontology:cs_reading_relation('85677896-9e98-409d-965f-4bed5e728f68', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('85677896-9e98-409d-965f-4bed5e728f68', foundational, permanent_claims_require_ongoing_contribution).
narrative_ontology:cs_axiom_status(permanent_claims_require_ongoing_contribution, holdable).
narrative_ontology:cs_axiom_grounding('85677896-9e98-409d-965f-4bed5e728f68', permanent_claims_require_ongoing_contribution, instrumental).
narrative_ontology:cs_axiom('85677896-9e98-409d-965f-4bed5e728f68', foundational, flexibility_costs_must_be_internalized).
narrative_ontology:cs_axiom_status(flexibility_costs_must_be_internalized, holdable).
narrative_ontology:cs_axiom_grounding('85677896-9e98-409d-965f-4bed5e728f68', flexibility_costs_must_be_internalized, deontological).
narrative_ontology:cs_reference_frame('85677896-9e98-409d-965f-4bed5e728f68', symmetric_deferred_compensation_bargain).
narrative_ontology:cs_drift_state('85677896-9e98-409d-965f-4bed5e728f68', contemporary_adjunctification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('85677896-9e98-409d-965f-4bed5e728f68', '').
narrative_ontology:cs_kernel_id(tenure_contract__institutional_extraction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, contingent_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, students_and_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, university_administrations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent appointments reached after a probationary period and peer review. Salary lines, course releases, and research support attach to them for life, insulated from enrollment swings and administrative preference. They staff the promotion-and-tenure committees that control entry to the permanent tier and dominate the senates that negotiate workload policy. Leaving for industry or another institution means surrendering the accumulated claim — pension vesting, sabbatical credit, lab space — and a professional self built around the appointment.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, tenured_faculty, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, tenured_faculty, agenda_setter).

% Teach on per-course, annual, or short-term contracts with no presumption of renewal, often at piece rates without benefits, frequently at several institutions at once. They absorb the schedule volatility the permanent tier is shielded from: enrollment-driven sections, last-minute staffing, large introductory courses. Years of specialized training and teaching-heavy loads thin their publication records, time out of the tenure pipeline is heavily penalized, and the academic job market is geographically concentrated — so remaining in the pipeline, however precarious, usually beats leaving.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, contingent_faculty, payer,
    powerless, immediate, trapped, national).

% Pay tuition that funds both the permanent salary commitments and the administrative apparatus managing the two-tier workforce, while a growing share of their instruction is delivered by the lowest-paid tier. They can choose among institutions, but nearly all sit inside the same funding structure, so price and enrollment withdrawal are their main levers.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, students_and_families, payer,
    moderate, immediate, constrained, national).

% Set budgets and decide how many positions carry permanent commitment versus term contracts. Permanent commitments constrain their flexibility, so they expand the contingent tier to absorb growth and volatility while publicly defending the permanence of the tenured core. Administrative careers advance by moving between institutions on short cycles, shorter than the commitments they administer.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, university_administrations, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, university_administrations, beneficiary).

% Researchers and teachers outside the guild network — industry scientists, independent scholars, international candidates — who could fill instructional and research roles if lines were reallocable. They are absent from the rooms where search criteria, fit judgments, and pipeline preferences are set, and are filtered out by practices oriented to internal candidates and pedigree.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, qualified_nontraditional_applicants, excluded,
    moderate, biographical, arbitrage, global).

% Study faculty employment structures across institutions: tenure density, contingent shares, wage effects, retirement patterns. They hold no stake in any particular arrangement and can see the full two-tier structure that participants inside any single institution cannot.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, higher_ed_labor_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__institutional_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Commits institutions irrevocably to a subset of their workforce so that long-horizon scholarly work can proceed without annual repricing, and staggers the labor market through a probationary tournament that feeds the permanent tier.
% TRANSFER_FUNCTION: Moves durable claims on salary lines, course releases, and research resources to those who cleared the tenure gate early in their careers; moves scheduling flexibility, enrollment risk, and piece-rate instructional labor onto contingent faculty; moves the financing of both tiers to students through tuition.
% ABSENT_VOICES: Contingent faculty deliver most instruction but are rarely seated in the senates and committees where workload and hiring policy are set; students have no vote on how tuition maps to staffing; qualified outsiders never see the search processes that filter them. Adjunct organizing drives exist precisely because the governed tier is largely absent from governance.
% DISAPPEARANCE_RATIONALE: Overnight abolition would reprice the entire academic labor market: permanent salary commitments would convert to term contracts, senior researchers would face annual retention competition, mid-career mobility would rise, and the contingent tier would either professionalize or dissolve depending on how institutions re-staffed instruction. Pension obligations, departmental hierarchies, and the doctoral pipeline would all reorganize around whatever risk allocation replaced it.
% FOUNDING_PROBLEM: Early twentieth-century trustees and donors dismissed professors for political and religious reasons; tenure was built to make inquiry independent of institutional displeasure, and to let institutions retain talent through deferred security rather than competitive cash salaries.
% FOUNDING_PROBLEM_CORROBORATION: The historical problem is corroborated outside the beneficiary set: AAUP investigative reports and contemporaneous press documented the political dismissals that produced the 1940 Statement. Today, faculty senates and civil-liberties records attest the protection problem still arises, while contingent-faculty unions, state auditor reports on instructional spending, and published labor-economic analyses attest that the operative function has drifted toward preserving incumbent claims. No source outside the benefiting parties attests that rent preservation was ever the founding purpose.
narrative_ontology:disappearance_verdict(tenure_contract__institutional_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__institutional_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__institutional_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tenure_contract__institutional_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__institutional_extraction_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__institutional_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenure_contract__institutional_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.76 at interval end) because the permanent claim is decoupled from ongoing contribution and because the costs of maintaining it are externalized to the contingent tier and to students. Suppression (0.66) is authored as a raw structural property, unscaled by power or scope: it consists of credential lock-in, pipeline penalties for time away, geographic concentration of academic employment, and hiring practices that filter entrants — not of coercive force scaled to any agent's size. Theater ratio (0.48) reflects the growing share of activity that is performative rather than functional: dossier assembly and letter-writing rituals, shared-governance meetings whose outcomes are predetermined, assessment bureaucracies that measure compliance rather than learning. Accessibility collapse (0.60) is moderate-high: alternatives are visible (international systems with weaker permanent claims, industry research careers) but collapse almost entirely for pipeline incumbents, whose specialization and pipeline penalties close the inside option while wage and prestige losses raise the cost of the outside one. Resistance (0.55) is real but fragmented: adjunct unionization drives, state legislative challenges to tenure, student debt politics, and disciplinary reform proposals pull from different directions and rarely coordinate. The three temporal series share one grid (1975–2025 at decade points) so every metric is authored at every examined time point; trajectories are monotonic rather than cyclical — budget-cycle oscillations exist but the secular drift toward a two-tier structure dominates them, and the base_properties values reflect the interval-end state. The claim (tangled_rope) and the metrics were authored independently: the reading asserts both a live coordination function and asymmetric extraction, and the metrics describe the operation as the reading sees it, without tuning either to a predicted engine verdict.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats compute differently. From the tenured seat, the arrangement is a earned entitlement administered through legitimate peer process — the coordination function is vivid, the extraction invisible, because the costs land on people the seat rarely encounters. From the contingent seat, the same structure is a closed guild: the coordination story justifies the gate they are stuck outside of. The administration seat computes a third view: permanence experienced as rigidity to be managed around, which is exactly why the contingent tier expands — the administrator's workaround is the cost-shift this reading measures. Students experience the structure only as price and instructional quality, with no visibility into the commitment structure producing both.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty are declared beneficiaries: the constraint subsidizes them (permanent claims, protected time), so their derived directionality sits near the beneficiary end and effective extraction damps toward zero or below — they collect from the arrangement. Contingent faculty and students are declared victims: the constraint extracts from them (flexibility costs, piece rates, tuition financing of the permanent tier), and their poor exit options (trapped, constrained) push them toward the full-target end, amplifying effective extraction. University administrations are dual-positioned: as agenda-setters they run the budget machinery, as secondary beneficiaries they capture the flexibility the two-tier structure yields them; their derivation sits between, closer to the beneficiary end than their managerial burdens alone would suggest. Qualified nontraditional applicants are excluded rather than coordinated — their harm is denial of access, but their arbitrage-grade outside options keep them from the trapped end. Higher-ed labor economists occupy the analytical seat with no directional exposure.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a hybrid coordination/extraction structure prevents two symmetrical misreadings. Reading it as pure coordination (the insider defense) hides who pays: it treats the contingent tier and tuition load as natural background rather than as the incidence of the permanent claim. Reading it as pure extraction (the abolitionist polemic) erases the real coordination function and predicts the wrong failure modes — if the protected-inquiry function is genuine, dismantling the arrangement reprices scholarly risk rather than liberating resources. On mandatrophy proper: the founding problem (political retaliation against scholars) is contested rather than dead, so no mandatrophy_resolved flag is declared — the arrangement has not outlived its mandate so much as inverted its incidence, keeping the original justification while shifting who bears the costs. The R5 mismatch consumer will find status=contested paired with verdict=world_rearranges, which correctly declines to fire the zombie flag: the world would rearrange because the coordination function is real, not because the founding problem is solved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the institutional_extraction_reading of kernel tenure_contract — how much of the measured extraction survives when the sibling readings'' epsilon referents are applied to the same arrangement?',
    'Cross-story comparison of the three linked constraint files: academic_freedom_reading indexes epsilon to the protection function (lower measured extraction, costs read as coordination price); demographic_reproduction_reading shifts the victim set to demographically filtered scholars (extraction concentrated at the gatekeeping stage rather than spread across the employment structure).',
    'The classification computed here is indexical to this reading. Under the freedom reading the same arrangement may certify as closer to pure coordination; under the demographic reading the enforcement surface, not the employment structure, carries the extraction. Corpus-level conclusions about tenure require all three files, not this one alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate structurally different constraints.').

omega_variable(
    coordination_price_vs_incumbent_rent,
    'How much of the measured extraction is the irreducible price of securing long-horizon inquiry, and how much is pure incumbent rent that would survive any protection scheme?',
    'Compare inquiry output and risk profiles across systems with weaker permanent claims (fixed-term European contracts, post-tenure-review regimes, jurisdictions that converted or abolished tenure): if risky long-horizon work declines when permanence weakens, part of the measured extraction is the coordination price; if it holds, the extraction is separable rent.',
    'If most extraction survives controls, the classification hardens toward the pure-extraction end and remedies shift from reform to replacement; if the protected-inquiry premium accounts for a large share, effective extraction falls toward coordination cost and the hybrid reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_price_vs_incumbent_rent, empirical, 'Separability of the coordination function from the extraction riding on it.').

omega_variable(
    adjunctification_causality,
    'Is contingent-labor growth caused by tenure rigidity (budget inflexibility forcing substitution), or by state disinvestment and administrative expansion using tenure as scapegoat?',
    'Difference-in-differences across states and systems varying in appropriations trajectories versus tenure-policy changes: if contingent shares track funding cuts more tightly than tenure rules, the cost-loading attributed to the constraint belongs substantially to the funding environment.',
    'Determines how much of the measured cost-shifting this constraint itself owns. If disinvestment is the primary driver, the constraint''s effective extraction drops and the two-tier structure reads as an adaptation imposed on it rather than a product of it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjunctification_causality, empirical, 'Causal attribution of the contingent-tier expansion that carries the reading''s cost-shift claim.').

omega_variable(
    cohort_concentration_of_claims,
    'Does the extraction concentrate in legacy cohorts (hires before the 1990s, holding defined-benefit pensions, lighter service loads, and cheaper entry housing), or is it intrinsic to permanent claims regardless of cohort?',
    'Within-tenured comparison of benefit packages, service loads, and real compensation by hire cohort; separation of the permanent-claim effect from the vintage-effect of entering under richer terms.',
    'If concentrated in legacy cohorts, the arrangement is a transition ratchet — reform targets grandfathering rules and the deal offered to new cohorts — and the permanent-claim institution itself may be salvageable as coordination; if uniform across cohorts, the extraction is structural to permanence as such.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohort_concentration_of_claims, empirical, 'Whether ''early winners'' is a cohort fact or a structural fact.').

omega_variable(
    suppression_mechanism_composition,
    'Is the suppression holding contingent faculty in the pipeline structural (economic dependency, credential lock-in, geographic concentration) or internalized (vocational identity, sunk-cost hope, status attachment to academic belonging)?',
    'Post-exit trajectory studies of faculty who leave the pipeline: if precarity perceptions and academic-identity attachment persist after stable outside employment is achieved, a substantial internalized component is present; if adjustment is rapid, suppression is predominantly structural.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure — leavers carry the lock with them, and pipeline attrition understates the constraint''s hold. If structural, removing the economic barriers would release the tier without cultural intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Structural versus internalized composition of the suppression binding the contingent tier.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__institutional_extraction_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t1975, tenure_contract__institutional_extraction_reading, theater_ratio, 1975, 0.22).
narrative_ontology:measurement(tenu_tr_t1985, tenure_contract__institutional_extraction_reading, theater_ratio, 1985, 0.27).
narrative_ontology:measurement(tenu_tr_t1995, tenure_contract__institutional_extraction_reading, theater_ratio, 1995, 0.33).
narrative_ontology:measurement(tenu_tr_t2005, tenure_contract__institutional_extraction_reading, theater_ratio, 2005, 0.39).
narrative_ontology:measurement(tenu_tr_t2015, tenure_contract__institutional_extraction_reading, theater_ratio, 2015, 0.44).
narrative_ontology:measurement(tenu_tr_t2025, tenure_contract__institutional_extraction_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(tenu_be_t1975, tenure_contract__institutional_extraction_reading, base_extractiveness, 1975, 0.46).
narrative_ontology:measurement(tenu_be_t1985, tenure_contract__institutional_extraction_reading, base_extractiveness, 1985, 0.53).
narrative_ontology:measurement(tenu_be_t1995, tenure_contract__institutional_extraction_reading, base_extractiveness, 1995, 0.61).
narrative_ontology:measurement(tenu_be_t2005, tenure_contract__institutional_extraction_reading, base_extractiveness, 2005, 0.67).
narrative_ontology:measurement(tenu_be_t2015, tenure_contract__institutional_extraction_reading, base_extractiveness, 2015, 0.72).
narrative_ontology:measurement(tenu_be_t2025, tenure_contract__institutional_extraction_reading, base_extractiveness, 2025, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t1975, tenure_contract__institutional_extraction_reading, suppression_requirement, 1975, 0.38).
narrative_ontology:measurement(tenu_su_t1985, tenure_contract__institutional_extraction_reading, suppression_requirement, 1985, 0.45).
narrative_ontology:measurement(tenu_su_t1995, tenure_contract__institutional_extraction_reading, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement(tenu_su_t2005, tenure_contract__institutional_extraction_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(tenu_su_t2015, tenure_contract__institutional_extraction_reading, suppression_requirement, 2015, 0.63).
narrative_ontology:measurement(tenu_su_t2025, tenure_contract__institutional_extraction_reading, suppression_requirement, 2025, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__institutional_extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% 'Tenure' is a colloquial label covering structurally distinct claims; per the epsilon-invariance principle it decomposes into three constraint stories sharing the tenure_contract kernel. This file authors the extraction structure (who captures permanent claims, who bears flexibility costs, who finances the arrangement). academic_freedom_reading authors the protection function with epsilon indexed to inquiry-protection value; demographic_reproduction_reading authors the gatekeeping structure with its victim set at the review stage. The upstream freedom claim is typically cited as legitimation for the arrangement this reading measures as extractive, so edges run from this story to both siblings. Relation choices: the extraction critique exerts structural pressure on the freedom reading — cost and rigidity evidence reshapes the political environment in which the freedom defense operates — without foreclosing it, since parties commonly hold both (relation: influences). The demographic reading coexists as a parallel live critique with a different mechanism and victim set; neither reading's core premise contradicts the other's (relation: coexists_with). Foundational axioms of this reading: permanent_claims_require_ongoing_contribution (instrumental grounding — decoupled claims fail the knowledge-production ends the arrangement serves) and flexibility_costs_must_be_internalized (deontological grounding — the party enjoying security owes the costs of providing it).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
