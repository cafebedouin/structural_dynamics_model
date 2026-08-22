% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__constitutional_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: qualified_immunity_doctrine__constitutional_fidelity_reading
 *   human_readable: Qualified Immunity Doctrine — Constitutional Fidelity Reading (Judicially Fabricated, Unauthorized)
 *   domain: constitutional law/civil rights/law enforcement
 *
 * SUMMARY:
 *   Qualified immunity is the judge-made rule that shields government
 *   officers from liability under Section 1983 unless they violated a
 *   constitutional right that was 'clearly established' at the time. This
 *   story is ONE reading of that contested kernel — the constitutional
 *   fidelity reading — under which the doctrine's defect lies in its
 *   provenance: it binds litigants nationwide without constitutional command
 *   or statutory authorization, having been authored, expanded, and
 *   administered exclusively by the courts whose power it enlarges. On this
 *   reading the doctrine is illegitimate regardless of its policy effects,
 *   because a rule displacing a congressionally enacted remedy is legitimate
 *   only if some authorized source enacted it. The ε referent is fixed to the
 *   standing arrangement — the doctrine as it actually operates — and is
 *   assessed by this reading's own lights, yielding a high authored ε; the
 *   endorsed alternative (a legislatively authored regime) is NOT the
 *   referent. CONSTRAINT FAMILY NOTE (ε-invariance decomposition): the
 *   colloquial label 'qualified immunity' decomposes into three structurally
 *   distinct stories linked by network edges — this file (unauthorized
 *   fabrication; snare claimed), the protective_scaffold reading
 *   (necessity-justified protection; different beneficiary weighting, softer
 *   expected type), and the accountability_void reading (impunity-extraction
 *   machinery; overlapping victim set, distributive emphasis). Each sibling
 *   assesses the SAME referent with a different evaluative frame and carries
 *   its own ε, stakeholders, and claimed type; no averaging occurs here.
 *   CLAIM/METRIC INDEPENDENCE: the snare claim is authored from this
 *   reading's structural assessment; the metrics are authored as descriptive
 *   of the doctrine's documented operation; the engine computes per-seat
 *   types from the structural data and any divergence from the claim is
 *   measurement, not error. KEY AGENTS (by structural relationship): -
 *   federal_judiciary: Agenda-setter and principal captured beneficiary
 *   (institutional/arbitrage) — authors, hardens, and administers the rule;
 *   collects institutional power; could end it at will -
 *   rank_and_file_police_officers: Practical beneficiary with a hidden cost
 *   leg (organized/constrained) — collects protection from suit; operates
 *   under a framework no legislature wrote - municipal_governments: Fiscal
 *   beneficiary (powerful/mobile) — avoids judgment payouts; bears defense
 *   costs up to the dismissal line - persons_injured_by_officer_misconduct:
 *   Primary target (powerless/trapped) — statutory claims dismissed before
 *   merits; no compensating alternative for the constitutional right itself -
 *   civil_rights_litigators: Secondary target (moderate/constrained) — absorb
 *   uncompensated contingency work on claims that die at step one -
 *   united_states_congress: Excluded lawmaker (institutional/mobile) —
 *   retains undisputed authority to codify or abolish; never asked -
 *   state_legislatures: Excluded lawmakers with demonstrated workaround
 *   capacity (institutional/constrained) — several have legislated their own
 *   immunity frameworks - legal_historians: Analytical observer
 *   (analytical/analytical) — document the founding problem and the drift
 *   from the statute's design
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.83).
domain_priors:suppression_score(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.79).
domain_priors:theater_ratio(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, extractiveness, 0.83).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__constitutional_fidelity_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__constitutional_fidelity_reading, "Qualified Immunity Doctrine — Constitutional Fidelity Reading (Judicially Fabricated, Unauthorized)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__constitutional_fidelity_reading, "constitutional law/civil rights/law enforcement").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__constitutional_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__constitutional_fidelity_reading, '1331b97a-2fd3-4878-bef9-60286b1b5a11').
narrative_ontology:cs_kernel_codification('1331b97a-2fd3-4878-bef9-60286b1b5a11', formalized).
narrative_ontology:cs_authority_grounding('1331b97a-2fd3-4878-bef9-60286b1b5a11', extraction).
narrative_ontology:cs_interpretation_layer_present('1331b97a-2fd3-4878-bef9-60286b1b5a11').
narrative_ontology:cs_reading_relation('1331b97a-2fd3-4878-bef9-60286b1b5a11', qualified_immunity_doctrine__protective_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('1331b97a-2fd3-4878-bef9-60286b1b5a11', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_axiom('1331b97a-2fd3-4878-bef9-60286b1b5a11', foundational, authorization_preconditions_binding_remedial_rules).
narrative_ontology:cs_axiom_status(authorization_preconditions_binding_remedial_rules, holdable).
narrative_ontology:cs_axiom_grounding('1331b97a-2fd3-4878-bef9-60286b1b5a11', authorization_preconditions_binding_remedial_rules, conventional).
narrative_ontology:cs_axiom('1331b97a-2fd3-4878-bef9-60286b1b5a11', foundational, policy_outcomes_cannot_legitimize_unauthorized_rules).
narrative_ontology:cs_axiom_status(policy_outcomes_cannot_legitimize_unauthorized_rules, holdable).
narrative_ontology:cs_axiom_grounding('1331b97a-2fd3-4878-bef9-60286b1b5a11', policy_outcomes_cannot_legitimize_unauthorized_rules, deontological).
narrative_ontology:cs_axiom('1331b97a-2fd3-4878-bef9-60286b1b5a11', secondary, judicial_self_authorization_is_void).
narrative_ontology:cs_axiom_status(judicial_self_authorization_is_void, holdable).
narrative_ontology:cs_axiom_grounding('1331b97a-2fd3-4878-bef9-60286b1b5a11', judicial_self_authorization_is_void, conventional).
narrative_ontology:cs_reference_frame('1331b97a-2fd3-4878-bef9-60286b1b5a11', legislated_remedy_baseline).
narrative_ontology:cs_drift_state('1331b97a-2fd3-4878-bef9-60286b1b5a11', contemporary_two_step_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('1331b97a-2fd3-4878-bef9-60286b1b5a11', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, rank_and_file_police_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, municipal_governments).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, persons_injured_by_officer_misconduct).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_litigators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, rank_and_file_police_officers).
narrative_ontology:constraint_vindicates(qualified_immunity_doctrine__constitutional_fidelity_reading, unauthorized_common_law_rulemaking_over_enacted_remedy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Created the doctrine by judicial opinion beginning in the late 1960s and hardened it across successive decisions; applies it to nearly every Section 1983 case through a mandatory two-step inquiry; collects expanded authority over remedial law that the governing statute nowhere delegates; could end it by majority opinion as easily as it began it, and has declined to; faces no external authority empowered to compel its continuation.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary, beneficiary).

% Work under the doctrine's shield: most misconduct claims against individual officers are dismissed at the first stage, sparing them personal judgment liability and career-ending awards, and their unions lobby to preserve the arrangement. They did not design the rule and cannot maintain it themselves; and they operate inside a framework no legislature ever wrote, so when courts adjust the standard their protections move without notice or consent.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, rank_and_file_police_officers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, rank_and_file_police_officers, payer).

% Avoid judgment payouts, settlement costs, and insurance-premium spikes when claims against officers are dismissed; budget offices incorporate predictable immunity-driven dismissal rates; they bear defense-litigation spending up to the dismissal line and advocate through municipal associations for the arrangement's continuation.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, municipal_governments, beneficiary,
    powerful, biographical, mobile, local).

% Hold federal statutory claims that courts routinely dismiss before any evaluation of what happened to them; receive neither compensation nor official acknowledgment of wrongdoing; cannot opt out of the doctrine's application to their cases; state-law suits sometimes reach related conduct but rarely vindicate the constitutional right itself.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, persons_injured_by_officer_misconduct, payer,
    powerless, biographical, trapped, national).

% Investigate, plead, and advance claims that statistically die at the first stage of the two-step test; work predominantly on contingency and absorb uncompensated hours on cases that end without fee awards; leaving the practice area would abandon the client population whose only federal remedy path runs through this procedure.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_litigators, payer,
    moderate, biographical, constrained, national).

% Enacted Section 1983 and retains undisputed authority to codify, limit, or abolish official immunity by ordinary legislation; has held hearings and twice passed House reform bills that did not survive the Senate; its silence is invoked by the doctrine's defenders as acquiescence, though the doctrine was never submitted for its approval.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, united_states_congress, excluded,
    institutional, generational, mobile, national).

% Hold concurrent authority over officer-liability rules inside their borders; several, beginning with Colorado in 2020, enacted statutory immunity frameworks of their own, demonstrating the legislative path the federal judge-made rule bypassed; most remain outside the federal adjudication conversation where the operative rule lives and is revised.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, state_legislatures, excluded,
    institutional, generational, constrained, regional).

% Document the statute's legislative history and the doctrine's departure from it; attest the founding adjudication problem and its transformation from outside the arrangement's operating institutions; publish analyses the courts cite in argument but that bind nothing.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, legal_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__constitutional_fidelity_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__constitutional_fidelity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives federal courts a uniform, repeatable procedure for resolving damage suits against public officers and shields officers from personal financial exposure for good-faith errors made in fast-moving situations.
% TRANSFER_FUNCTION: Moves compensation and official acknowledgment away from persons whose constitutional rights were violated, and moves rulemaking authority over remedial law from Congress and the state legislatures to the federal judiciary; fiscal relief flows incidentally to officers and municipal budgets.
% ABSENT_VOICES: Congress and the state legislatures — the authorized lawmakers — never consented to the rule and hold no seat in the adjudication venue where it operates; injured persons appear only case-by-case as litigants with no collective voice; the 1871 statute's framers are absent entirely, their documented purposes cited by historians rather than represented by anyone.
% DISAPPEARANCE_RATIONALE: Thousands of pending Section 1983 cases would proceed to merits evaluation; municipal risk pricing, police-professional insurance, and officer training incentives would reprice within quarters; the judiciary's self-assumed authority over remedial law would revert to the enacted statute; and the statutory immunity frameworks several states have already legislated show the replacement architecture substantially exists.
% FOUNDING_PROBLEM: After Congress enacted Section 1983 without addressing official immunity, courts confronted damage suits against officers with no settled rule: full personal liability threatened to punish good-faith errors and deter public service, while wholesale immunity would hollow out the statute's accountability purpose.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: legal-historical scholarship (including Reinert & Meltzer, 'How Qualified Immunity Fails,' Yale Law Journal 2017) documents both the original adjudication difficulty and the doctrine's drift from the statute's design; House Judiciary Committee hearing records from 2019–2020 record former judges, prosecutors, and police executives attesting to the founding problem and its subsequent distortion; Colorado's 2020 legislative findings expressly declare the federal judge-made rule contrary to the legislature's understanding of Section 1983's intent.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__constitutional_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__constitutional_fidelity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__constitutional_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.83, 'stealth/ox-alpha', 'none', direct).

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
 *   Authored ε is 0.83 because, assessed by this reading, the doctrine forecloses the enacted federal remedy near-categorically: empirical studies of the two-step test report first-stage dismissal rates above ninety percent, and the formal preservation of liability for 'egregious' misconduct survives mainly in doctrine books, not dockets. Suppression is authored 0.79 as a RAW STRUCTURAL PROPERTY (unscaled — the engine alone scales extractiveness by directionality and scope): the two-step inquiry is mandatory on every court including reluctant ones, appellate panels correct trial judges who try to reach the merits early, and no participant can opt out. Theater_ratio 0.58 reflects the gap between the performed individualized inquiry (searches for 'clearly established' analogues, balancing language) and the ritualized near-categorical outcome; roughly half the visible activity defends the appearance of rigor rather than producing adjudication. Accessibility_collapse 0.65: once the rule is understood, plaintiff alternatives partially persist (state tort suits, administrative discipline, occasional DOJ pattern-or-practice actions) but none substitutes for the constitutional remedy itself, so the effective alternative space is badly degraded without being zero. Resistance 0.62: sustained bar-advocacy campaigns, two House-passed reform bills, statutory replacements in multiple states beginning in 2020, published dissents from sitting justices, and broad legal-academy critique — real and recurring, defeated so far at the federal level. Temporal design: one shared eight-point grid across all three tracked metrics, all points observed; the trajectories are ratchets, not cycles — periodic scandal-reform pressure (crisis, bill introduction, fade) oscillates in the resistance environment but the doctrine's own operation has moved monotonically toward harder screening, so no cyclical measurement pattern is asserted. Coalition note: individual victims are powerless, but organized impact-litigation coalitions exist; their failure mode is structural — each claim must still pass the same two-step filter individually, so coalition strength does not convert into claim survival.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience radically different arrangements under the same case law. From the federal_judiciary seat the doctrine is its own considered craft — a workable governance instrument it designed, refined, and applies daily; that seat should compute coordination-heavy and extraction-light. From the officer and municipal seats it is protection: predictable dismissal of most claims, budgetable risk. From the injured-person and litigator seats it is a locked door: a statutory right that exists on paper and dies in procedure. From the congressional and state-legislative seats it is a jurisdictional occupation: their remedial authority exercised by an institution that never sought it. The engine computes these per-seat divergences from power, exit, and role data; this story's authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The federal_judiciary is declared beneficiary and holds the agenda_setter role: it authors the rule, administers it, and collects the institutional-power rent, placing its d near the beneficiary pole. Rank_and_file_police_officers are declared beneficiaries (they collect dismissal protection) — a DIRECTIONALITY OVERRIDE lifts their d modestly (power_atom organized, d 0.26) because the automatic derivation would sit them near a pure-beneficiary pole while they also bear a cost invisible to role data: they operate inside a framework no legislature authorized, so their protection is perpetually contestable and shifts beneath them with each judicial adjustment; no other stakeholder shares the organized atom, so the override is unambiguous. Municipal_governments are net fiscal beneficiaries (avoided payouts exceed defense spend) and stay near the beneficiary pole. Persons_injured_by_officer_misconduct are declared victims with trapped exit: once immunity attaches there is no alternative federal forum, pushing d toward the full-target pole. Civil_rights_litigators are declared victims with constrained exit — they cannot leave the practice area without abandoning their clients' only remedy path. Congress and state legislatures hold excluded seats: they are outside the derivation by design (authored absence is commentary-grade, never correction-grade), recorded here as the parties whose consent was never sought.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — adjudicating damage suits against public officers without ruining good-faith servants or nullifying the statute's accountability purpose — is still live, so the mismatch consumer reads status(live) x verdict(world_rearranges) and raises no zombie flag. The drift signal travels instead through the theater_ratio series (0.18 to 0.58): the individualized-merits function the objective test promised has been progressively replaced by its proxy, first-stage dismissal. Classification discipline cuts both ways here. Reading the doctrine as a protective coordination device (the sibling scaffold story) would mislabel enforced impunity as public service; reading it as nothing but an accountability void (the sibling void story) misses that it persists atop a genuinely live adjudication problem, which is what gives its cover story traction. This reading's contribution is orthogonal to both: because legitimacy turns on authorization rather than function, even a hypothetically perfectly-functioning version of the doctrine would remain illegitimate — the snare claim rests on the provenance defect plus the documented extraction, with the protective rationale functioning as cover whose substantive work (shielding officers from personal ruin) is already performed by municipal indemnification, undercutting any residual genuine-coordination claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_assignment_qi,
    'This constraint instantiates the constitutional_fidelity reading of the qualified_immunity_doctrine kernel; what structural delta follows if a sibling reading governs the same referent?',
    'Compile and compare the sibling stories (qualified_immunity_doctrine__protective_scaffold_reading, qualified_immunity_doctrine__accountability_void_reading) against the identical standing arrangement; inspect where per-seat classifications and beneficiary sets diverge.',
    'Under the protective_scaffold reading the beneficiary set centers on officers and municipalities and the computed type may soften toward rope/scaffold; under the accountability_void reading the type stays snare-flavored with emphasis shifted to distributive impunity; under this reading provenance is decisive and the judiciary is the captured seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_assignment_qi, conceptual, 'Kernel membership and sibling-reading structural deltas for the qualified immunity family.').

omega_variable(
    disagreement_location_legitimacy_basis,
    'Where exactly do the three readings locate the disagreement: authorization lineage (this reading), operational necessity (protective_scaffold), or distributive impunity (accountability_void)?',
    'Structural comparison of the three stories'' axioms, reference frames, and drift states; the disagreement is located wherever the readings assign legitimacy to different sources (enacted law vs. enabling function vs. outcome distribution).',
    'Determines which axis drives classification: if authorization lineage governs, the doctrine''s entire operation is tainted regardless of function; if necessity governs, extraction is weighed against protection delivered; if distribution governs, only the impunity flow matters.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_legitimacy_basis, conceptual, 'Location of the inter-reading disagreement within the kernel contest.').

omega_variable(
    implicit_carriage_of_common_law_immunities,
    'Did the 1871 enactment of Section 1983 implicitly carry forward then-existing common-law official immunities, giving parts of the modern doctrine a statutory lineage?',
    'Archival legislative history of the Ku Klux Act debates and contemporaneous officer-liability practice, assessed by legal historians outside the benefiting parties; the historical record on what the 1871 Congress understood about immunities is the deciding evidence.',
    'If implicit carriage is established, the ''wholly fabricated'' charge narrows to the modern clearly-established standard added after 1967; if absent, the entire doctrine lacks statutory ancestry and this reading''s indictment extends to its root.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_carriage_of_common_law_immunities, empirical, 'Whether any portion of the doctrine has statutory lineage or is wholly judicially fabricated.').

omega_variable(
    epsilon_cross_reading_stability,
    'The kernel manifest flags epsilon_base as indeterminate because the framework itself is contested; does authored epsilon remain stable across readings over the fixed referent, or does the reading-indexed assessment diverge enough to flip computed types?',
    'Cross-file comparison of authored epsilon in all three family stories over the same referent; divergence beyond the engine''s tolerance band flips per-seat classifications.',
    'If epsilon diverges sharply across readings, corpus-level meta-analysis must treat the family as a single contested object with three assessments rather than three independent constraints; per-seat divergence becomes signal, not noise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_cross_reading_stability, conceptual, 'Reading-indexed epsilon divergence over a fixed referent.').

omega_variable(
    statutory_fix_political_feasibility,
    'Fixing_cost is authored ''cheap'' on technical grounds (one statute, or one majority opinion ending what opinions began); do political veto points render the effective cost prohibitive in practice?',
    'Observe the reform-bill trajectory (House-passed policing-reform measures dying in the Senate in consecutive sessions), state statutory adoptions as workaround evidence, and any Supreme Court signals about willingness to revisit the doctrine.',
    'If the fix is politically prohibitive despite being technically cheap, the persistence mechanism shifts from institutional will to veto-point structure, lengthening the entrenchment horizon and strengthening the case that maintenance is deliberate rather than inertial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_fix_political_feasibility, preference, 'Whether the technically cheap fix is reachable given the political preference landscape.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__constitutional_fidelity_reading, 0, 43).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(qual_tr_t0, observed).
narrative_ontology:measurement(qual_tr_t6, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement_basis(qual_tr_t6, observed).
narrative_ontology:measurement(qual_tr_t12, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement_basis(qual_tr_t12, observed).
narrative_ontology:measurement(qual_tr_t18, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 18, 0.33).
narrative_ontology:measurement_basis(qual_tr_t18, observed).
narrative_ontology:measurement(qual_tr_t24, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(qual_tr_t24, observed).
narrative_ontology:measurement(qual_tr_t31, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 31, 0.47).
narrative_ontology:measurement_basis(qual_tr_t31, observed).
narrative_ontology:measurement(qual_tr_t37, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 37, 0.53).
narrative_ontology:measurement_basis(qual_tr_t37, observed).
narrative_ontology:measurement(qual_tr_t43, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 43, 0.58).
narrative_ontology:measurement_basis(qual_tr_t43, observed).

% Extraction over time
narrative_ontology:measurement(qual_be_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(qual_be_t0, observed).
narrative_ontology:measurement(qual_be_t6, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 6, 0.51).
narrative_ontology:measurement_basis(qual_be_t6, observed).
narrative_ontology:measurement(qual_be_t12, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 12, 0.57).
narrative_ontology:measurement_basis(qual_be_t12, observed).
narrative_ontology:measurement(qual_be_t18, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 18, 0.63).
narrative_ontology:measurement_basis(qual_be_t18, observed).
narrative_ontology:measurement(qual_be_t24, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 24, 0.69).
narrative_ontology:measurement_basis(qual_be_t24, observed).
narrative_ontology:measurement(qual_be_t31, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 31, 0.75).
narrative_ontology:measurement_basis(qual_be_t31, observed).
narrative_ontology:measurement(qual_be_t37, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 37, 0.8).
narrative_ontology:measurement_basis(qual_be_t37, observed).
narrative_ontology:measurement(qual_be_t43, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 43, 0.83).
narrative_ontology:measurement_basis(qual_be_t43, observed).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(qual_su_t0, observed).
narrative_ontology:measurement(qual_su_t6, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement_basis(qual_su_t6, observed).
narrative_ontology:measurement(qual_su_t12, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement_basis(qual_su_t12, observed).
narrative_ontology:measurement(qual_su_t18, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 18, 0.66).
narrative_ontology:measurement_basis(qual_su_t18, observed).
narrative_ontology:measurement(qual_su_t24, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(qual_su_t24, observed).
narrative_ontology:measurement(qual_su_t31, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 31, 0.73).
narrative_ontology:measurement_basis(qual_su_t31, observed).
narrative_ontology:measurement(qual_su_t37, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 37, 0.76).
narrative_ontology:measurement_basis(qual_su_t37, observed).
narrative_ontology:measurement(qual_su_t43, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 43, 0.79).
narrative_ontology:measurement_basis(qual_su_t43, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__constitutional_fidelity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__protective_scaffold_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__accountability_void_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, section_1983_statutory_remedy).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the 'qualified immunity' label per the ε-invariance principle: the natural-language concept covers three structurally distinct claims that cannot share one story because they yield different ε, beneficiary structures, and types over the same referent. Upstream/downstream structure: the protective_scaffold reading is upstream (its necessity rationale is the publicly cited justification and supplies the cover story the other two readings indict); this constitutional_fidelity reading and the accountability_void reading are downstream critiques that consume that rationale. Every member links to the others via affects_constraints; each file documents the decomposition in its narrative_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__constitutional_fidelity_reading, organized, 0.26).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
