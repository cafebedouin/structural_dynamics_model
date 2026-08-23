% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__hindu_codified_reading, []).

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
 *   constraint_id: marriage_authority_kernel__hindu_codified_reading
 *   human_readable: Codified Hindu Marriage Authority (Hindu Marriage Act 1955, Civil-Court Reading)
 *   domain: legal/religious governance
 *
 * SUMMARY:
 *   The Hindu Marriage Act 1955 is the codified statute through which
 *   marriage, divorce, maintenance, and custody are legally constituted for
 *   everyone the Act defines as Hindu — a definition that quietly sweeps in
 *   Sikhs, Buddhists, and Jains. Adjudication sits with civil courts, which
 *   interpret the text through precedent; religious authorities solemnize but
 *   no longer decide. This story is ONE reading of the marriage-authority
 *   kernel (see kernel_context); the sibling readings are separate
 *   constraints, not folded in here. The claim and the metrics are authored
 *   independently: claimed_type tangled_rope reflects the judgment that
 *   genuine community-wide coordination and asymmetric, actively enforced
 *   extraction coexist in one structure; the metrics describe the
 *   arrangement's actual operation without being tuned to that label.
 *
 * KEY AGENTS:
 *   - - indian_parliament: Agenda-setter (institutional/constrained) — enacted and alone may amend the codified framework
 *   - - state_civil_courts: Agenda-setter and primary beneficiary (institutional/constrained) — holds adjudicative monopoly, absorbs interpretive drift
 *   - - hindu_community_households: Primary beneficiary (organized/identity_locked) — receives uniform rules; voluntary identity lock
 *   - - hindu_modernist_reform_establishment: Secondary beneficiary (institutional/arbitrage) — collects vindication of the codification program
 *   - - hindu_women_in_marital_disputes: Primary target (powerless/trapped) — bears the provisions' gendered residual costs
 *   - - sikh_buddhist_jain_adherents: Target (organized/identity_locked) — statutorily subsumed without consent; involuntary identity lock
 *   - - hindu_traditionalist_priesthood: Displaced authority (moderate/identity_locked) — lost interpretive jurisdiction, retained ceremonial role
 *   - - interfaith_and_optout_couples: Excluded (moderate/constrained) — routed outside this reading's framework by design
 *   - - womens_movement_scholars: Analytical observer (organized/analytical) — documents asymmetry, holds no operational seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, 0.5).
domain_priors:suppression_score(marriage_authority_kernel__hindu_codified_reading, 0.42).
domain_priors:theater_ratio(marriage_authority_kernel__hindu_codified_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__hindu_codified_reading, "Codified Hindu Marriage Authority (Hindu Marriage Act 1955, Civil-Court Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__hindu_codified_reading, "legal/religious governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__hindu_codified_reading, 'dd3c1569-1e09-48fe-98fa-5d7637528011').
narrative_ontology:cs_kernel_codification('dd3c1569-1e09-48fe-98fa-5d7637528011', fixed_text).
narrative_ontology:cs_authority_grounding('dd3c1569-1e09-48fe-98fa-5d7637528011', lineage).
narrative_ontology:cs_interpretation_layer_present('dd3c1569-1e09-48fe-98fa-5d7637528011').
narrative_ontology:cs_reading_relation('dd3c1569-1e09-48fe-98fa-5d7637528011', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('dd3c1569-1e09-48fe-98fa-5d7637528011', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('dd3c1569-1e09-48fe-98fa-5d7637528011', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('dd3c1569-1e09-48fe-98fa-5d7637528011', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('dd3c1569-1e09-48fe-98fa-5d7637528011', foundational, statute_supersedes_scriptural_adjudication).
narrative_ontology:cs_axiom_status(statute_supersedes_scriptural_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('dd3c1569-1e09-48fe-98fa-5d7637528011', statute_supersedes_scriptural_adjudication, conventional).
narrative_ontology:cs_axiom('dd3c1569-1e09-48fe-98fa-5d7637528011', foundational, communal_scope_of_marriage_regulation).
narrative_ontology:cs_axiom_status(communal_scope_of_marriage_regulation, holdable).
narrative_ontology:cs_axiom_grounding('dd3c1569-1e09-48fe-98fa-5d7637528011', communal_scope_of_marriage_regulation, conventional).
narrative_ontology:cs_axiom('dd3c1569-1e09-48fe-98fa-5d7637528011', secondary, gradualist_gender_reform_within_codified_frame).
narrative_ontology:cs_axiom_status(gradualist_gender_reform_within_codified_frame, holdable).
narrative_ontology:cs_axiom_grounding('dd3c1569-1e09-48fe-98fa-5d7637528011', gradualist_gender_reform_within_codified_frame, instrumental).
narrative_ontology:cs_reference_frame('dd3c1569-1e09-48fe-98fa-5d7637528011', communal_codified_statute).
narrative_ontology:cs_drift_state('dd3c1569-1e09-48fe-98fa-5d7637528011', contemporary_ucc_debate_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dd3c1569-1e09-48fe-98fa-5d7637528011', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_community_households).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, state_civil_courts).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_modernist_reform_establishment).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_women_in_marital_disputes).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, sikh_buddhist_jain_adherents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_traditionalist_priesthood).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_traditionalist_priesthood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the Hindu Marriage Act in 1955 after the wider Hindu Code Bill was scaled back by traditionalist opposition, and retains sole power to amend or replace it. Has liberalized divorce grounds (1976) and left the constitutional directive toward a uniform civil code unexercised for seven decades. Acting requires assembling a coalition that has repeatedly failed to form.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, indian_parliament, agenda_setter,
    institutional, generational, constrained, national).

% Interpret and apply the Act in district courts, High Courts, and the Supreme Court; every doctrinal question — what counts as a valid marriage, when desertion or cruelty is made out, how maintenance is computed — is settled by judicial precedent rather than by priests or community councils. Acquired a near-monopoly on matrimonial adjudication when the Act displaced customary forums. Bound by the statute they interpret: they cannot decline the docket or rewrite the text, only gloss it.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, state_civil_courts, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__hindu_codified_reading, state_civil_courts, beneficiary).

% Marry, register, separate, and inherit under one uniform set of rules regardless of region, caste, or school of customary law, with ceremonies of their choosing recognized as sufficient solemnization. Disputes go to a court rather than to caste elders. Leaving the framework means converting out of the community or marrying under the Special Marriage Act, both of which carry heavy social and familial cost, so the framework is effectively the only one they can use while remaining who they are.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_community_households, beneficiary,
    organized, biographical, identity_locked, national).

% The legal-political current that designed and shepherded the codification — law ministers, drafters, law-commission members and their intellectual heirs. The Act's survival vindicates their program of modernizing personal law through legislation rather than religious internal reform, and their successors staff the commissions and benches that keep interpreting it. They can redirect their energies to uniform-code advocacy at will.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_modernist_reform_establishment, beneficiary,
    institutional, generational, arbitrage, national).

% Contested divorces, maintenance claims, custody fights, and conjugal-rights petitions run through provisions whose burdens have historically fallen unevenly on wives: restitution decrees ordered wives back into broken households for decades, maintenance amounts lag living costs, and custody presumptions long favored fathers. Improvements arrived piecemeal, by amendment and judicial reinterpretation. Exiting a bad marriage is possible but expensive — financially, socially, often physically — and exiting the legal framework itself is not a real option.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_women_in_marital_disputes, payer,
    powerless, biographical, trapped, national).

% Statutorily counted as Hindus for marriage purposes since 1955 without having been asked: the Act's definition sweeps them in, so weddings under the Anand Karaj or Buddhist rites take legal effect through a law named for another community. Organized campaigns for a separate Sikh Marriage Act and for standalone recognition have run for decades without success. Opting out individually would mean disavowing the very classification they object to collectively; there is no exit that keeps their identity intact.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, sikh_buddhist_jain_adherents, payer,
    organized, generational, identity_locked, national).

% Before codification, schools of customary law and community authorities decided what made a marriage valid and dissoluble. The Act moved those questions to the courts, leaving priests the ceremony but not the verdict. Many opposed the original bill precisely over divorce and remarriage; they now operate inside a framework they did not write, and their standing depends on the same community identity the statute defines.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_traditionalist_priesthood, payer,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__hindu_codified_reading, hindu_traditionalist_priesthood, beneficiary).

% Couples where one partner is not a Hindu under the Act's definition cannot use this framework at all; they are routed to the Special Marriage Act, with its public notice period that invites family objection and social exposure. They hold standing views on a system that assigns legal routes by religion — views the framework's own machinery never hears, because they sit outside its jurisdiction by design.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, interfaith_and_optout_couples, excluded,
    moderate, biographical, constrained, national).

% Feminist lawyers, academics, and activists who document the asymmetries, bring test litigation, and draft reform proposals. They hold no seat in the framework's operation: they can persuade courts and Parliament but cannot themselves set or apply the rules. Their analyses are the main external record of how the provisions land on the ground.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, womens_movement_scholars, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__hindu_codified_reading, state_civil_courts).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__hindu_codified_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single codified set of marriage, divorce, maintenance, and custody rules for everyone statutorily defined as Hindu, replacing region-, caste-, and school-specific customary variation, and concentrates adjudication in trained civil courts instead of dispersed community authorities.
% TRANSFER_FUNCTION: Moves adjudicative and definitional authority from customary and religious forums to the state judiciary; moves dispute outcomes (maintenance payments, custody, conjugal-rights compliance) between spouses along lines that have historically weighted wives' obligations and husbands' claims; and moves the power to say who counts as 'Hindu' for marriage purposes from communities to the statute book.
% ABSENT_VOICES: Sikh, Buddhist, and Jain representatives objecting to statutory subsumption petition Parliament from outside it; interfaith couples living under the Special Marriage Act's notice-period burdens have no seat in this framework's operation; traditionalist interpreters whose adjudicative role was displaced were consulted during enactment but hold no standing voice in interpretation; and women experiencing the provisions' uneven burdens encounter the system only as litigants.
% DISAPPEARANCE_RATIONALE: Repealed overnight, millions of registered marriages would lose their governing law mid-life: pending divorces, maintenance orders, and custody arrangements would have no framework, courts would improvise under general law, community and religious authorities would contest reclaimed jurisdiction, and the statutory answer to 'who counts as Hindu for marriage' would vanish — the entire matrimonial-adjudication order would have to be rebuilt.
% FOUNDING_PROBLEM: Post-independence reformers faced fragmented customary marriage law (Mitakshara and Dayabhaga schools, regional and sectarian variants), widespread polygamy, no general right to divorce, and no legal exit for women in failed marriages; the Hindu Code Bill project aimed to unify and modernize these rules through legislation.
% FOUNDING_PROBLEM_CORROBORATION: Law Commission of India papers and parliamentary committee records attest the original fragmentation and no-exit problems; subsequent Law Commission consultations and Supreme Court opinions acknowledge that the gender-equity half of the founding problem remains only partly solved; Sikh community representations to Parliament independently attest the subsumption grievance. Attestation does not rest solely on the Act's beneficiaries.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__hindu_codified_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__hindu_codified_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__hindu_codified_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__hindu_codified_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__hindu_codified_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__hindu_codified_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__hindu_codified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The arrangement earns its coordination credit honestly: one uniform rulebook and professional adjudication replaced hundreds of customary regimes, and that function is performed daily, not ceremonially — hence low theater (0.22, drifting up slowly as anniversary rhetoric and reform pageantry accumulate around a maturing statute). Base extractiveness settles near 0.50: substantial genuine service, substantial residual asymmetric burden. Suppression (0.42) is authored as a raw structural property — it is not scaled by power or scope downstream; it reflects that exits exist (Special Marriage Act, conversion) but are socially and procedurally expensive, and that the subsumed communities' exit runs through the very legislature that has declined to grant it for seventy years. Accessibility collapse is moderate (0.45): once inside the framework, alternatives are costly but not imaginary. Resistance (0.50) is continuous — feminist litigation, minority campaigns, uniform-code politics — without ever becoming effective enough to force structural change. The measurement series share one time grid (decade points 0–70); trajectories are monotonic rather than cyclical, so no oscillation mechanism is modeled. Enforcement capacity (suppression_requirement) rises with family-court buildout through the 1980s then plateaus — the one enforcement-infrastructure story the interval tells.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (Parliament, the courts) the arrangement presents as a functioning, improvable coordination device they steward and periodically refine. From the household seat it presents as roughly fair infrastructure with occasional rough edges. From the target seats it presents very differently: women in disputes meet provisions whose burdens land on them, and subsumed minorities meet a classification imposed without consent. Same statute, same courts — structurally different experiences, which is what the per-seat computation should surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (households, courts, reform establishment) drive those seats toward the beneficiary end; victim declarations (women in disputes, subsumed adherents) drive those seats toward the target end. Exit options modulate within that: households are identity-locked voluntarily (leaving means leaving the community), while the subsumed communities are identity-locked involuntarily (their exit is held by Parliament) — same exit atom, different mechanism, and the difference matters for how much of the burden each seat can shed. Courts combine agenda-setting with benefit-collection, which is why the receipt surface names them. No directionality overrides are needed: the beneficiary/victim declarations plus exit atoms already separate the seats cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem splits: the fragmentation/no-exit half is essentially solved (uniform rules, divorce available), while the gender-equity half remains live and contested. Treating the arrangement as pure coordination would erase the documented asymmetric burdens; treating it as pure extraction would erase the real uniformity and adjudication gains every household receives. The tangled-rope reading keeps both halves visible, and the contested founding-status prevents the solved half from laundering the unsolved half as finished work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the marriage_authority_kernel (hindu_codified_reading). Which structural elements would sibling readings change, and where exactly is the disagreement located?',
    'Cross-reading comparison of the five sibling stories'' victim/beneficiary sets and epsilon values; the disagreement localizes to the source-of-authority element (enacted statute vs scripture vs custom vs individual right) and derivatively to the victim set (who bears each reading''s residual burdens).',
    'If the secular reading were adopted as the sole framework, this reading''s victim set (subsumed minorities, women under asymmetric provisions) would be replaced by the secular reading''s own contested set; classification of THIS constraint is unaffected — the omega records committer structure, not a defect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one reading of a five-reading kernel; sibling deltas recorded, not folded into this constraint.').

omega_variable(
    subsumption_consent_status,
    'Does the statutory subsumption of Sikh, Buddhist, and Jain communities under the Act''s definition of ''Hindu'' operate as imposed classification or as accepted administrative simplification?',
    'Track sustained community-level mobilization for separate marriage statutes (Sikh Marriage Act demands, Anand Karaj recognition campaigns) and legislative responses; persistent organized demand for exit indicates imposition rather than acceptance.',
    'If imposed, the sikh_buddhist_jain_adherents seat computes nearer the full-target end and the constraint''s effective extraction rises; if accepted simplification, that seat''s directionality falls toward symmetric and the overall classification softens toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsumption_consent_status, empirical, 'Whether statutory subsumption of minority communities is extraction or accepted simplification.').

omega_variable(
    gender_equity_metric_choice,
    'Is the reading''s gender-equity position ''better than Muslim, worse than secular'' when measured by formal provisions, litigated outcomes, or exit costs — and does the ranking survive the metric choice?',
    'Parallel audit of the three metrics across sibling readings: statutory-text comparison, case-outcome sampling, and costed exit-path analysis.',
    'Formal-provision measurement flatters this reading relative to the shariat reading; outcome and exit-cost measurement narrows or reverses the gaps, changing the comparative delta the kernel context asserts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equity_metric_choice, conceptual, 'Comparative gender-equity ranking depends on which observable measures it.').

omega_variable(
    ucc_convergence_direction,
    'Does the codified reading converge toward the secular civil reading (Uniform Civil Code adoption) or harden communal boundaries in response to uniform-code pressure?',
    'Legislative trajectory tracking: state-level uniform-code enactments, parliamentary introduction of a national code, or counter-mobilization entrenching personal-law institutions.',
    'Convergence would push this constraint toward transitional-scaffold behavior (absorption into a civil code as its sunset); hardening would raise suppression and entrench the tangled-rope structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ucc_convergence_direction, preference, 'Trajectory of the codified reading relative to the secular sibling under uniform-code politics.').

omega_variable(
    judicial_constitutional_absorption,
    'Are civil courts progressively absorbing constitutional equality norms into their interpretation of the Act (moving the arrangement toward the secular reading''s substance) or entrenching the communal frame?',
    'Longitudinal coding of Supreme Court and High Court matrimonial rulings for equality-norm uptake versus communal-frame reaffirmation.',
    'Absorption lowers measured extractiveness over time (drift toward rope); entrenchment raises it (drift toward a snare-flavored tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_constitutional_absorption, empirical, 'Direction of judicial interpretation: constitutional absorption versus communal entrenchment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__hindu_codified_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(marr_tr_t10, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(marr_tr_t20, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(marr_tr_t30, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(marr_tr_t40, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(marr_tr_t50, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 50, 0.19).
narrative_ontology:measurement(marr_tr_t60, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(marr_tr_t70, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 70, 0.22).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 0, 0.56).
narrative_ontology:measurement(marr_be_t10, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(marr_be_t20, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(marr_be_t30, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(marr_be_t40, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 40, 0.47).
narrative_ontology:measurement(marr_be_t50, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(marr_be_t60, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 60, 0.49).
narrative_ontology:measurement(marr_be_t70, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 70, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(marr_su_t10, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(marr_su_t20, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 20, 0.32).
narrative_ontology:measurement(marr_su_t30, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(marr_su_t40, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(marr_su_t50, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement(marr_su_t60, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 60, 0.41).
narrative_ontology:measurement(marr_su_t70, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 70, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__hindu_codified_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, secular_civil_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Indian marriage law' decomposes into five structurally distinct readings of one kernel (the source of marital-law authority). This file instantiates the hindu_codified_reading only: epsilon is authored for the standing HMA arrangement as this reading holds it, not averaged across readings. Sibling files carry their own epsilon, beneficiary/victim sets, and classifications; the edges here record the family linkage. Direction of influence: the codified reading influences the secular reading (the HMA operates as the de facto template for Uniform Civil Code proposals, shaping what a unified code would look like) while coexisting with the three other communal readings under India's plural personal-law settlement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
