% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__secular_civil_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__secular_civil_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: marriage_authority_kernel__secular_civil_reading
 *   human_readable: Special Marriage Act 1954 — Secular Civil Reading of Marriage Authority
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   The Special Marriage Act 1954 constitutes a secular civil reading of
 *   marriage authority in India: any two adults, regardless of religion, may
 *   marry under civil law grounded in constitutional individual rights rather
 *   than any community's personal law. This is one of five distinct
 *   constitutional readings of the same underlying marriage-authority kernel
 *   that India's legal pluralism sustains simultaneously — Hindu, Muslim,
 *   Christian, and Parsi personal law regimes each ground marriage authority
 *   differently, and the secular civil reading does not replace them but sits
 *   alongside them as an opt-in alternative. This story models the secular
 *   civil reading only; its epsilon is stable and does not average over the
 *   sibling readings, which are separate constraint stories linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - civil_court_system: agenda_setter (institutional/analytical) — administers registration, notice, and dissolution
 *   - inter_religious_couples: beneficiary (moderate/mobile) — gains marriage standing otherwise unavailable
 *   - notice_period_exposed_couples: payer (powerless/trapped) — bears acute exposure risk from the statutory notice mechanism
 *   - couples_facing_community_ostracism: payer (powerless/constrained) — bears diffuse, uncompensated social costs
 *   - personal_law_boards: excluded (organized/constrained) — loses jurisdiction whenever a couple opts in, has no formal standing in proceedings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, 0.38).
domain_priors:suppression_score(marriage_authority_kernel__secular_civil_reading, 0.42).
domain_priors:theater_ratio(marriage_authority_kernel__secular_civil_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__secular_civil_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__secular_civil_reading, "Special Marriage Act 1954 — Secular Civil Reading of Marriage Authority").
narrative_ontology:topic_domain(marriage_authority_kernel__secular_civil_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__secular_civil_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__secular_civil_reading, '62a9edf1-7eb4-4ed7-a736-7c1172f63fd6').
narrative_ontology:cs_kernel_codification('62a9edf1-7eb4-4ed7-a736-7c1172f63fd6', formalized).
narrative_ontology:cs_authority_grounding('62a9edf1-7eb4-4ed7-a736-7c1172f63fd6', expertise).
narrative_ontology:cs_interpretation_layer_present('62a9edf1-7eb4-4ed7-a736-7c1172f63fd6').
narrative_ontology:cs_reading_relation('62a9edf1-7eb4-4ed7-a736-7c1172f63fd6', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('62a9edf1-7eb4-4ed7-a736-7c1172f63fd6', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('62a9edf1-7eb4-4ed7-a736-7c1172f63fd6', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('62a9edf1-7eb4-4ed7-a736-7c1172f63fd6', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_axiom('62a9edf1-7eb4-4ed7-a736-7c1172f63fd6', foundational, individual_constitutional_rights_supersede_community_jurisdiction).
narrative_ontology:cs_axiom_status(individual_constitutional_rights_supersede_community_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('62a9edf1-7eb4-4ed7-a736-7c1172f63fd6', individual_constitutional_rights_supersede_community_jurisdiction, deontological).
narrative_ontology:cs_axiom('62a9edf1-7eb4-4ed7-a736-7c1172f63fd6', foundational, marriage_validity_independent_of_religious_identity).
narrative_ontology:cs_axiom_status(marriage_validity_independent_of_religious_identity, holdable).
narrative_ontology:cs_axiom_grounding('62a9edf1-7eb4-4ed7-a736-7c1172f63fd6', marriage_validity_independent_of_religious_identity, conventional).
narrative_ontology:cs_reference_frame('62a9edf1-7eb4-4ed7-a736-7c1172f63fd6', constitutional_individual_rights_supremacy).
narrative_ontology:cs_drift_state('62a9edf1-7eb4-4ed7-a736-7c1172f63fd6', contemporary_uniform_civil_code_debate, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('62a9edf1-7eb4-4ed7-a736-7c1172f63fd6', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, inter_religious_couples).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, civil_court_system).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, gender_equality_claimants).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, couples_facing_community_ostracism).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, notice_period_exposed_couples).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, constitutional_supremacy_over_personal_law).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, individual_rights_as_marriage_ground).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers registration, the mandatory notice period, objection hearings, and dissolution under the Special Marriage Act. Adjudicates using uniform statutory grounds rather than any single community's doctrine, and its authority is the constitutional claim that individual rights override community jurisdiction over marriage.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, civil_court_system, agenda_setter,
    institutional, civilizational, analytical, national).

% Gain the only legal channel to marry across religious lines without either party converting. The Act gives them standing to marry at all against community law that would otherwise bar or unrecognize the union.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, inter_religious_couples, beneficiary,
    moderate, biographical, mobile, national).

% Women and others seeking marriage, divorce, or inheritance terms more equitable than those available under some personal-law regimes use the Act's uniform, gender-neutral statutory grounds instead of community adjudication.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, gender_equality_claimants, beneficiary,
    moderate, biographical, mobile, national).

% The statutory 30-day public notice requirement posts their names and addresses for objection, exposing them to family surveillance, honor-based violence, and community intervention before the marriage can be solemnized. For couples fleeing family opposition this window is the single greatest practical danger of the civil route.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, notice_period_exposed_couples, payer,
    powerless, immediate, trapped, local).

% Marrying under the Act, even when legally uncomplicated, triggers loss of caste or community standing, disinheritance, or social exile — costs the civil code does not compensate and cannot prevent, since it has no jurisdiction over informal community sanction.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, couples_facing_community_ostracism, payer,
    powerless, biographical, constrained, local).

% Hindu, Muslim, Christian, and Parsi personal law authorities lose jurisdiction whenever a couple opts into the secular Act; they are not party to Special Marriage Act proceedings and have no seat at the civil court's table, though their communities' informal sanctions are what makes the civil option costly to use.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, personal_law_boards, excluded,
    organized, generational, constrained, national).

% Higher courts periodically review the notice-period provision and the Act's interaction with personal law, weighing individual rights against religious freedom claims and community autonomy arguments raised by personal law boards.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__secular_civil_reading, civil_court_system).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__secular_civil_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, religion-neutral legal channel through which any two adults can marry regardless of faith, resolving the coordination problem that no single community's personal law can validly marry parties across religious lines.
% TRANSFER_FUNCTION: Moves adjudicative authority over marriage and divorce from community religious bodies to civil courts for any couple who opts in; moves exposure risk from the couple's private choice onto a public notice mechanism that shifts information to families and communities capable of intervening.
% ABSENT_VOICES: Personal law boards have no formal role in Special Marriage Act proceedings and cannot object through the statute itself — their influence operates entirely through informal community sanction outside the courtroom, which the civil code does not register or mitigate.
% DISAPPEARANCE_RATIONALE: If the Act vanished, inter-religious and civilly-oriented couples would have no marriage route that does not require one party's conversion or submission to a single community's personal law; the entire population of couples currently relying on it would be forced back into community jurisdiction or into marrying without legal recognition.
% FOUNDING_PROBLEM: Pre-1954, no legal mechanism existed for consenting adults to marry across religious communities without conversion, and individual rights claims (against dowry-linked or unequal personal-law terms) had no secular forum; the Act was built to give constitutional individual rights a marriage-law expression independent of religious authority.
% FOUNDING_PROBLEM_CORROBORATION: Attested outside the Act's direct beneficiaries by Law Commission of India reports repeatedly documenting the ongoing gap for inter-faith couples, and by human rights researchers documenting the notice-period's continuing exposure risk — corroboration exists but is contested by personal law boards, who characterize the same founding problem as a pretext for state incursion on religious jurisdiction.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__secular_civil_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__secular_civil_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__secular_civil_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__secular_civil_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__secular_civil_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__secular_civil_reading_tests).
:- end_tests(marriage_authority_kernel__secular_civil_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.38) and rising slowly: the Act itself extracts little directly, but the notice-period mechanism has, over decades, become an increasingly documented vector for family surveillance and honor-based intervention as its use has grown among couples fleeing opposition — an accumulating cost layered onto the coordination function. Suppression is authored as declining slightly (0.50 to 0.42) reflecting incremental judicial and administrative reforms narrowing notice-period exposure, though it remains substantial because the mechanism's core structure (public notice, objection window) is statutorily unchanged. Theater ratio rises modestly as procedural formality accumulates without proportional protective function. Accessibility collapse is low-moderate (0.35): community personal-law alternatives remain fully available to those who do not need the civil route, so alternatives are not suppressed, only costly to combine.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil courts and inter-religious/equality-seeking couples are structural beneficiaries: the Act exists to give them a channel personal law forecloses. Couples exposed during the notice period and those facing community ostracism are targets of the same statute's operating mechanism — the very transparency that lets the state verify no prior marriage exists is what exposes them to family intervention. Personal law boards are neither beneficiary nor victim in the statute's own terms but are structurally excluded, losing jurisdiction whenever the option is exercised, which is why their objections operate outside the courtroom via community sanction rather than through the Act's own process.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (no secular marriage forum for inter-faith couples or individual-rights claimants) remains live and is corroborated by continuing use rates and Law Commission documentation, which forecloses a pure mandatrophy reading — this is not a scaffold whose sunset has passed. But the notice-period mechanism, originally a fraud-prevention safeguard, has drifted toward functioning as an involuntary disclosure mechanism whose costs fall disproportionately on the couples least able to bear community backlash — the tangled_rope classification captures a genuine, still-live coordination function (secular marriage access) running through the same structure as an asymmetric cost (notice-period exposure) that active enforcement (mandatory posting, statutory timeline) sustains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    notice_period_safeguard_or_exposure_mechanism,
    'Is the 30-day public notice requirement a necessary fraud-prevention safeguard, or has it become primarily an exposure mechanism that enables family and community intervention against the couple''s wishes?',
    'Comparative study of marriage-fraud rates under notice-based versus non-notice civil marriage regimes, cross-referenced with documented instances of family intervention, harassment, or violence occurring specifically during the notice window.',
    'If the safeguard function is negligible relative to the exposure harm, the notice period reclassifies from coordination cost toward pure extraction riding on the coordination function, pushing the constraint toward snare at the payer seats; if the safeguard function is substantial, the tangled_rope reading holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notice_period_safeguard_or_exposure_mechanism, empirical, 'Whether the notice period is genuine safeguard or accumulated extraction mechanism.').

omega_variable(
    secular_authority_as_naturalized_or_contested_kernel,
    'Is constitutional individual-rights authority over marriage a settled constitutional fact, or is it one contested reading among several that India''s legal system has not actually resolved in favor of any single authority?',
    'Track Supreme Court jurisprudence on personal law versus constitutional supremacy claims (e.g., uniform civil code litigation) for signs of convergence toward a single authoritative reading versus continued coexistence of parallel regimes.',
    'If courts move toward treating the secular civil reading as constitutionally mandatory and displacing personal law, this reading''s relationship to its siblings shifts from coexists_with toward forecloses; if coexistence persists indefinitely, the current reading_relations remain accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secular_authority_as_naturalized_or_contested_kernel, conceptual, 'Whether the kernel contest among marriage-authority readings is stable or trending toward resolution.').

omega_variable(
    informal_community_sanction_attribution,
    'Should the social costs borne by couples facing community ostracism be attributed to the Special Marriage Act''s structure, or to the personal law communities'' independent sanctioning power, which the Act neither creates nor can reach?',
    'Legal and sociological analysis of whether the Act''s opt-in visibility (registration records, notice postings) is a necessary precondition for community sanction, or whether communities would sanction inter-religious unions regardless of legal channel chosen.',
    'If the Act''s own visibility mechanisms are a necessary precondition for the sanction, the victim attribution to this constraint is well-grounded; if community sanction is independent of the legal channel used, the victim classification should attach more to the personal law regimes'' informal enforcement than to this constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_community_sanction_attribution, conceptual, 'Whether community-ostracism costs are properly attributable to this constraint or to the excluded personal law regimes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__secular_civil_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1954, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(marr_tr_t1968, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1968, 0.12).
narrative_ontology:measurement(marr_tr_t1985, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1985, 0.14).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2000, 0.16).
narrative_ontology:measurement(marr_tr_t2014, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2014, 0.19).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(marr_be_t1954, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1954, 0.22).
narrative_ontology:measurement(marr_be_t1968, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1968, 0.26).
narrative_ontology:measurement(marr_be_t1985, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1985, 0.3).
narrative_ontology:measurement(marr_be_t2000, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2000, 0.33).
narrative_ontology:measurement(marr_be_t2014, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2014, 0.36).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1954, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1954, 0.5).
narrative_ontology:measurement(marr_su_t1968, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1968, 0.48).
narrative_ontology:measurement(marr_su_t1985, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1985, 0.46).
narrative_ontology:measurement(marr_su_t2000, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2000, 0.44).
narrative_ontology:measurement(marr_su_t2014, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2014, 0.43).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__secular_civil_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__secular_civil_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__parsi_communal_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the marriage_authority_kernel, each a separate constraint story with its own extractiveness, beneficiary/victim structure, and classification. The secular civil reading is the only one available to inter-religious couples and functions as a structural release valve for the personal-law regimes: its existence lets each personal-law reading maintain internal doctrinal purity (no requirement to accommodate inter-faith unions) precisely because dissenting couples can exit to the civil track instead of forcing reform from within. This creates an asymmetric relationship — the secular reading absorbs the exit pressure that would otherwise fall on the community regimes, which is why its stakeholder surface includes couples paying real social costs that the personal-law regimes never have to internalize.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
